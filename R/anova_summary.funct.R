# Some R AoV summary functions
# https://github.com/GavinDuley/PenguinoidRTools
# Copyright (c) 2024 onwards, Gavin Duley
# Licenced under the GPL-3.0 licence

# aovSummaryTable -------------------------------------------------------------

#' AoV summary table
#'
#' @name aovSummaryTable
#' @description This function generates a table of means and key statistical
#'  values from an ANOVA for each numeric variable in a data frame. Non-numeric
#'  columns (other than the grouping variable) are ignored. It also
#'  appends rows for the Benjamini-Hochberg (BH) corrected p-values and significance.
#'  The output includes a "Type" column so that the row labels are exported to Excel.
#'
#' @param aov_data The data frame containing the data.
#' @param group_var The name of the column containing the grouping variable.
#' @param output_file Optional file name (with .xlsx extension) to save the table.
#' @param return_raw Logical; if TRUE, the raw outputs are assigned to a global variable.
#' @param output_name The name of the global variable to store raw outputs.
#' @return A data frame summary table.
#' @importFrom stats aov as.formula p.adjust
#' @importFrom agricolae HSD.test
#' @importFrom openxlsx write.xlsx
#' @import dplyr
#' @importFrom gtools stars.pval
#' @export
aovSummaryTable <- function(aov_data, 
                            group_var, 
                            output_file = NULL,
                            return_raw = FALSE,
                            output_name = "aov_results") {
  if (!group_var %in% colnames(aov_data)) {
    stop("Error: group_var '", group_var, "' is not present in aov_data.")
  }
  aov_data[[group_var]] <- as.factor(aov_data[[group_var]])

  # Get the factor levels for the grouping variable and define base row labels.
  group_levels <- levels(aov_data[[group_var]])
  base_rows <- c(group_levels, "P-value", "F-value", "Significant")

  # Create the initial summary table with a "Type" column holding the row labels.
  summary_table <- data.frame(Type = base_rows, stringsAsFactors = FALSE)

  # Vector to store raw p-values for BH correction.
  pvalues_vec <- numeric(0)
  raw_outputs <- list()

  # Helper: TRUE if the column is effectively invariant for ANOVA purposes --
  # either the response is constant, or group_var has <2 levels after
  # removing NAs for that column (which would cause the "contrasts" error).
  col_is_invariant <- function(col_name) {
    col_vals <- aov_data[[col_name]]
    if (length(unique(col_vals)) <= 1) return(TRUE)
    non_na_groups <- aov_data[[group_var]][!is.na(col_vals)]
    length(unique(non_na_groups)) < 2
  }

  # Analyse each numeric column; everything else (factors, characters, the
  # grouping variable itself) is ignored.
  numeric_cols <- colnames(aov_data)[sapply(aov_data, is.numeric)]

  for (columnname in numeric_cols) {
    if (col_is_invariant(columnname)) {
      summary_table[[columnname]] <- rep("invariant", length(base_rows))
      next
    }

    # Run ANOVA and Tukey HSD. Backticks allow non-syntactic column names.
    formula <- as.formula(paste0("`", columnname, "` ~ `", group_var, "`"))
    t.anova <- aov(formula, data = aov_data)
    test2 <- agricolae::HSD.test(t.anova, trt = group_var, group = TRUE)

    raw_outputs[[columnname]] <- list(aov = t.anova, tukey = test2)

    # Build group summaries matching the factor levels.
    group_summaries <- rep("Not available", length(group_levels))
    names(group_summaries) <- group_levels
    for (grp in intersect(rownames(test2$means), group_levels)) {
      group_summaries[grp] <- paste0(
        signif(test2$means[grp, columnname], digits = 4),
        " ", test2$groups[grp, "groups"]
      )
    }

    # Extract the p-value and F-value by name rather than position. The
    # group effect is the single non-Residuals row of the ANOVA summary.
    aov_summary <- summary(t.anova)[[1]]
    rownames(aov_summary) <- trimws(rownames(aov_summary))
    effect_row <- setdiff(rownames(aov_summary), "Residuals")[1]
    raw_p <- aov_summary[effect_row, "Pr(>F)"]
    f_value <- signif(aov_summary[effect_row, "F value"], digits = 4)
    p_value_str <- paste0(signif(raw_p, digits = 4), " ", stars.pval(raw_p))
    significance <- ifelse(raw_p <= 0.05, "SIGNIFICANT", "NOT SIGNIFICANT")

    # Store the raw p-value.
    pvalues_vec[columnname] <- raw_p

    # Append values (group summaries then overall statistics) as a new column.
    summary_table[[columnname]] <- c(
      group_summaries,
      p_value_str,
      f_value,
      significance
    )
  }

  # Compute BH-corrected p-values across all analysed variables and append
  # them as two new rows. Invariant columns are labelled as such.
  bh_p_values <- p.adjust(pvalues_vec, method = "BH")
  bh_rows <- data.frame(Type = c("BH-Corrected-P-value", "BH-Significant"),
                        stringsAsFactors = FALSE)
  for (col in setdiff(colnames(summary_table), "Type")) {
    if (col %in% names(bh_p_values)) {
      bh_val <- bh_p_values[col]
      bh_rows[[col]] <- c(
        paste0(signif(bh_val, digits = 4), " ", stars.pval(bh_val)),
        ifelse(bh_val <= 0.05, "SIGNIFICANT", "NOT SIGNIFICANT")
      )
    } else {
      bh_rows[[col]] <- c("invariant", "invariant")
    }
  }
  summary_table <- rbind(summary_table, bh_rows)

  # Write the table to Excel (the "Type" column is now included).
  if (!is.null(output_file)) {
    openxlsx::write.xlsx(summary_table, output_file, rowNames = FALSE)
  }
  
  if (return_raw) {
    assign(output_name, raw_outputs, envir = .GlobalEnv)
  }
  
  return(summary_table)
}

# aovInteractSummaryTable -----------------------------------------------------

#' AoV summary table with interactions
#'
#' @name aovInteractSummaryTable
#' @description This function generates a table of means and key statistical
#'  values from an ANOVA that includes interactions among multiple grouping variables.
#'  It also appends BH-corrected p-values and significance as additional rows.
#'  The BH correction is applied separately within each effect family (i.e. all
#'  p-values for a given main effect or interaction are corrected together,
#'  across variables). A "Type" column is added so that the row labels export
#'  correctly to Excel.
#'
#' @param aov_data The data frame containing the data.
#' @param group_vars A vector of column names containing the grouping variables.
#' @param output_file Optional file name (with .xlsx extension) to save the table.
#' @param return_raw Logical; if TRUE, raw outputs are assigned to a global variable.
#' @param output_name The name of the global variable to store raw outputs.
#' @return A data frame summary table.
#' @importFrom stats aov as.formula p.adjust
#' @importFrom agricolae HSD.test
#' @importFrom openxlsx write.xlsx
#' @import dplyr
#' @importFrom gtools stars.pval
#' @export
aovInteractSummaryTable <- function(aov_data, 
                                    group_vars, 
                                    output_file = NULL,
                                    return_raw = FALSE,
                                    output_name = "aov_results") {
  if (!all(group_vars %in% colnames(aov_data))) {
    stop("Error: One or more group_vars are not present in aov_data.")
  }
  for (gv in group_vars) {
    aov_data[[gv]] <- as.factor(aov_data[[gv]])
  }

  # Helper: TRUE if the column is effectively invariant for ANOVA purposes --
  # either the response is constant, or any group_var has <2 levels after
  # removing NAs for that column (which would cause the "contrasts" error).
  col_is_invariant <- function(col_name) {
    col_vals <- aov_data[[col_name]]
    if (length(unique(col_vals)) <= 1) return(TRUE)
    non_na_data <- aov_data[!is.na(col_vals), ]
    any(vapply(group_vars,
               function(gv) length(unique(non_na_data[[gv]])) < 2,
               logical(1)))
  }

  # Analyse each numeric column; everything else (factors, characters, the
  # grouping variables themselves) is ignored.
  numeric_cols <- colnames(aov_data)[sapply(aov_data, is.numeric)]
  invariant_cols <- Filter(col_is_invariant, numeric_cols)
  model_cols <- setdiff(numeric_cols, invariant_cols)

  # Fit each model once, caching the ANOVA and Tukey HSD results.
  # Backticks allow non-syntactic column names.
  rhs <- paste0("`", group_vars, "`", collapse = " * ")
  models <- lapply(model_cols, function(col_name) {
    formula <- as.formula(paste0("`", col_name, "` ~ ", rhs))
    t.anova <- aov(formula, data = aov_data)
    test2 <- agricolae::HSD.test(t.anova, trt = group_vars, group = TRUE)
    list(aov = t.anova, tukey = test2)
  })
  names(models) <- model_cols
  raw_outputs <- models

  # Collect all treatment-combination row names across variables.
  row_names <- unique(unlist(lapply(models, function(m) rownames(m$tukey$groups))))

  # Determine all effect names from the first ANOVA (excluding Residuals).
  # Note: rownames from summary(aov()) have trailing whitespace, so we must
  # trim before comparing.
  effect_names <- c()
  if (length(models) > 0) {
    first_summary <- summary(models[[1]]$aov)[[1]]
    effect_names <- setdiff(trimws(rownames(first_summary)), "Residuals")
  }

  # Create row labels with separate rows for each effect's statistics
  stat_rows <- unlist(lapply(effect_names, function(effect) {
    paste0(c("P-value-", "F-value-", "Significant-"), effect)
  }))
  row_labels <- c(row_names, stat_rows)

  # Build the summary column for each variable. P-values are collected per
  # effect so that BH correction can be applied within each effect family.
  summary_list <- list()
  pvalues_by_effect <- list()

  for (col_name in model_cols) {
    t.anova <- models[[col_name]]$aov
    test2 <- models[[col_name]]$tukey

    group_summaries <- rep("Not available", length(row_names))
    names(group_summaries) <- row_names
    for (grp in intersect(rownames(test2$means), row_names)) {
      mean_val <- signif(test2$means[grp, col_name], digits = 4)
      group_summaries[grp] <- paste0(mean_val, " ", test2$groups[grp, "groups"])
    }

    # Extract ALL effects' p-values and F-values by name (not by position)
    aov_summary <- summary(t.anova)[[1]]
    rownames(aov_summary) <- trimws(rownames(aov_summary))
    effect_stats <- c()
    for (effect in effect_names) {
      if (effect %in% rownames(aov_summary)) {
        p_val <- aov_summary[effect, "Pr(>F)"]
        f_val <- aov_summary[effect, "F value"]

        effect_stats <- c(effect_stats,
                          paste0(signif(p_val, digits = 4), " ", stars.pval(p_val)),
                          signif(f_val, digits = 4),
                          ifelse(p_val <= 0.05, "SIGNIFICANT", "NOT SIGNIFICANT"))

        pvalues_by_effect[[effect]][col_name] <- p_val
      } else {
        # If effect not found, add NA values
        effect_stats <- c(effect_stats, NA, NA, NA)
      }
    }

    summary_list[[col_name]] <- c(group_summaries, effect_stats)
  }

  # Populate invariant columns with "INVARIANT" for every row
  for (col_name in invariant_cols) {
    summary_list[[col_name]] <- rep("INVARIANT", length(row_labels))
  }

  # Restore original column order (non-invariant + invariant may be out of order)
  summary_list <- summary_list[intersect(numeric_cols, names(summary_list))]

  # Create the summary table
  summary_table <- as.data.frame(summary_list, stringsAsFactors = FALSE,
                                 check.names = FALSE)
  summary_table <- cbind(Type = row_labels, summary_table)

  # Append BH-corrected p-values and significance, correcting within each
  # effect family (all variables' p-values for one effect at a time).
  variable_cols <- setdiff(colnames(summary_table), "Type")
  for (effect in effect_names) {
    bh_p_values <- p.adjust(pvalues_by_effect[[effect]], method = "BH")

    bh_rows <- data.frame(Type = paste0(c("BH-Corrected-P-value-", "BH-Significant-"), effect),
                          stringsAsFactors = FALSE)
    for (col in variable_cols) {
      if (col %in% names(bh_p_values)) {
        bh_val <- bh_p_values[col]
        bh_rows[[col]] <- c(
          paste0(signif(bh_val, digits = 4), " ", stars.pval(bh_val)),
          ifelse(bh_val <= 0.05, "SIGNIFICANT", "NOT SIGNIFICANT")
        )
      } else {
        bh_rows[[col]] <- c("INVARIANT", "INVARIANT")
      }
    }
    summary_table <- rbind(summary_table, bh_rows)
  }

  if (!is.null(output_file)) {
    openxlsx::write.xlsx(summary_table, output_file, rowNames = FALSE)
  }
  
  if (return_raw) {
    assign(output_name, raw_outputs, envir = .GlobalEnv)
  }
  
  return(summary_table)

}
