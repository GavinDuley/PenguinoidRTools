# Some R AoV summary functions
# https://github.com/GavinDuley/PenguinoidRTools
# Copyright (c) 2024 onwards, Gavin Duley
# Licenced under the GPL-3.0 licence

usethis::use_github_action("check-standard")

# aovSummaryTable -------------------------------------------------------------

#' AoV summary table
#'
#' @name aovSummaryTable
#' @description This function generates a table of means and key statistical
#'  values from an ANOVA for each numeric variable in a data frame. It also
#'  appends rows for the Benjamini–Hochberg (BH) corrected p-values and significance.
#'  The output includes a “Type” column so that the row labels are exported to Excel.
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
  # Get the factor levels for the grouping variable and define base row labels.
  group_levels <- levels(as.factor(aov_data[[group_var]]))
  base_rows <- c(group_levels, "P-value", "F-value", "Significant")
  
  # Create the initial summary table with a "Type" column holding the row labels.
  summary_table <- data.frame(Type = base_rows, stringsAsFactors = FALSE)
  
  # Vector to store raw p-values for BH correction.
  pvalues_vec <- c()
  
  if (return_raw) {
    raw_outputs <- list()
  }
  
  # Skip factor columns.
  factor_indices <- which(sapply(aov_data, is.factor))
  
  # Loop over each column.
  for (i in 1:ncol(aov_data)) {
    if (i %in% factor_indices) next
    if (!is.numeric(aov_data[[i]])) {
      stop(paste("Error: Column", colnames(aov_data)[i], "is not numeric or factor."))
    }
    columnname <- colnames(aov_data)[i]
    
    if (length(unique(aov_data[[i]])) == 1) {
      # For invariant columns, fill with "invariant".
      summary_table[[columnname]] <- rep("invariant", length(base_rows))
    } else {
      # Run ANOVA and Tukey HSD.
      t.anova <- aov(as.formula(paste(columnname, "~", group_var)), data = aov_data)
      test2 <- agricolae::HSD.test(t.anova, trt = group_var, group = TRUE)
      
      if (return_raw) {
        raw_outputs[[columnname]] <- list(aov = t.anova, tukey = test2)
      }
      
      # Build group summaries matching the factor levels.
      group_summaries <- rep("Not available", length(group_levels))
      names(group_summaries) <- group_levels
      for (grp in rownames(test2$means)) {
        if (grp %in% group_levels) {
          group_summaries[grp] <- paste0(
            signif(test2$means[grp, columnname], digits = 4),
            " ", test2$groups[grp, "groups"]
          )
        }
      }
      
      # Extract the raw p-value and F-value.
      raw_p <- summary(t.anova)[[1]][1, 5]
      p_value_str <- paste0(signif(raw_p, digits = 4), " ", stars.pval(raw_p))
      f_value <- signif(summary(t.anova)[[1]][1, 4], digits = 4)
      significance <- if (raw_p <= 0.05) "SIGNIFICANT" else "NOT SIGNIFICANT"
      
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
  }
  
  # Compute BH-corrected p-values if any p-values were collected.
  if (length(pvalues_vec) > 0) {
    bh_p_values <- p.adjust(pvalues_vec, method = "BH")
    # Build two vectors for the new rows.
    bh_corr_row <- c("BH-Corrected-P-value")
    bh_sig_row <- c("BH-Significant")
    # For each variable column (skip the "Type" column).
    for (col in setdiff(colnames(summary_table), "Type")) {
      if (col %in% names(bh_p_values)) {
        bh_val <- bh_p_values[col]
        bh_corr_row <- c(bh_corr_row, paste0(signif(bh_val, digits = 4), " ", stars.pval(bh_val)))
        bh_sig_row <- c(bh_sig_row, ifelse(bh_val <= 0.05, "SIGNIFICANT", "NOT SIGNIFICANT"))
      } else {
        bh_corr_row <- c(bh_corr_row, "invariant")
        bh_sig_row <- c(bh_sig_row, "invariant")
      }
    }
    # Append these new rows to the summary table.
    summary_table <- rbind(summary_table,
                           setNames(as.list(bh_corr_row), colnames(summary_table)),
                           setNames(as.list(bh_sig_row), colnames(summary_table)))
  } else {
    # If no p-values were collected, add rows with NAs.
    new_row <- rep(NA, ncol(summary_table))
    summary_table <- rbind(summary_table,
                           setNames(new_row, colnames(summary_table)),
                           setNames(new_row, colnames(summary_table)))
    summary_table[nrow(summary_table)-1, "Type"] <- "BH-Corrected-P-value"
    summary_table[nrow(summary_table), "Type"] <- "BH-Significant"
  }
  
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
#'  A "Type" column is added so that the row labels export correctly to Excel.
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
  
  if (return_raw) {
    raw_outputs <- list()
  }
  
  summary_list <- list()
  row_names_list <- list()
  pvalues_vec <- c()
  
  # First pass: collect all row names from group summaries
  for (col_name in colnames(aov_data)) {
    if (!is.numeric(aov_data[[col_name]]) || is.factor(aov_data[[col_name]])) next
    
    formula <- as.formula(paste(col_name, "~", paste(group_vars, collapse = "*")))
    t.anova <- aov(formula, data = aov_data)
    test2 <- agricolae::HSD.test(t.anova, trt = group_vars, group = TRUE)
    
    row_names_list[[col_name]] <- rownames(test2$groups)
  }
  
  row_names <- unique(unlist(row_names_list))

  # Determine all effect names from the first ANOVA
  effect_names <- c()
  if (length(row_names_list) > 0) {
    first_col <- names(row_names_list)[1]
    formula <- as.formula(paste(first_col, "~", paste(group_vars, collapse = "*")))
    temp_aov <- aov(formula, data = aov_data)
    aov_summary <- summary(temp_aov)[[1]]
    # Get effect names (excluding Residuals)
    effect_names <- rownames(aov_summary)[rownames(aov_summary) != "Residuals"]
  }

  # Create row labels with separate rows for each effect's statistics
  stat_rows <- c()
  for (effect in effect_names) {
    stat_rows <- c(stat_rows,
                   paste0("P-value-", effect),
                   paste0("F-value-", effect),
                   paste0("Significant-", effect))
  }
  row_labels <- c(row_names, stat_rows)
  
  # Second pass: build summary for each variable
  for (col_name in names(row_names_list)) {
    formula <- as.formula(paste(col_name, "~", paste(group_vars, collapse = "*")))
    t.anova <- aov(formula, data = aov_data)
    test2 <- agricolae::HSD.test(t.anova, trt = group_vars, group = TRUE)
    
    group_summaries <- rep("Not available", length(row_names))
    names(group_summaries) <- row_names
    for (grp in rownames(test2$means)) {
      if (grp %in% row_names) {
        mean_val <- signif(test2$means[grp, col_name], digits = 4)
        group_summaries[grp] <- paste0(mean_val, " ", test2$groups[grp, "groups"])
      }
    }

    # Extract ALL effects' p-values and F-values (not just the first row)
    aov_summary <- summary(t.anova)[[1]]
    effect_stats <- c()
    for (effect in effect_names) {
      if (effect %in% rownames(aov_summary)) {
        p_val <- aov_summary[effect, "Pr(>F)"]
        f_val <- aov_summary[effect, "F value"]

        effect_stats <- c(effect_stats,
                          paste0(signif(p_val, digits = 4), " ", stars.pval(p_val)),
                          signif(f_val, digits = 4),
                          ifelse(p_val <= 0.05, "SIGNIFICANT", "NOT SIGNIFICANT"))

        # Store p-value for BH correction with unique name
        pvalues_vec[paste0(col_name, ":", effect)] <- p_val
      } else {
        # If effect not found, add NA values
        effect_stats <- c(effect_stats, NA, NA, NA)
      }
    }

    group_summaries <- c(group_summaries, effect_stats)
    summary_list[[col_name]] <- group_summaries
    
    if (return_raw) {
      raw_outputs[[col_name]] <- list(aov = t.anova, tukey = test2)
    }
  }
  
  # Create the summary table
  summary_table <- as.data.frame(summary_list, stringsAsFactors = FALSE)
  summary_table <- cbind(Type = row_labels, summary_table)
  
  # Compute BH-corrected p-values (correcting ALL p-values together)
  if (length(pvalues_vec) > 0) {
    bh_p_values <- p.adjust(pvalues_vec, method = "BH")

    # Create separate rows for each effect's BH-corrected statistics
    bh_rows <- list()
    for (effect in effect_names) {
      bh_corr_row <- c(paste0("BH-Corrected-P-value-", effect))
      bh_sig_row <- c(paste0("BH-Significant-", effect))

      for (col in setdiff(colnames(summary_table), "Type")) {
        pval_name <- paste0(col, ":", effect)
        if (pval_name %in% names(bh_p_values)) {
          bh_val <- bh_p_values[pval_name]
          bh_corr_row <- c(bh_corr_row, paste0(signif(bh_val, digits = 4), " ", stars.pval(bh_val)))
          bh_sig_row <- c(bh_sig_row, ifelse(bh_val <= 0.05, "SIGNIFICANT", "NOT SIGNIFICANT"))
        } else {
          bh_corr_row <- c(bh_corr_row, "N/A")
          bh_sig_row <- c(bh_sig_row, "N/A")
        }
      }

      bh_rows[[length(bh_rows) + 1]] <- setNames(as.list(bh_corr_row), colnames(summary_table))
      bh_rows[[length(bh_rows) + 1]] <- setNames(as.list(bh_sig_row), colnames(summary_table))
    }

    # Append all BH rows to the summary table
    for (row in bh_rows) {
      summary_table <- rbind(summary_table, row)
    }
  } else {
    # If no p-values, add placeholder rows
    for (effect in effect_names) {
      new_row <- rep(NA, ncol(summary_table))
      summary_table <- rbind(summary_table, setNames(new_row, colnames(summary_table)))
      summary_table[nrow(summary_table), "Type"] <- paste0("BH-Corrected-P-value-", effect)
      summary_table <- rbind(summary_table, setNames(new_row, colnames(summary_table)))
      summary_table[nrow(summary_table), "Type"] <- paste0("BH-Significant-", effect)
    }
  }
  
  if (!is.null(output_file)) {
    openxlsx::write.xlsx(summary_table, output_file, rowNames = FALSE)
  }
  
  if (return_raw) {
    assign(output_name, raw_outputs, envir = .GlobalEnv)
  }
  
  return(summary_table)
}