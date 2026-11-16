# Some R AoV summary functions
# https://github.com/GavinDuley/PenguinoidRTools
# Copyright (c) 2024 onwards, Gavin Duley
# Licenced under the GPL-3.0 licence

# Internal helper: warn about columns that were skipped so the reason is
# visible in the console as well as in the summary table. skip_labels is a
# named character vector of column name -> skip label.
warn_skipped_columns <- function(skip_labels) {
  if (length(skip_labels) == 0) return(invisible(NULL))
  warning(
    "The following columns could not be analysed and were filled with a ",
    "label instead: ",
    paste0(names(skip_labels), " (", skip_labels, ")", collapse = ", "),
    ". They may be invariant or contain too many missing values - please ",
    "check them, and consider removing them from the dataset prior to ",
    "analysis.",
    call. = FALSE
  )
  invisible(NULL)
}

# Internal helper: the summary tables use a "Type" column for the row labels,
# so a response variable with that name would silently overwrite them.
stop_if_type_column <- function(numeric_cols) {
  if ("Type" %in% numeric_cols) {
    stop("aov_data has a numeric column named 'Type', which clashes with the ",
         "row-label column of the summary table. Please rename it before ",
         "analysis.", call. = FALSE)
  }
  invisible(NULL)
}

# Internal helper: Type II ANOVA table for a fitted factorial aov() model.
# Each term is tested after all other terms that do not contain it (so a main
# effect is adjusted for the other main effects but not for interactions it
# is part of), against the residual mean square of the full model. The
# result therefore does not depend on the order of the terms. In a balanced
# design it equals the sequential (Type I) table from summary(aov()).
# Returns a data frame with one row per term (row names are the term labels)
# and columns "Sum Sq", "Df", "F value" and "Pr(>F)".
type2_anova <- function(fit) {
  mf <- stats::model.frame(fit)
  tt <- stats::terms(fit)
  labels <- attr(tt, "term.labels")
  # variables x terms incidence matrix, without the response row
  fac <- attr(tt, "factors")[-1, , drop = FALSE]
  resp <- as.name(names(mf)[1])

  rss <- function(lbls) {
    f <- stats::reformulate(if (length(lbls) > 0) lbls else "1", response = resp)
    m <- stats::lm(f, data = mf)
    c(ss = stats::deviance(m), df = stats::df.residual(m))
  }
  full <- c(ss = stats::deviance(fit), df = stats::df.residual(fit))

  rows <- lapply(seq_along(labels), function(i) {
    vars_i <- fac[, i] > 0
    # terms that contain term i (including itself) are left out of the base
    contains_i <- vapply(seq_along(labels),
                         function(j) all(fac[vars_i, j] > 0), logical(1))
    base <- labels[!contains_i]
    r0 <- rss(base)
    r1 <- rss(c(base, labels[i]))
    ss <- unname(r0["ss"] - r1["ss"])
    df <- unname(r0["df"] - r1["df"])
    if (df > 0 && full[["df"]] > 0) {
      f_val <- (ss / df) / (full[["ss"]] / full[["df"]])
      p_val <- stats::pf(f_val, df, full[["df"]], lower.tail = FALSE)
    } else {
      f_val <- NA_real_
      p_val <- NA_real_
    }
    data.frame(`Sum Sq` = ss, Df = df, `F value` = f_val, `Pr(>F)` = p_val,
               check.names = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- labels
  out
}

# aovSummaryTable -------------------------------------------------------------

#' AoV summary table
#'
#' @name aovSummaryTable
#' @description This function generates a table of means and key statistical
#'  values from an ANOVA for each numeric variable in a data frame. Non-numeric
#'  columns (other than the grouping variable) are ignored. Missing values are
#'  allowed: observations with an NA in a given variable are dropped for that
#'  variable's ANOVA only, and group means are computed over the observed
#'  values. Columns that cannot be analysed are filled with a label instead,
#'  and a warning lists them: "invariant" (constant, all NA, or fewer than
#'  two groups observed), "insufficient data" (fewer than two residual
#'  degrees of freedom left after removing NAs, the minimum Tukey's test can
#'  handle), or "not analysable" (the model or Tukey test failed for another
#'  reason). It also
#'  appends rows for the Benjamini–Hochberg (BH) corrected p-values and significance.
#'  The output includes a “Type” column so that the row labels are exported to Excel.
#'
#' @param aov_data The data frame containing the data.
#' @param group_var The name of the column containing the grouping variable.
#' @param output_file Optional file name (with .xlsx extension) to save the table.
#' @param return_raw Logical; if TRUE, the raw outputs are assigned to a global variable.
#' @param output_name The name of the global variable to store raw outputs.
#' @return A data frame summary table.
#' @importFrom stats aov as.formula na.omit p.adjust
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

  # Helper: NA if the column can be analysed, otherwise the label to fill its
  # rows with. "invariant": the response is constant or all NA (NAs don't
  # count as a distinct value), or group_var has <2 levels after removing NAs
  # for that column (which would cause the "contrasts" error).
  # "insufficient data": fewer than two residual degrees of freedom remain
  # after removing NAs. Two is the minimum HSD.test can handle: R's
  # qtukey/ptukey return NaN for df < 2, which crashes HSD.test.
  col_skip_label <- function(col_name) {
    col_vals <- aov_data[[col_name]]
    if (length(unique(col_vals[!is.na(col_vals)])) <= 1) return("invariant")
    # Observations with a missing group are dropped by aov(), so they must
    # not count as a group or towards the residual degrees of freedom.
    non_na_groups <- aov_data[[group_var]][!is.na(col_vals)]
    non_na_groups <- non_na_groups[!is.na(non_na_groups)]
    n_groups <- length(unique(non_na_groups))
    if (n_groups < 2) return("invariant")
    if (length(non_na_groups) - n_groups < 2) return("insufficient data")
    NA_character_
  }

  # Analyse each numeric column; everything else (factors, characters, the
  # grouping variable itself) is ignored.
  numeric_cols <- colnames(aov_data)[sapply(aov_data, is.numeric)]
  stop_if_type_column(numeric_cols)
  skip_labels <- character(0)

  for (columnname in numeric_cols) {
    skip_label <- col_skip_label(columnname)
    if (!is.na(skip_label)) {
      skip_labels[columnname] <- skip_label
      summary_table[[columnname]] <- rep(skip_label, length(base_rows))
      next
    }

    # Run ANOVA and Tukey HSD. Backticks allow non-syntactic column names.
    # na.omit is set explicitly (rather than relying on the session's
    # na.action option) so rows with an NA in this column are dropped for
    # this column's ANOVA only. If the model or Tukey test still fails on
    # degenerate data despite the checks above, skip the column with a
    # warning naming it rather than failing the whole table.
    fit <- tryCatch({
      formula <- as.formula(paste0("`", columnname, "` ~ `", group_var, "`"))
      t.anova <- aov(formula, data = aov_data, na.action = na.omit)
      test2 <- agricolae::HSD.test(t.anova, trt = group_var, group = TRUE)
      list(aov = t.anova, tukey = test2)
    }, error = function(e) e)

    if (inherits(fit, "error")) {
      warning("Column '", columnname, "' could not be analysed: ",
              conditionMessage(fit), call. = FALSE)
      skip_labels[columnname] <- "not analysable"
      summary_table[[columnname]] <- rep("not analysable", length(base_rows))
      next
    }
    t.anova <- fit$aov
    test2 <- fit$tukey

    raw_outputs[[columnname]] <- fit

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
  # them as two new rows. Skipped columns keep their skip label.
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
      bh_rows[[col]] <- rep(skip_labels[[col]], 2)
    }
  }
  summary_table <- rbind(summary_table, bh_rows)

  warn_skipped_columns(skip_labels)

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
#'  Missing values are allowed: observations with an NA in a given variable are
#'  dropped for that variable's ANOVA only, and group means are computed over
#'  the observed values. Columns that cannot be analysed are filled with a
#'  label instead, and a warning lists them: "INVARIANT" (constant, all NA,
#'  or fewer than two levels of a grouping variable observed),
#'  "INSUFFICIENT DATA" (fewer than two residual degrees of freedom left
#'  after removing NAs, the minimum Tukey's test can handle), or
#'  "NOT ANALYSABLE" (the model or Tukey test failed for another reason).
#'  It also appends BH-corrected p-values and significance as additional rows.
#'  The BH correction is applied separately within each effect family (i.e. all
#'  p-values for a given main effect or interaction are corrected together,
#'  across variables). A "Type" column is added so that the row labels export
#'  correctly to Excel.
#'
#' @details
#' \strong{Sums of squares and unbalanced designs.} P-values and F-values use
#' Type II sums of squares: each effect is tested after all other effects
#' that do not contain it (a main effect is adjusted for the other main
#' effects, but not for interactions it is part of), against the residual
#' mean square of the full model. The results therefore do not depend on the
#' order of \code{group_vars}. In a balanced design (the same number of
#' observations in every combination of the grouping variables) Type II
#' results are identical to the sequential (Type I) results of
#' \code{summary(aov())}. In an unbalanced design they differ, and a warning
#' names the affected variables. Note that a design can be balanced overall
#' but unbalanced for a particular variable, because observations with an NA
#' in that variable are dropped for its ANOVA. Group means and Tukey letters
#' come from the full model and are unaffected. With \code{return_raw = TRUE}
#' the Type II table for each variable is stored as \code{type2} alongside
#' the \code{aov} and \code{tukey} objects.
#'
#' @param aov_data The data frame containing the data.
#' @param group_vars A vector of column names containing the grouping variables.
#' @param output_file Optional file name (with .xlsx extension) to save the table.
#' @param return_raw Logical; if TRUE, raw outputs are assigned to a global variable.
#' @param output_name The name of the global variable to store raw outputs.
#' @return A data frame summary table.
#' @importFrom stats aov as.formula na.omit p.adjust
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

  # Helper: NA if the column can be analysed, otherwise the label to fill its
  # rows with. "INVARIANT": the response is constant or all NA (NAs don't
  # count as a distinct value), or any group_var has <2 levels after removing
  # NAs for that column (which would cause the "contrasts" error).
  # "INSUFFICIENT DATA": fewer than two residual degrees of freedom remain
  # after removing NAs. Two is the minimum HSD.test can handle: R's
  # qtukey/ptukey return NaN for df < 2, which crashes HSD.test.
  col_skip_label <- function(col_name) {
    col_vals <- aov_data[[col_name]]
    if (length(unique(col_vals[!is.na(col_vals)])) <= 1) return("INVARIANT")
    # Observations with a missing grouping value are dropped by aov(), so
    # they must not count as a cell or towards the residual degrees of freedom.
    non_na_data <- aov_data[!is.na(col_vals) &
                              stats::complete.cases(aov_data[group_vars]), ]
    if (any(vapply(group_vars,
                   function(gv) length(unique(non_na_data[[gv]])) < 2,
                   logical(1)))) {
      return("INVARIANT")
    }
    n_cells <- nrow(unique(non_na_data[group_vars]))
    if (nrow(non_na_data) - n_cells < 2) return("INSUFFICIENT DATA")
    NA_character_
  }

  # Analyse each numeric column; everything else (factors, characters, the
  # grouping variables themselves) is ignored.
  numeric_cols <- colnames(aov_data)[sapply(aov_data, is.numeric)]
  stop_if_type_column(numeric_cols)
  skip_labels <- vapply(numeric_cols, col_skip_label, character(1))
  model_cols <- numeric_cols[is.na(skip_labels)]

  # Fit each model once, caching the ANOVA and Tukey HSD results.
  # Backticks allow non-syntactic column names.
  # na.omit is set explicitly (rather than relying on the session's
  # na.action option) so rows with an NA in a column are dropped for
  # that column's ANOVA only.
  rhs <- paste0("`", group_vars, "`", collapse = " * ")
  models <- lapply(model_cols, function(col_name) {
    tryCatch({
      formula <- as.formula(paste0("`", col_name, "` ~ ", rhs))
      t.anova <- aov(formula, data = aov_data, na.action = na.omit)
      test2 <- agricolae::HSD.test(t.anova, trt = group_vars, group = TRUE)
      # Balanced: every combination of the grouping variables has the same
      # number of observations for this column (after removing NAs).
      cell_n <- table(droplevels(stats::model.frame(t.anova)[group_vars]))
      list(aov = t.anova, tukey = test2, type2 = type2_anova(t.anova),
           balanced = length(unique(as.vector(cell_n))) == 1)
    }, error = function(e) e)
  })
  names(models) <- model_cols

  # If a model or Tukey test still fails on degenerate data despite the
  # checks above, skip that column with a warning naming it rather than
  # failing the whole table.
  failed <- vapply(models, inherits, logical(1), what = "error")
  for (col_name in model_cols[failed]) {
    warning("Column '", col_name, "' could not be analysed: ",
            conditionMessage(models[[col_name]]), call. = FALSE)
    skip_labels[col_name] <- "NOT ANALYSABLE"
  }
  models <- models[!failed]
  model_cols <- model_cols[!failed]
  skipped_cols <- numeric_cols[!is.na(skip_labels)]
  raw_outputs <- models

  # Collect all treatment-combination row names across variables.
  row_names <- unique(unlist(lapply(models, function(m) rownames(m$tukey$groups))))

  # All effect names (main effects and interactions). These come from the
  # model terms, so they are the same for every variable.
  effect_names <- c()
  if (length(models) > 0) {
    effect_names <- rownames(models[[1]]$type2)
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
    # from the Type II table. An effect that cannot be estimated (e.g. an
    # interaction with empty cells) has NA statistics.
    aov_summary <- models[[col_name]]$type2
    effect_stats <- c()
    for (effect in effect_names) {
      if (effect %in% rownames(aov_summary) &&
          !is.na(aov_summary[effect, "Pr(>F)"])) {
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

  # Populate skipped columns with their skip label for every row
  for (col_name in skipped_cols) {
    summary_list[[col_name]] <- rep(skip_labels[[col_name]], length(row_labels))
  }

  # Restore original column order (analysed + skipped may be out of order)
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
        bh_rows[[col]] <- rep(skip_labels[[col]], 2)
      }
    }
    summary_table <- rbind(summary_table, bh_rows)
  }

  warn_skipped_columns(skip_labels[!is.na(skip_labels)])

  unbalanced_cols <- model_cols[!vapply(models, function(m) m$balanced,
                                        logical(1))]
  if (length(unbalanced_cols) > 0) {
    warning(
      "Unbalanced design (unequal numbers of observations per combination ",
      "of grouping variables, after removing NAs) for: ",
      paste(unbalanced_cols, collapse = ", "),
      ". P-values and F-values use Type II sums of squares, so they do not ",
      "depend on the order of group_vars, but they differ from ",
      "summary(aov()), which is sequential (Type I).",
      call. = FALSE
    )
  }

  if (!is.null(output_file)) {
    openxlsx::write.xlsx(summary_table, output_file, rowNames = FALSE)
  }

  if (return_raw) {
    assign(output_name, raw_outputs, envir = .GlobalEnv)
  }

  return(summary_table)

}
