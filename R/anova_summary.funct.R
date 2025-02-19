# Some R AoV summary functions
# https://github.com/GavinDuley/PenguinoidRTools
# Copyright (c) 2024 onwards, Gavin Duley
# Licenced under the GPL-3.0 licence

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

# aovInterSummaryTable -----------------------------------------------------

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
  row_names <- NULL
  pvalues_vec <- c()
  
  # Iterate over numeric columns.
  for (col_name in colnames(aov_data)) {
    if (!is.numeric(aov_data[[col_name]])) next
    if (is.factor(aov_data[[col_name]])) next
    
    formula <- as.formula(paste(col_name, "~", paste(group_vars, collapse = "*")))
    t.anova <- aov(formula, data = aov_data)
    test2 <- agricolae::HSD.test(t.anova, trt = group_vars, group = TRUE)
    
    if (return_raw) {
      raw_outputs[[col_name]] <- list(aov = t.anova, tukey = test2)
    }
    
    if (is.null(row_names)) {
      row_names <- rownames(test2$groups)
    }
    
    group_summaries <- rep(NA, length(row_names))
    for (i in seq_along(row_names)) {
      group_name <- row_names[i]
      mean_value <- signif(test2$means[[col_name]][i], digits = 4)
      group_summaries[i] <- paste0(mean_value, " ", test2$groups$groups[i])
    }
    
    p_value <- signif(summary(t.anova)[[1]][1, 5], digits = 4)
    f_value <- signif(summary(t.anova)[[1]][1, 4], digits = 4)
    significance <- if (p_value <= 0.05) "SIGNIFICANT" else "NOT SIGNIFICANT"
    
    pvalues_vec[col_name] <- summary(t.anova)[[1]][1, 5]
    
    summary_list[[col_name]] <- c(group_summaries,
                                  paste0(p_value, " ", stars.pval(p_value)),
                                  f_value,
                                  significance)
  }
  
  # Create the summary table data frame.
  summary_table <- as.data.frame(do.call(cbind, summary_list), stringsAsFactors = FALSE)
  summary_table <- cbind(Type = c(row_names, "P-value", "F-value", "Significant","BH Cor. p-value", "BH Significant"), summary_table)
  
  # Compute BH-corrected p-values.
  if (length(pvalues_vec) > 0) {
    bh_p_values <- p.adjust(pvalues_vec, method = "BH")
    bh_corr_row <- c("BH-Corrected-P-value")
    bh_sig_row <- c("BH-Significant")
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
    summary_table <- rbind(summary_table,
                           setNames(as.list(bh_corr_row), colnames(summary_table)),
                           setNames(as.list(bh_sig_row), colnames(summary_table)))
  } else {
    new_row <- rep(NA, ncol(summary_table))
    summary_table <- rbind(summary_table,
                           setNames(new_row, colnames(summary_table)),
                           setNames(new_row, colnames(summary_table)))
    summary_table[nrow(summary_table)-1, "Type"] <- "BH-Corrected-P-value"
    summary_table[nrow(summary_table), "Type"] <- "BH-Significant"
  }
  
  if (!is.null(output_file)) {
    openxlsx::write.xlsx(summary_table, output_file, rowNames = FALSE)
  }
  
  if (return_raw) {
    assign(output_name, raw_outputs, envir = .GlobalEnv)
  }
  
  return(summary_table)
}
