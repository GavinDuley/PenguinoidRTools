# Some R AoV summary functions
# https://github.com/GavinDuley/PenguinoidRTools
# Copyright (c) 2024 onwards, Gavin Duley
# Licenced under the GPL-3.0 licence

# aovSummaryTable -------------------------------------------------------------

#' AoV summary table
#'
#' @name aovSummaryTable
#' @description This allows you to generate a table of means and key statistical
#'  values from an analysis of variance (AoV) test for a whole dataframe. 
#'  It uses the agricolae, dplyr, gtools, and openxlsx packages.\cr
#'  Please convert any non-numeric columns to factors before using this function.
#'  Ideally, factor columns should be the first few columns in the data frame.\cr
#'  It does not cope with interactions at the moment.\cr
#'  NB, BH = Benjamini-Hochberg (FDR) Correction
#'
#' @param aov_data The data frame containing the data to be analysed.
#' @param group_var The name of the column containing the group names (in quotes).
#' @param output_file If a file name is supplied (in quotes), it will output an Excel file with the summary table. The filename should have an .xlsx extension.
#' @param return_raw Logical. If TRUE, assigns a list with raw ANOVA and Tukey test outputs to the specified object. Default is FALSE.
#' @param output_name The name of the list object to store raw ANOVA and Tukey results. Default is "aov_results".
#' @return Returns a summary table and (optionally) saves it as an Excel file in the active directory. If `return_raw` is TRUE, also assigns a list with raw ANOVA and Tukey test outputs.
#' @examples
#' # Load the necessary package
#' library(agricolae)
#' 
#' # Load the greenhouse dataset
#' data(greenhouse, package = "agricolae")
#' 
#' # Extract the greenhouse1 data
#' aov_data <- greenhouse$greenhouse1
#' 
#' # Run the aovSummaryTable function
#' result <- aovSummaryTable(
#'   aov_data = aov_data, 
#'   group_var = "variety", 
#'   return_raw = TRUE
#' )
#' 
#' # Print the result (the summary table)
#' print(result)
#' @importFrom stats aov as.formula setNames p.adjust
#' @importFrom agricolae HSD.test
#' @importFrom openxlsx write.xlsx
#' @import dplyr
#' @importFrom dplyr `%>%`
#' @importFrom stringr str_replace
#' @importFrom rlang .data
#' @export
aovSummaryTable <- function(aov_data, 
                            group_var, 
                            output_file = NULL,
                            return_raw = FALSE,
                            output_name = "aov_results") {
  # Get the group levels and initialize the summary table.
  group_levels <- levels(as.factor(aov_data[[group_var]]))
  # Initial rows: one per group level, then "P-value", "F-value", "Significant"
  summary_table <- as.data.frame(c(group_levels, "P-value", "F-value", "Significant"))
  names(summary_table) <- group_var
  
  # This vector will store raw numeric p-values for later BH correction.
  pvalues_vec <- c()
  
  # Initialize the list for raw outputs if return_raw is TRUE.
  if (return_raw) {
    raw_outputs <- list()
  }
  
  # Identify the indices of factor columns.
  factor_indices <- which(sapply(aov_data, is.factor))
  
  # Iterate over columns, skipping factor columns.
  for (i in 1:ncol(aov_data)){
    if (i %in% factor_indices) {
      next  # Skip this iteration if the column is a factor
    }
    # Check if the column is numeric
    if (!is.numeric(aov_data[[i]])) {
      stop(paste("Error: Column", colnames(aov_data)[i], "is not numeric or factor."))
    }
    columnname <- colnames(aov_data)[i]
    
    if (length(unique(aov_data[, i])) == 1) {
      # For invariant columns, add a column of "invariant" values.
      summary_table <- cbind(summary_table, "invariant")
      colnames(summary_table)[ncol(summary_table)] <- columnname
    } else {
      t.anova <- aov(as.formula(paste(columnname, "~", group_var)), data = aov_data)
      test2 <- agricolae::HSD.test(t.anova, trt = group_var, group = TRUE)
      
      # Save raw outputs if return_raw is TRUE.
      if (return_raw) {
        raw_outputs[[columnname]] <- list(aov = t.anova, tukey = test2)
      }
      
      # Initialize group summaries with all factor levels.
      group_summaries <- rep("Not available", length(group_levels))
      names(group_summaries) <- group_levels
      
      # Fill in available group summaries from the Tukey test.
      for (grp in rownames(test2[["means"]])) {
        if (grp %in% group_levels) {
          group_summaries[grp] <- paste0(
            signif(test2[["means"]][grp, columnname], digits = 4),
            " ", test2[["groups"]][grp, "groups"]
          )
        }
      }
      
      # Extract raw p-value and F-value from the ANOVA summary.
      raw_p <- summary(t.anova)[[1]][1, 5]
      p_value_str <- paste0(signif(raw_p, digits = 4), " ", stars.pval(raw_p))
      f_value <- signif(summary(t.anova)[[1]][1, 4], digits = 4)
      significant <- if (raw_p <= 0.05) "SIGNIFICANT" else "NOT SIGNIFICANT"
      
      # Store the raw p-value (for BH correction later).
      pvalues_vec[columnname] <- raw_p
      
      # Append the group summaries, P-value, F-value, and significance as a new column.
      summary_table <- cbind(summary_table, c(
        group_summaries,
        p_value_str,
        f_value,
        significant
      ))
      colnames(summary_table)[ncol(summary_table)] <- columnname
    }
  }
  
  # After processing all variables, add the BH-corrected rows (if any p-values were collected).
  if (length(pvalues_vec) > 0) {
    bh_p_values <- p.adjust(pvalues_vec, method = "BH")
    # The first column of summary_table is the grouping column; we fill its new rows with an empty string.
    bh_corr_row <- c("")
    bh_sig_row <- c("")
    # Loop over the variable columns (i.e. all columns except the first one).
    for (col in colnames(summary_table)[-1]) {
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
                           "BH-Corrected-P-Value" = bh_corr_row,
                           "BH-Significant" = bh_sig_row)
  } else {
    summary_table <- rbind(summary_table,
                           "BH-Corrected-P-Value" = rep(NA, ncol(summary_table)),
                           "BH-Significant" = rep(NA, ncol(summary_table)))
  }
  
  # Set the row names.
  base_rows <- c(group_levels, "P-value", "F-value", "Significant")
  if (nrow(summary_table) == length(base_rows) + 2) {
    rownames(summary_table) <- c(base_rows, "BH-Corrected-P-Value", "BH-Significant")
  } else {
    rownames(summary_table) <- base_rows
  }
  
  # Write to Excel if output_file is specified.
  if (!is.null(output_file)) {
    openxlsx::write.xlsx(summary_table, output_file, rowNames = FALSE)
  }
  
  # If return_raw is TRUE, assign raw outputs to the specified object name in the global environment.
  if (return_raw) {
    assign(output_name, raw_outputs, envir = .GlobalEnv)
  }
  
  # Return the summary table.
  return(summary_table)
}

# aovInterSummaryTable -----------------------------------------------------

#' AoV summary table with interactions
#'
#' @name aovInteractSummaryTable
#' @description This allows you to generate a table of means and key statistical
#'  values from an analysis of variance (AoV) test for a whole dataframe. 
#'  It uses the agricolae, dplyr, gtools, and openxlsx packages.\cr
#'  Please convert any non-numeric columns to factors before using this function.
#'  Ideally, factor columns should be the first few columns in the data frame.\cr
#'  It handles interactions among multiple group variables. \cr
#'  NB, BH = Benjamini-Hochberg (FDR) Correction
#'
#' @param aov_data The data frame containing the data to be analysed.
#' @param group_vars A vector of column names containing the group names (in quotes). If multiple group variables are provided, interactions will be considered.
#' @param output_file If a file name is supplied (in quotes), it will output an Excel file with the summary table. The filename should have an .xlsx extension.
#' @param return_raw Logical. If TRUE, assigns a list with raw ANOVA and Tukey test outputs to the specified object. Default is FALSE.
#' @param output_name The name of the list object to store raw ANOVA and Tukey results. Default is "aov_results".
#' @return Returns a summary table and (optionally) saves it as an Excel file in the active directory. If `return_raw` is TRUE, also assigns a list with raw ANOVA and Tukey test outputs.
#' @examples
#' # Load the necessary package
#' library(agricolae)
#' 
#' # Load the greenhouse dataset
#' data(greenhouse, package = "agricolae")
#' 
#' # Extract the greenhouse1 data
#' aov_data <- greenhouse$greenhouse1
#' 
#' # Run the aovInteractSummaryTable function
#' result <- aovInteractSummaryTable(
#'   aov_data = aov_data, 
#'   group_vars = c("variety", "method"), 
#'   output_file = NULL, 
#'   return_raw = TRUE, 
#'   output_name = "aov_results"
#' )
#' 
#' # Print the result (the summary table)
#' print(result)
#' @importFrom stats aov as.formula setNames p.adjust
#' @importFrom agricolae HSD.test
#' @importFrom openxlsx write.xlsx
#' @import dplyr
#' @importFrom dplyr `%>%`
#' @importFrom stringr str_replace
#' @importFrom rlang .data
#' @importFrom gtools stars.pval
#' @export
aovInteractSummaryTable <- function(aov_data, 
                                    group_vars, 
                                    output_file = NULL,
                                    return_raw = FALSE,
                                    output_name = "aov_results") {
  # Check if group_vars are valid.
  if (!all(group_vars %in% colnames(aov_data))) {
    stop("Error: One or more group_vars are not present in aov_data.")
  }
  
  # Initialize raw outputs list if return_raw is TRUE.
  if (return_raw) {
    raw_outputs <- list()
  }
  
  # Initialize the summary list and a vector to collect raw p-values.
  summary_list <- list()
  row_names <- NULL
  pvalues_vec <- c()
  
  # Iterate over numeric columns in the dataset.
  for (col_name in colnames(aov_data)) {
    if (!is.numeric(aov_data[[col_name]])) next
    if (is.factor(aov_data[[col_name]])) next
    
    # Construct the formula including interactions.
    formula <- as.formula(paste(col_name, "~", paste(group_vars, collapse = "*")))
    t.anova <- aov(formula, data = aov_data)
    test2 <- agricolae::HSD.test(t.anova, trt = group_vars, group = TRUE)
    
    # Store raw outputs if return_raw is TRUE.
    if (return_raw) {
      raw_outputs[[col_name]] <- list(aov = t.anova, tukey = test2)
    }
    
    # Capture row names from the Tukey test (only once).
    if (is.null(row_names)) {
      row_names <- rownames(test2$groups)
    }
    
    # Create summary for each group.
    group_summaries <- rep(NA, length(row_names))
    for (i in seq_along(row_names)) {
      group_name <- row_names[i]
      mean_value <- signif(test2$means[[col_name]][i], digits = 4)
      group_summary <- paste0(mean_value, " ", test2$groups$groups[i])
      group_summaries[i] <- group_summary
    }
    
    # Extract raw p-value and F-value.
    raw_p <- summary(t.anova)[[1]][1, 5]
    p_value_str <- paste0(signif(raw_p, digits = 4), " ", stars.pval(raw_p))
    f_value <- signif(summary(t.anova)[[1]][1, 4], digits = 4)
    significant <- if (raw_p <= 0.05) "SIGNIFICANT" else "NOT SIGNIFICANT"
    
    # Store the raw p-value for BH correction.
    pvalues_vec[col_name] <- raw_p
    
    # Append the summary vector for this variable.
    summary_list[[col_name]] <- c(group_summaries, p_value_str, f_value, significant)
  }
  
  # Create the summary table data frame.
  summary_table <- as.data.frame(do.call(cbind, summary_list), stringsAsFactors = FALSE)
  rownames(summary_table) <- c(row_names, "P-value", "F-value", "Significant")
  
  # Add the BH-corrected rows.
  if (length(pvalues_vec) > 0) {
    bh_p_values <- p.adjust(pvalues_vec, method = "BH")
    bh_corr_row <- c()
    bh_sig_row <- c()
    # Loop over each variable (i.e. each column).
    for (col in colnames(summary_table)) {
      bh_val <- bh_p_values[col]
      bh_corr_row <- c(bh_corr_row, paste0(signif(bh_val, digits = 4), " ", stars.pval(bh_val)))
      bh_sig_row <- c(bh_sig_row, ifelse(bh_val <= 0.05, "SIGNIFICANT", "NOT SIGNIFICANT"))
    }
    summary_table <- rbind(summary_table,
                           "BH-Corrected-P-Value" = bh_corr_row,
                           "BH-Significant" = bh_sig_row)
  } else {
    summary_table <- rbind(summary_table,
                           "BH-Corrected-P-Value" = rep(NA, ncol(summary_table)),
                           "BH-Significant" = rep(NA, ncol(summary_table)))
  }
  
  # Write to Excel if output_file is specified.
  if (!is.null(output_file)) {
    openxlsx::write.xlsx(summary_table, output_file, rowNames = TRUE)
  }
  
  # Assign raw outputs to the global environment if return_raw is TRUE.
  if (return_raw) {
    assign(output_name, raw_outputs, envir = .GlobalEnv)
  }
  
  # Return the summary table.
  return(summary_table)
}
