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
#'  It does not cope with interactions at the moment.
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
#' @importFrom stats aov as.formula setNames
#' @importFrom agricolae HSD.test
#' @importFrom openxlsx write.xlsx
#' @import dplyr
#' @importFrom dplyr `%>%`
#' @importFrom stringr str_replace
#' @importFrom rlang .data
#' @importFrom gtools stars.pval
#' @export
aovSummaryTable <- function(aov_data, 
                            group_var, 
                            output_file = NULL,
                            return_raw = FALSE,
                            output_name = "aov_results") {
  # Initialize the summary table
  summary_table <- as.data.frame(c(levels(as.factor(aov_data[[group_var]])),
                                   "P-value",
                                   "F-value", 
                                   "Significant")) %>% setNames(group_var)
  
  # Identify the indices of factor columns
  factor_indices <- which(sapply(aov_data, is.factor))
  
  # Initialize the list for raw outputs if return_raw is TRUE
  if (return_raw) {
    raw_outputs <- list()
  }
  
  # Iterate over columns, skipping factor columns
  for (i in 1:ncol(aov_data)){
    if (i %in% factor_indices) {
      next  # Skip this iteration if the column is a factor
    }
    # Check if the column is numeric
    if (!is.numeric(aov_data[[i]])) {
      stop(paste("Error: Column", colnames(aov_data)[i], "is not numeric or factor."))
    }
    columnname <- colnames(aov_data)[i]
    
    if (length(unique(aov_data[,i])) == 1) {
      summary_table <- cbind(summary_table, "invariant")
      colnames(summary_table)[ncol(summary_table)] <- columnname
    } else {
      t.anova <- aov(as.formula(paste(columnname, paste0("~ ", group_var))), data = aov_data)
      test2 <- agricolae::HSD.test(t.anova, trt = group_var, group = T)
      
      # Save raw outputs if return_raw is TRUE
      if (return_raw) {
        raw_outputs[[columnname]] <- list(aov = t.anova, tukey = test2)
      }
      
      # Create a vector to store group summaries
      group_summaries <- c()
      for (j in 1:length(test2[["means"]][[columnname]])) {
        group_summaries <- c(group_summaries, paste0(signif(test2[["means"]][[columnname]][j],digits=4), " ", test2[["groups"]][["groups"]][j]))
      }
      
      summary_table <- cbind(summary_table, c(
        group_summaries,
        paste0(signif(summary(t.anova)[[1]][1,5],digits=4), " ", summary(t.anova)[[1]][1,5] %>% stars.pval),
        signif(summary(t.anova)[[1]][1,4],digits=4),
        if(summary(t.anova)[[1]][1,5] <= 0.05){
          "SIGNIFICANT"
        } else {
          "NOT SIGNIFICANT"
        }
      ))
      colnames(summary_table)[ncol(summary_table)] <- columnname
    }
  }
  
  # Write to Excel only if output_file is specified
  if (!is.null(output_file)) {
    write.xlsx(summary_table, output_file, rowNames = FALSE)
  }
  
  # If return_raw is TRUE, assign raw outputs to the specified object name in the global environment
  if (return_raw) {
    assign(output_name, raw_outputs, envir = .GlobalEnv)
  }
  
  # Return the summary table
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
#'  It handles interactions among multiple group variables.
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
#' @importFrom stats aov as.formula setNames
#' @importFrom agricolae HSD.test
#' @importFrom openxlsx write.xlsx
#' @import dplyr
#' @importFrom dplyr `%>%`
#' @importFrom stringr str_replace
#' @importFrom rlang .data
#' @importFrom gtools stars.pval
#' @export
#' 
aovInteractSummaryTable <- function(aov_data, 
                                    group_vars, 
                                    output_file = NULL,
                                    return_raw = FALSE,
                                    output_name = "aov_results") {
  # Check if group_vars are valid
  if (!all(group_vars %in% colnames(aov_data))) {
    stop("Error: One or more group_vars are not present in aov_data.")
  }
  
  # Initialize raw outputs list if return_raw is TRUE
  if (return_raw) {
    raw_outputs <- list()
  }
  
  # Initialize the summary table list
  summary_list <- list()
  row_names <- NULL
  
  # Iterate over numeric columns in the dataset
  for (col_name in colnames(aov_data)) {
    if (!is.numeric(aov_data[[col_name]])) {
      next  # Skip non-numeric columns
    }
    
    # Skip factor columns for now
    if (is.factor(aov_data[[col_name]])) {
      next
    }
    
    # Construct the formula
    formula <- as.formula(paste(col_name, "~", paste(group_vars, collapse = "*")))
    
    # Perform ANOVA
    t.anova <- aov(formula, data = aov_data)
    test2 <- agricolae::HSD.test(t.anova, trt = group_vars, group = TRUE)
    
    # Store raw outputs if return_raw is TRUE
    if (return_raw) {
      raw_outputs[[col_name]] <- list(aov = t.anova, tukey = test2)
    }
    
    # Extract row names for the summary table
    if (is.null(row_names)) {
      row_names <- rownames(test2$groups)
    }
    
    # Create summary for each column
    group_summaries <- rep(NA, length(row_names))
    for (i in seq_along(row_names)) {
      group_name <- row_names[i]
      mean_value <- signif(test2$means[[col_name]][i], digits = 4)
      group_summary <- paste0(mean_value, " ", test2$groups$groups[i])
      group_summaries[i] <- group_summary
    }
    
    # Append results to the summary list
    p_value <- signif(summary(t.anova)[[1]][1, 5], digits = 4)
    f_value <- signif(summary(t.anova)[[1]][1, 4], digits = 4)
    significant <- if (p_value <= 0.05) "SIGNIFICANT" else "NOT SIGNIFICANT"
    
    summary_list[[col_name]] <- c(group_summaries, paste0(p_value, " ", stars.pval(p_value)), f_value, significant)
  }
  
  # Create data frame from the summary list
  summary_table <- as.data.frame(do.call(cbind, summary_list), stringsAsFactors = FALSE)
  rownames(summary_table) <- c(row_names, "P-value", "F-value", "Significant")
  
  # Write to Excel if output_file is specified
  if (!is.null(output_file)) {
    openxlsx::write.xlsx(summary_table, output_file, rowNames = TRUE)
  }
  
  # Assign raw outputs to the global environment if return_raw is TRUE
  if (return_raw) {
    assign(output_name, raw_outputs, envir = .GlobalEnv)
  }
  
  # Return the summary table
  return(summary_table)
}
