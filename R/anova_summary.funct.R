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
<<<<<<< HEAD
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
=======
#' @return Returns a summary table and (optionally) saves it as an Excel file in the active directory.
#' @examples aovSummaryTable(aov_data, "Group", "stats_summary.xlsx")
#' @importFrom dplyr %>%
#' @importFrom stats aov lm
#' @importFrom agricolae HSD.test
#' @importFrom openxlsx write.xlsx
#' @importFrom gtools stars.pval
#' @importFrom snakecase to_any_case
#' @export
aovSummaryTable <- function(aov_data, 
                            group_var, 
                            output_file = NULL) {
  # # List of required packages
  # packages <- c("agricolae", "dplyr", "gtools", "openxlsx")
  # # Check if packages are installed
  # for(pkg in packages){
  #   if(!require(pkg, character.only = TRUE)){
  #     install.packages(pkg)
  #     library(pkg, character.only = TRUE)
  #   }
  # }
  # checks for characters aov() will object to. Thanks to ChatGPT.
  # Get the variable names from the dataframe
  var_names <- names(aov_data)
  # convert the names with snakecase::to_any_case
  converted_var_names <- snakecase::to_any_case(var_names, case = "none")
  # Check if any variable names were changed
  if (!all(var_names == converted_var_names)) {
    stop(paste0("Some variable names contain special characters or spaces. 
                Please rename your variables. This can be done easily using 
                janitor::clean_names()."))
  }
  # Check if variable name starts with a number
  for (var_name in var_names) {
    if (grepl("^[0-9]", var_name)) {
      stop(paste0("Variable name '", var_name, "' starts with a number. 
                  Please rename your variables. This can be done easily using 
                  janitor::clean_names()."))
    }
  }
  # Create a summary table with the group variable as the first column
>>>>>>> 78de140e8b07677a9bb7200299ca7501ec55197b
  summary_table <- as.data.frame(c(levels(as.factor(aov_data[[group_var]])),
                                   "P-value",
                                   "F-value", 
                                   "Significant")) %>% setNames(group_var)
<<<<<<< HEAD
  
=======

>>>>>>> 78de140e8b07677a9bb7200299ca7501ec55197b
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
<<<<<<< HEAD
      t.anova <- aov(as.formula(paste(columnname, paste0("~ ", group_var))), data = aov_data)
      test2 <- agricolae::HSD.test(t.anova, trt = group_var, group = T)
      
      # Save raw outputs if return_raw is TRUE
      if (return_raw) {
        raw_outputs[[columnname]] <- list(aov = t.anova, tukey = test2)
      }
=======
      t.anova <- as.formula(paste(colnames(aov_data)[i], paste0("~ ", group_var))) %>%
        aov(data=aov_data)
      test2 <- as.formula(paste(colnames(aov_data)[i], paste0("~ ", group_var))) %>%
        lm(data = aov_data) %>% 
        HSD.test(group_var)
      # reorder rownames of test2$groups to match the same order as aov_data
      # this should fix the bug whereby the wrong mean and group were showing for 
      # the individual rows
      correct_order <- match(rownames(test2$means), rownames(test2$groups))
      test2$groups <- test2$groups[correct_order,]
>>>>>>> 78de140e8b07677a9bb7200299ca7501ec55197b
      
      # Create a vector to store group summaries
      group_summaries <- c()
      for (j in 1:length(test2[["groups"]][[columnname]])) {
        group_summaries <- c(group_summaries, paste0(round(test2$groups[j,1],digits=2), " ", test2$groups[j,2]))
      }
      
      summary_table <- cbind(summary_table, c(
        group_summaries,
        paste0(round(summary(t.anova)[[1]][1,5],digits=2), " ", summary(t.anova)[[1]][1,5] %>% stars.pval),
        round(summary(t.anova)[[1]][1,4],digits=2),
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
<<<<<<< HEAD
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
=======
#' @return Returns a summary table and (optionally) saves it as an Excel file in the active directory.
#' @examples aovSummaryTable(aov_data, "Group1","*","Group2", "stats_summary.xlsx")
#' @importFrom dplyr %>%
#' @importFrom stats aov lm
#' @importFrom agricolae HSD.test
#' @importFrom openxlsx write.xlsx
#' @importFrom gtools stars.pval
#' @importFrom snakecase to_any_case
>>>>>>> 78de140e8b07677a9bb7200299ca7501ec55197b
#' @export
#' 
library(dplyr)
library(agricolae)
library(openxlsx)
library(gtools)
aovInteractSummaryTable <- function(aov_data, 
<<<<<<< HEAD
                                 group_vars, 
                                 output_file = NULL,
                                 return_raw = FALSE,
                                 output_name = "aov_results") {
=======
                                    group_var_1,
                                    interaction_funct = "*",
                                    group_var_2,
                                    output_file = NULL) {
  # checks for characters aov() will object to. Thanks to ChatGPT.
  # Get the variable names from the dataframe
  var_names <- names(aov_data)
  # convert the names with snakecase::to_any_case
  converted_var_names <- snakecase::to_any_case(var_names, case = "none")
  # Check if any variable names were changed
  if (!all(var_names == converted_var_names)) {
    stop(paste0("Some variable names contain special characters or spaces. 
                Please rename your variables. This can be done easily using 
                janitor::make_clean_names()."))
  }
  # Check if variable name starts with a number
  for (var_name in var_names) {
    if (grepl("^[0-9]", var_name)) {
      stop(paste0("Variable name '", var_name, "' starts with a number. 
                  Please rename your variables. This can be done easily using 
                  janitor::clean_names()."))
    }
  }
  # Names of interactions for summary table
  group_var_1_levels <- levels(as.factor(aov_data[[group_var_1]]))
  group_var_2_levels <- levels(as.factor(aov_data[[group_var_2]]))
  interactions <- expand.grid(group_var_1_levels, group_var_2_levels) %>%
    apply(1,paste,collapse=".")
  summary_table <- as.data.frame(c(levels(as.factor(aov_data[[group_var_1]])),
                                   paste0(group_var_1, " p value"),
                                   levels(as.factor(aov_data[[group_var_2]])),
                                   paste0(group_var_2, " p value"),
                                   interactions,
                                   "Interaction p value")) %>% 
    setNames(group_var_1)
>>>>>>> 78de140e8b07677a9bb7200299ca7501ec55197b
  # Identify the indices of factor columns
  factor_indices <- which(sapply(aov_data, is.factor))
  
  # Initialize the list for raw outputs if return_raw is TRUE
  if (return_raw) {
    raw_outputs <- list()
  }
  
  # Initialize an empty list to collect columns for the summary table
  summary_list <- list()
  row_names <- NULL
  
  # Iterate over columns, skipping factor columns
  for (i in 1:ncol(aov_data)) {
    if (i %in% factor_indices) {
      next  # Skip this iteration if the column is a factor
    }
    # Check if the column is numeric
    if (!is.numeric(aov_data[[i]])) {
      stop(paste("Error: Column", colnames(aov_data)[i], "is not numeric or factor."))
    }
    columnname <- colnames(aov_data)[i]
    
    if (length(unique(aov_data[, i])) == 1) {
      # Handle invariant columns
      summary_list[[columnname]] <- rep("invariant", length(row_names))
    } else {
      # Create the formula for ANOVA with interactions
      formula <- as.formula(paste(columnname, "~", paste(group_vars, collapse = "*")))
      t.anova <- aov(formula, data = aov_data)
      test2 <- agricolae::HSD.test(t.anova, trt = group_vars, group = T)
      
<<<<<<< HEAD
      # Save raw outputs if return_raw is TRUE
      if (return_raw) {
        raw_outputs[[columnname]] <- list(aov = t.anova, tukey = test2)
      }
      
      # Get row names from Tukey test
      if (is.null(row_names)) {
        row_names <- rownames(test2[["groups"]])
      }
      
      # Create a vector to store group summaries
      group_summaries <- rep(NA, length(row_names))
      for (j in seq_along(row_names)) {
        # Get the row name and corresponding values
        group_name <- row_names[j]
        mean_value <- signif(test2[["means"]][[columnname]][j], digits = 4)
        group_summary <- paste0(mean_value, " ", test2[["groups"]][["groups"]][j])
        group_summaries[j] <- group_summary
      }
      
      # Append results
      p_value <- signif(summary(t.anova)[[1]][1, 5], digits = 4)
      f_value <- signif(summary(t.anova)[[1]][1, 4], digits = 4)
      significant <- if (p_value <= 0.05) "SIGNIFICANT" else "NOT SIGNIFICANT"
      
      # Append to the list
      summary_list[[columnname]] <- c(group_summaries,
                                      paste0(p_value, " ", stars.pval(p_value)),
                                      f_value,
                                      significant)
    }
=======
    t.var1.aov <- as.formula(paste(colnames(aov_data)[i], "~", group_var_1)) %>%
      aov(data=aov_data)
    t.var2.aov <- as.formula(paste(colnames(aov_data)[i], "~", group_var_2)) %>%
      aov(data=aov_data)
    t.inter.aov <- as.formula(paste(colnames(aov_data)[i], 
                                    "~",
                                    group_var_1, 
                                    interaction_funct,
                                    group_var_2)) %>%
      aov(data=aov_data)

    #group_var1_str <- paste0("'", group_var_1, "'")
    test2_var1 <- as.formula(paste(colnames(aov_data)[i], paste0("~ ", group_var_1))) %>%
      lm(data = aov_data) %>% 
      HSD.test(group_var_1)

    #group_var2_str <- paste0("'", group_var_2, "'")    
    test2_var2 <- as.formula(paste(colnames(aov_data)[i], paste0("~ ", group_var_2))) %>%
      lm(data = aov_data) %>% 
      HSD.test(group_var_2)
    
    # next part, with thanks to Adriana
    # Create interaction term
    interaction_term <- interaction(aov_data[[group_var_1]], aov_data[[group_var_2]])
    
    # Fit the model
    formula <- as.formula(paste(columnname, "~ interaction_term"))
    test2_inter_model <- glm(formula, data = aov_data)
    
    # Perform ANOVA and HSD test
    test2_inter_aov <- anova(test2_inter_model)
    test2_inter <- HSD.test(test2_inter_model, "interaction_term", alpha = 0.05, group = TRUE, console = TRUE)

    # Create a vector to store group summaries
    group_summaries_1 <- c()
    for (j in 1:length(test2_var1[["means"]][[columnname]])) {
      group_summaries_1 <- c(group_summaries_1, paste0(signif(test2_var1[["means"]][[columnname]][j],digits=4), " ", test2_var1[["groups"]][["groups"]][j]))
    }
    
    group_summaries_2 <- c()
    for (j in 1:length(test2_var2[["means"]][[columnname]])) {
      group_summaries_2 <- c(group_summaries_2, paste0(signif(test2_var2[["means"]][[columnname]][j],digits=4), " ", test2_var2[["groups"]][["groups"]][j]))
    }
    
    # Create a named vector for the Tukey means
    tukey_means <- setNames(test2_inter[["means"]][[columnname]], rownames(test2_inter[["means"]]))
    
    # Create a named vector for the Tukey groups
    tukey_groups <- setNames(test2_inter[["groups"]][["groups"]], rownames(test2_inter[["means"]]))
    
    # Create a named vector to store group summaries
    group_summaries_inter <- setNames(rep(NA, length(interactions)), interactions)
    for (j in names(tukey_means)) {
      group_summaries_inter[j] <- paste0(signif(tukey_means[j],digits=4), " ", tukey_groups[j])
    }
    
    
    columnname <- colnames(aov_data)[i]
    summary_table[ , columnname] <- 0                  # Append new column
    summary_table[ , columnname] <- c(
      group_summaries_1,
      paste0(signif(summary(t.var1.aov)[[1]][1,5],digits=4), " ", summary(t.var1.aov)[[1]][1,5] %>% stars.pval),
      group_summaries_2,
      paste0(signif(summary(t.var2.aov)[[1]][1,5],digits=4), " ", summary(t.var2.aov)[[1]][1,5] %>% stars.pval),
      group_summaries_inter,
      paste0(signif(summary(t.inter.aov)[[1]][1,5],digits=4), " ", summary(t.inter.aov)[[1]][1,5] %>% stars.pval)
    )
  }
>>>>>>> 78de140e8b07677a9bb7200299ca7501ec55197b
  }
  
  # Create a data frame from the list
  # summary_table <- do.call(cbind, summary_list)
  # rownames(summary_table) <- c(row_names, "P-value", "F-value", "Significant")
  summary_table <- as.data.frame(do.call(cbind, summary_list), stringsAsFactors = FALSE)
  rownames(summary_table) <- c(row_names, "P-value", "F-value", "Significant")
  
  
  # Write to Excel only if output_file is specified
  if (!is.null(output_file)) {
    write.xlsx(summary_table, output_file, rowNames = TRUE)
  }
  
  # If return_raw is TRUE, assign raw outputs to the specified object name in the global environment
  if (return_raw) {
    assign(output_name, raw_outputs, envir = .GlobalEnv)
  }
  
  # Return the summary table
  return(summary_table)
}

