# Some R AoV summary functions
# https://github.com/GavinDuley/PenguinoidRTools
# Copyright (c) 2024 onwards, Gavin Duley
# Licenced under the GPL-3.0 licence

# aovSummaryTable -------------------------------------------------------------

#' AoV summary table
#'
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
  # Create a summary table with the group variable as the first column
  summary_table <- as.data.frame(c(levels(as.factor(aov_data[[group_var]])),
                                   "P-value",
                                   "F-value", 
                                   "Significant")) %>% setNames(group_var)

  # Identify the indices of factor columns
  factor_indices <- which(sapply(aov_data, is.factor))
  
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
      
      # Create a vector to store group summaries
      group_summaries <- c()
      for (j in 1:length(test2[["groups"]][[columnname]])) {
        group_summaries <- c(group_summaries, paste0(signif(test2$groups[j,1],digits=4), " ", test2$groups[j,2]))
      }
      
      summary_table <- cbind(summary_table, c(
        group_summaries,
        paste0(signif(summary(t.anova)[[1]][1,5],digits=4), " ", summary(t.anova)[[1]][1,5] %>% stars.pval),
        signif(summary(t.anova)[[1]][1,4],digits=4),
        if(summary(t.anova)[[1]][1,5] <= 0.05){
          print("SIGNIFICANT")
        } else {
          print("NOT SIGNIFICANT")
        }
      ))
      colnames(summary_table)[ncol(summary_table)] <- columnname
    }
  }
  # Write to Excel only if output_file is specified
  if (!is.null(output_file)) {
    write.xlsx(summary_table, output_file, rowNames=F)
  }
  
  # Always return the summary_table dataframe
  return(summary_table)
}

# aovInteractSummaryTable -----------------------------------------------------

#' AoV summary table with interaction
#'
#' @description This allows you to generate a table of means and key statistical
#'  values from an analysis of variance (AoV) test for a whole dataframe,
#'  with interactions between two factors. 
#'  It uses the agricolae, dplyr, gtools, and openxlsx packages.\cr
#'  Please convert any non-numeric columns to factors before using this function.
#'  Ideally, factor columns should be the first few columns in the data frame.\cr
#'
#' @param aov_data The data frame containing the data to be analysed.
#' @param group_var_1 The name of the column containing the first group names (in quotes).
#' @param group_var_2 The name of the column containing the second group names (in quotes).
#' @param interaction_funct The interaction function to use. Defaults to "*".
#' @param output_file If a file name is supplied (in quotes), it will output an Excel file with the summary table. The filename should have an .xlsx extension.
#' @return Returns a summary table and (optionally) saves it as an Excel file in the active directory.
#' @examples aovSummaryTable(aov_data, "Group1","*","Group2", "stats_summary.xlsx")
#' @importFrom dplyr %>%
#' @importFrom stats aov lm
#' @importFrom agricolae HSD.test
#' @importFrom openxlsx write.xlsx
#' @importFrom gtools stars.pval
#' @importFrom snakecase to_any_case
#' @export


aovInteractSummaryTable <- function(aov_data, 
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
  # Identify the indices of factor columns
  factor_indices <- which(sapply(aov_data, is.factor))
  
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
  }
  # Write to Excel only if output_file is specified
  if (!is.null(output_file)) {
    write.xlsx(summary_table, output_file, rowNames=F)
  }
  
  # Always return the summary_table dataframe
  return(summary_table)
}
