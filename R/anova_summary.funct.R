# Some R AoV summary functions
# https://github.com/GavinDuley/PenguinoidUtils
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
#' @param output_file The name of the output file (in quotes). Defaults to "stats_summary.xlsx".
#' @return Returns a summary table and saves it as an Excel file in the active directory.
#' @examples aovSummaryTable(aov_data, "Group", "stats_summary.xlsx")
#' @export
aovSummaryTable <- function(aov_data, 
                            group_var, 
                            output_file = "stats_summary.xlsx") {
  # # List of required packages
  # packages <- c("agricolae", "dplyr", "gtools", "openxlsx")
  # # Check if packages are installed
  # for(pkg in packages){
  #   if(!require(pkg, character.only = TRUE)){
  #     install.packages(pkg)
  #     library(pkg, character.only = TRUE)
  #   }
  # }
  summary_table <- as.data.frame(c(levels(as.factor(aov_data[[group_var]])),
                                   "P-value",
                                   "F-value", 
                                   "Significant")) %>% setNames("Group")
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
          print("SIGNIFICANT")
        } else {
          print("NOT SIGNIFICANT")
        }
      ))
      colnames(summary_table)[ncol(summary_table)] <- columnname
    }
  }
  write.xlsx(summary_table, output_file, rowNames=F)
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
#' @param output_file The name of the output file (in quotes). Defaults to "stats_summary.xlsx".
#' @return Returns a summary table and saves it as an Excel file in the active directory.
#' @examples aovSummaryTable(aov_data, "Group", "stats_summary.xlsx")
#' @export


aovInteractSummaryTable <- function(aov_data, 
                                    group_var_1, 
                                    group_var_2,
                                    interaction_funct = "*",
                                    output_file = "stats_interact_summary.xlsx") { 
  summary_table <- as.data.frame(c(levels(as.factor(aov_data[[group_var_1]])),
                                   levels(as.factor(aov_data[[group_var_2]])),
                                   paste0(group_var_1, " p value"),
                                   paste0(group_var_2, " p value"),
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
    
    # test2_inter <- as.formula(paste(colnames(aov_data)[i], paste0("~ ", group_var_1,
    #                                                         interaction_funct,
    #                                                         group_var_2))) %>%
    #   lm(data = aov_data) %>% 
    #   HSD.test(group_var_1, interaction_funct, group_var_2)
    
    columnname <- colnames(aov_data)[i]
    
    # Create a vector to store group summaries
    group_summaries_1 <- c()
    for (j in 1:length(test2_var1[["means"]][[columnname]])) {
      group_summaries_1 <- c(group_summaries_1, paste0(signif(test2_var1[["means"]][[columnname]][j],digits=4), " ", test2_var1[["groups"]][["groups"]][j]))
    }
    
    group_summaries_2 <- c()
    for (j in 1:length(test2_var2[["means"]][[columnname]])) {
      group_summaries_2 <- c(group_summaries_2, paste0(signif(test2_var2[["means"]][[columnname]][j],digits=4), " ", test2_var2[["groups"]][["groups"]][j]))
    }
    
        
    columnname <- colnames(aov_data)[i]
    summary_table[ , columnname] <- 0                  # Append new column
    summary_table[ , columnname] <- c(
      group_summaries_1,
      group_summaries_2,
      paste0(signif(summary(t.var1.aov)[[1]][1,5],digits=4), " ", summary(t.var1.aov)[[1]][1,5] %>% stars.pval),
      paste0(signif(summary(t.var2.aov)[[1]][1,5],digits=4), " ", summary(t.var2.aov)[[1]][1,5] %>% stars.pval),
      paste0(signif(summary(t.inter.aov)[[1]][1,5],digits=4), " ", summary(t.inter.aov)[[1]][1,5] %>% stars.pval)
    )
  }
  }
  write.xlsx(summary_table,output_file,rowNames=F,colNames=T)
}
