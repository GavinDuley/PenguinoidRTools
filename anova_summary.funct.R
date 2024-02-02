# Some R AoV summary functions
# https://github.com/GavinDuley/PenguinoidsR
# Copyright (c) 2024 onwards, Gavin Duley
# Licenced under the GPL-3.0 licence

# aovSummaryTable -------------------------------------------------------------

# How to run this function:
# aovSummaryTable(aov_data, "Group", "stats_summary.xlsx")
# Where aov_data is the data frame containing the data to be analysed, 
# "Group" is the name of the column containing the group names, 
# and "stats_summary.xlsx" is the name of the output file.
aovSummaryTable <- function(aov_data, group_var, output_file = "stats_summary.xlsx") {
  # List of required packages
  packages <- c("agricolae", "dplyr", "gtools", "openxlsx")
  
  # Check if packages are installed
  for(pkg in packages){
    if(!require(pkg, character.only = TRUE)){
      install.packages(pkg)
      library(pkg, character.only = TRUE)
    }
  }
  
  summary_table <- as.data.frame(c(levels(as.factor(aov_data[[group_var]])),"P-value","F-value", "Significant")) %>% setNames("Group")
  for (i in 4:ncol(aov_data)){
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

