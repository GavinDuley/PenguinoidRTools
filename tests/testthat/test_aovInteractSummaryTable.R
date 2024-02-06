# Load libraries
library(PenguinoidRTools)
library(dplyr)

# Fake data thanks to ChatGPT
# Define the levels for the independent variables
regions <- c("Coteau de Vignes", "Terrasse Graveleuse", "Île au Vin", "Vallée des Vins")
cultivars <- c("Syrah", "Grenache", "Mourvèdre", "Cabernet Sauvignon", "Merlot", "Malbec")

# Define the names for the dependent variables
compounds <- c("Resveratrol", "Quercetin", "Epicatechin", "Catechin", "Procyanidin B1", 
               "Procyanidin B2", "Procyanidin B3", "Procyanidin B4", "Procyanidin C1", 
               "Caffeic acid", "Coutaric acid", "Caftaric acid", "Ferulic acid", 
               "Gallic acid", "p-Coumaric acid")

# Create a data frame with all combinations of regions and cultivars, each repeated twice
region_cultivar_combinations <- expand.grid(Règion=regions, Cultivarδ=cultivars)
aov_data <- region_cultivar_combinations[rep(seq_len(nrow(region_cultivar_combinations)), each = 2), ]

# Assign a unique wine name to each row and set it as the row name
rownames(aov_data) <- paste0("FAKEWINE", seq_len(nrow(aov_data)))

# For each compound, generate values based on the region and cultivar and add them as a new column to the data frame
set.seed(123)  # for reproducibility
for (compound in compounds) {
  # Generate values based on the region and cultivar
  values <- with(aov_data, ifelse(Règion == "Coteau de Vignes" & Cultivarδ %in% c("Syrah", "Grenache", "Mourvèdre"),
                                  rnorm(nrow(aov_data), mean = 10, sd = 2),
                                  ifelse(Règion == "Terrasse Graveleuse" & Cultivarδ %in% c("Cabernet Sauvignon", "Merlot", "Malbec"),
                                         rnorm(nrow(aov_data), mean = 15, sd = 2),
                                         rnorm(nrow(aov_data), mean = 5, sd = 2))))
  
  # Add the values as a new column to the data frame
  aov_data[[compound]] <- values
}

# Print the first few rows of the data frame
head(aov_data)

aov_data$Règion <- as.factor(aov_data$Règion)
aov_data$Cultivarδ <- as.factor(aov_data$Cultivarδ)

# Call your function with the test data
result <- aovInteractSummaryTable(aov_data,"Règion","*","Cultivarδ")
# Add assertions here to check that the result is what you expect.
expect_equal(dim(result), c(22, 3))

# Call your function with the test data but different interaction
result2 <- aovInteractSummaryTable(greenhouse$greenhouse1,"variety","+","method")
# Add assertions here to check that the result is what you expect.
expect_equal(dim(result2), c(10, 3))
