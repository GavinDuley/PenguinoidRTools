# Load the CO2 dataset from the datasets package
library(PenguinoidRTools)
library(dplyr)

# Fake data thanks to ChatGPT
# Define the regions, cultivars, winery names, and compounds
regions <- c("Coteau de Vignes", "Terrasse Graveleuse", "Île au Vin", "Vallée des Vins")
cultivars <- c("Syrah", "Grenache", "Mourvèdre", "Cabernet Sauvignon", "Merlot", "Malbec")
wineries <- c("Château de Faux", "Domaine du Pseudo", "Cave de l'Imitation", "Maison de l'Artifice")
compounds <- c("Resveratrol", "Quercetin", "Epicatechin", "Catechin", "Procyanidin B1", 
               "Procyanidin B2", "Procyanidin B3", "Procyanidin B4", "Procyanidin C1", 
               "Caffeic acid", "Coutaric acid", "Caftaric acid", "Ferulic acid", 
               "Gallic acid", "p-Coumaric acid")

# Create a data frame without wine names
wine_data <- expand.grid(Region = rep(regions, each=length(cultivars)*2),
                         Cultivar = rep(cultivars, times=length(regions)*2))
wine_data$Winery <- sample(wineries, nrow(wine_data), replace=TRUE)

# Limit to two wines per cultivar per region
wine_data <- wine_data[!duplicated(wine_data[, c("Region", "Cultivar")]), ]

# Add compound columns
set.seed(123)  # for reproducibility
for (compound in compounds) {
  wine_data[[compound]] <- rnorm(nrow(wine_data), mean=100, sd=15)
}

# Set wine names as row names
rownames(wine_data) <- paste0("FAKEWINE", 1:nrow(wine_data))

# Print the data frame
print(wine_data)


wine_data$Region <- as.factor(wine_data$Region)
wine_data$Cultivar <- as.factor(wine_data$Cultivar)
wine_data$Winery <- as.factor(wine_data$Winery)

wine_data<- janitor::clean_names(wine_data)

# Call your function with the test data
result <- aovInteractSummaryTable(wine_data,"region","*","cultivar")
# Add assertions here to check that the result is what you expect.
expect_equal(dim(result), c(37, 16))

# Call your function with the test data but different interaction
result2 <- aovInteractSummaryTable(greenhouse$greenhouse1,"variety","+","method")
# Add assertions here to check that the result is what you expect.
expect_equal(dim(result2), c(10, 3))
