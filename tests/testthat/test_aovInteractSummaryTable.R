# Load the CO2 dataset from the datasets package
library(datasets)
library(PenguinoidUtils)
library(testthat)
data(greenhouse)

# Call your function with the test data
result <- aovInteractSummaryTable(greenhouse$greenhouse1,"variety","*","method")
# Add assertions here to check that the result is what you expect.
expect_equal(dim(result), c(10, 3))

# Call your function with the test data but different interaction
result2 <- aovInteractSummaryTable(greenhouse$greenhouse1,"variety","+","method")
# Add assertions here to check that the result is what you expect.
expect_equal(dim(result2), c(10, 3))
