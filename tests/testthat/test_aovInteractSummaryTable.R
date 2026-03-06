library(agricolae)
library(PenguinoidRTools)
library(testthat)

# Load the correct dataset
data(greenhouse, package = "agricolae")

test_that("aovInteractSummaryTable produces a valid data frame with greenhouse data", {
  # Check if the dataset loaded properly
  expect_true(exists("greenhouse"))
  
  # Access the correct element within the dataset
  greenhouse1_data <- greenhouse$greenhouse1
  
  # Call your function with the test data
  result <- aovInteractSummaryTable(greenhouse1_data, c("variety", "method"))
  
  # Check if result is a data.frame
  expect_s3_class(result, "data.frame")
  
  # Ensure the result is not empty
  expect_true(nrow(result) > 0)
  expect_true(ncol(result) > 0)
  
  # Ensure it has expected columns (you can adjust these based on your function's output)
  expected_columns <- c("tubers", "weight")
  expect_true(all(expected_columns %in% colnames(result)))
  
  # Check that the content is character (or numeric if expected)
  expect_true(all(sapply(result, is.character) | sapply(result, is.numeric)))
})

test_that("aovInteractSummaryTable does not include Residuals rows", {
  greenhouse1_data <- greenhouse$greenhouse1

  result <- aovInteractSummaryTable(greenhouse1_data, c("variety", "method"))

  # No Type label should contain "Residuals"
  residual_rows <- grep("Residuals", result$Type, value = TRUE)
  expect_length(residual_rows, 0)
})

test_that("aovInteractSummaryTable handles constant-value (invariant) columns without error", {
  greenhouse1_data <- greenhouse$greenhouse1
  # Add a column that is completely constant
  greenhouse1_data$constant_col <- 42.0

  result <- aovInteractSummaryTable(greenhouse1_data, c("variety", "method"))

  expect_s3_class(result, "data.frame")
  expect_true("constant_col" %in% colnames(result))
  # Every cell in that column should be "INVARIANT"
  expect_true(all(result[["constant_col"]] == "INVARIANT"))
})

test_that("aovInteractSummaryTable handles columns present for only one factor level without error", {
  greenhouse1_data <- greenhouse$greenhouse1
  # Add a column with values only for the first variety level; NA for all others
  first_variety <- levels(as.factor(greenhouse1_data$variety))[1]
  greenhouse1_data$single_factor_col <- ifelse(
    greenhouse1_data$variety == first_variety,
    runif(nrow(greenhouse1_data)),
    NA_real_
  )

  expect_no_error({
    result <- aovInteractSummaryTable(greenhouse1_data, c("variety", "method"))
  })

  result <- aovInteractSummaryTable(greenhouse1_data, c("variety", "method"))
  expect_true("single_factor_col" %in% colnames(result))
  expect_true(all(result[["single_factor_col"]] == "INVARIANT"))
})

test_that("aovSummaryTable handles columns present for only one factor level without error", {
  greenhouse1_data <- greenhouse$greenhouse1
  first_variety <- levels(as.factor(greenhouse1_data$variety))[1]
  greenhouse1_data$single_factor_col <- ifelse(
    greenhouse1_data$variety == first_variety,
    runif(nrow(greenhouse1_data)),
    NA_real_
  )

  expect_no_error({
    result <- aovSummaryTable(greenhouse1_data, group_var = "variety")
  })

  result <- aovSummaryTable(greenhouse1_data, group_var = "variety")
  expect_true("single_factor_col" %in% colnames(result))
  expect_true(all(result[["single_factor_col"]] == "invariant"))
})

