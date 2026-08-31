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

  result <- suppressWarnings(
    aovInteractSummaryTable(greenhouse1_data, c("variety", "method"))
  )

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

  expect_no_error(suppressWarnings({
    result <- aovInteractSummaryTable(greenhouse1_data, c("variety", "method"))
  }))

  result <- suppressWarnings(
    aovInteractSummaryTable(greenhouse1_data, c("variety", "method"))
  )
  expect_true("single_factor_col" %in% colnames(result))
  expect_true(all(result[["single_factor_col"]] == "INVARIANT"))
})

test_that("aovSummaryTable accepts character grouping and ignores other character columns", {
  greenhouse1_data <- greenhouse$greenhouse1
  greenhouse1_data$variety <- as.character(greenhouse1_data$variety)
  greenhouse1_data$notes <- "free text"

  result <- aovSummaryTable(greenhouse1_data, group_var = "variety")

  expect_s3_class(result, "data.frame")
  expect_true(all(c("tubers", "weight") %in% colnames(result)))
  expect_false("notes" %in% colnames(result))
})

test_that("aovInteractSummaryTable accepts character grouping variables", {
  greenhouse1_data <- greenhouse$greenhouse1
  greenhouse1_data$variety <- as.character(greenhouse1_data$variety)
  greenhouse1_data$method <- as.character(greenhouse1_data$method)

  result <- aovInteractSummaryTable(greenhouse1_data, c("variety", "method"))

  expect_s3_class(result, "data.frame")
  expect_true(all(c("tubers", "weight") %in% colnames(result)))
})

test_that("aovInteractSummaryTable applies BH correction within each effect family", {
  greenhouse1_data <- greenhouse$greenhouse1

  result <- aovInteractSummaryTable(greenhouse1_data, c("variety", "method"))

  for (effect in c("variety", "method", "variety:method")) {
    raw_p <- sapply(c("tubers", "weight"), function(v) {
      s <- summary(aov(as.formula(paste(v, "~ variety * method")),
                       data = greenhouse1_data))[[1]]
      rownames(s) <- trimws(rownames(s))
      s[effect, "Pr(>F)"]
    })
    expected <- p.adjust(raw_p, method = "BH")
    bh_row <- result[result$Type == paste0("BH-Corrected-P-value-", effect), ]
    for (v in c("tubers", "weight")) {
      expect_true(startsWith(bh_row[[v]], as.character(signif(expected[v], 4))))
    }
  }
})

test_that("aovSummaryTable handles columns present for only one factor level without error", {
  greenhouse1_data <- greenhouse$greenhouse1
  first_variety <- levels(as.factor(greenhouse1_data$variety))[1]
  greenhouse1_data$single_factor_col <- ifelse(
    greenhouse1_data$variety == first_variety,
    runif(nrow(greenhouse1_data)),
    NA_real_
  )

  expect_no_error(suppressWarnings({
    result <- aovSummaryTable(greenhouse1_data, group_var = "variety")
  }))

  result <- suppressWarnings(
    aovSummaryTable(greenhouse1_data, group_var = "variety")
  )
  expect_true("single_factor_col" %in% colnames(result))
  expect_true(all(result[["single_factor_col"]] == "invariant"))
})

test_that("aovSummaryTable computes group means over observed values when NAs are present", {
  na_data <- data.frame(
    EtOH = c("CTRL", "CTRL", "CTRL", "9PC", "9PC", "9PC"),
    Dp_gluc = c(18.15, 14.89, 12.55, NA, 13.47, 10.25)
  )

  result <- aovSummaryTable(na_data, group_var = "EtOH")

  # Means must be over the observed values only (NA is "not measured",
  # not zero): mean(13.47, 10.25) = 11.86, not mean(0, 13.47, 10.25).
  expect_true(startsWith(result[result$Type == "9PC", "Dp_gluc"],
                         as.character(signif(mean(c(13.47, 10.25)), 4))))
  expect_true(startsWith(result[result$Type == "CTRL", "Dp_gluc"],
                         as.character(signif(mean(c(18.15, 14.89, 12.55)), 4))))

  # P- and F-values must match a direct aov() fit, which drops NA rows.
  direct <- summary(aov(Dp_gluc ~ EtOH, data = na_data))[[1]]
  expect_true(startsWith(result[result$Type == "F-value", "Dp_gluc"],
                         as.character(signif(direct[1, "F value"], 4))))
})

test_that("aovSummaryTable treats constant-except-NA and all-NA columns as invariant", {
  na_data <- data.frame(
    EtOH = c("CTRL", "CTRL", "CTRL", "9PC", "9PC", "9PC"),
    Dp_gluc = c(18.15, 14.89, 12.55, NA, 13.47, 10.25),
    const_na = c(5, 5, 5, NA, 5, 5),
    all_na = NA_real_
  )

  result <- suppressWarnings(aovSummaryTable(na_data, group_var = "EtOH"))

  expect_true(all(result[["const_na"]] == "invariant"))
  expect_true(all(result[["all_na"]] == "invariant"))
})

test_that("aovSummaryTable drops NA rows even if the session na.action is na.fail", {
  na_data <- data.frame(
    EtOH = c("CTRL", "CTRL", "CTRL", "9PC", "9PC", "9PC"),
    Dp_gluc = c(18.15, 14.89, 12.55, NA, 13.47, 10.25)
  )

  old_opt <- options(na.action = "na.fail")
  on.exit(options(old_opt), add = TRUE)

  expect_no_error(aovSummaryTable(na_data, group_var = "EtOH"))
})

test_that("aovInteractSummaryTable computes means over observed values when NAs are present", {
  greenhouse1_data <- greenhouse$greenhouse1
  greenhouse1_data$tubers_na <- greenhouse1_data$tubers
  greenhouse1_data$tubers_na[1] <- NA_real_
  greenhouse1_data$const_na <- 42
  greenhouse1_data$const_na[2] <- NA_real_

  result <- suppressWarnings(
    aovInteractSummaryTable(greenhouse1_data, c("variety", "method"))
  )

  # The treatment combination that lost an observation should show the mean
  # of the remaining observed values.
  combo <- paste(greenhouse1_data$variety[1], greenhouse1_data$method[1], sep = ":")
  all_combos <- paste(greenhouse1_data$variety, greenhouse1_data$method, sep = ":")
  observed <- greenhouse1_data$tubers_na[!is.na(greenhouse1_data$tubers_na) &
                                           all_combos == combo]
  expect_true(startsWith(result[result$Type == combo, "tubers_na"],
                         as.character(signif(mean(observed), 4))))

  # Constant-except-NA columns are invariant, and no NaN leaks into the table.
  expect_true(all(result[["const_na"]] == "INVARIANT"))
  expect_false(any(grepl("NaN", result[["tubers_na"]])))
})

test_that("aovSummaryTable skips columns with fewer than 2 residual df instead of crashing", {
  # HSD.test crashes with "missing value where TRUE/FALSE needed" when the
  # ANOVA has fewer than two residual degrees of freedom, because R's
  # qtukey/ptukey return NaN for df < 2. 'sparse0' has one observed value
  # per group (df = 0); 'sparse1' has three observations across two groups
  # (df = 1).
  na_data <- data.frame(
    EtOH = c("CTRL", "CTRL", "CTRL", "9PC", "9PC", "9PC"),
    Dp_gluc = c(18.15, 14.89, 12.55, NA, 13.47, 10.25),
    sparse0 = c(11.2, NA, NA, NA, 9.8, NA),
    sparse1 = c(6.14, NA, 5.34, NA, NA, 3.07)
  )

  expect_no_error(suppressWarnings(
    result <- aovSummaryTable(na_data, group_var = "EtOH")
  ))
  expect_true(all(result[["sparse0"]] == "insufficient data"))
  expect_true(all(result[["sparse1"]] == "insufficient data"))
  # The healthy column is still analysed as usual.
  expect_true(startsWith(result[result$Type == "9PC", "Dp_gluc"],
                         as.character(signif(mean(c(13.47, 10.25)), 4))))
})

test_that("aovInteractSummaryTable skips columns with fewer than 2 residual df instead of crashing", {
  greenhouse1_data <- greenhouse$greenhouse1
  combos <- paste(greenhouse1_data$variety, greenhouse1_data$method, sep = ":")
  # One observation per variety:method cell -> zero residual df.
  greenhouse1_data$sparse0 <- ifelse(duplicated(combos), NA_real_,
                                     greenhouse1_data$tubers)
  # One extra observation on top of that -> one residual df (qtukey NaN).
  greenhouse1_data$sparse1 <- greenhouse1_data$sparse0
  greenhouse1_data$sparse1[which(is.na(greenhouse1_data$sparse1))[1]] <-
    greenhouse1_data$tubers[which(is.na(greenhouse1_data$sparse0))[1]]

  expect_no_error(suppressWarnings({
    result <- aovInteractSummaryTable(greenhouse1_data, c("variety", "method"))
  }))
  expect_true(all(result[["sparse0"]] == "INSUFFICIENT DATA"))
  expect_true(all(result[["sparse1"]] == "INSUFFICIENT DATA"))
  # The healthy columns are still analysed as usual.
  expect_true(any(grepl("^P-value-variety$", result$Type)))
  expect_false(any(grepl("NaN", result[["tubers"]])))
})

test_that("aovSummaryTable warns with the names and reasons of skipped columns", {
  na_data <- data.frame(
    EtOH = c("CTRL", "CTRL", "CTRL", "9PC", "9PC", "9PC"),
    Dp_gluc = c(18.15, 14.89, 12.55, NA, 13.47, 10.25),
    Vitisin_A = c(NA, NA, NA, NA, NA, 3.11),
    Unassigned = c(4.72, NA, NA, NA, NA, 5.66)
  )

  expect_warning(
    aovSummaryTable(na_data, group_var = "EtOH"),
    "Vitisin_A \\(invariant\\), Unassigned \\(insufficient data\\)"
  )
})

test_that("aovInteractSummaryTable warns with the names and reasons of skipped columns", {
  greenhouse1_data <- greenhouse$greenhouse1
  greenhouse1_data$constant_col <- 42.0

  expect_warning(
    aovInteractSummaryTable(greenhouse1_data, c("variety", "method")),
    "constant_col \\(INVARIANT\\)"
  )
})

test_that("unexpected per-column failures are labelled and warned about, not fatal", {
  skip_if_not_installed("testthat", "3.1.7")
  local_mocked_bindings(HSD.test = function(...) stop("boom"),
                        .package = "agricolae")

  na_data <- data.frame(
    EtOH = c("CTRL", "CTRL", "CTRL", "9PC", "9PC", "9PC"),
    Dp_gluc = c(18.15, 14.89, 12.55, NA, 13.47, 10.25)
  )
  w <- capture_warnings(result <- aovSummaryTable(na_data, group_var = "EtOH"))
  expect_true(any(grepl("Column 'Dp_gluc' could not be analysed: boom", w)))
  expect_true(all(result[["Dp_gluc"]] == "not analysable"))

  greenhouse1_data <- greenhouse$greenhouse1
  w2 <- capture_warnings(
    result2 <- aovInteractSummaryTable(greenhouse1_data, c("variety", "method"))
  )
  expect_true(any(grepl("Column 'tubers' could not be analysed: boom", w2)))
  expect_true(any(grepl("tubers \\(NOT ANALYSABLE\\)", w2)))
})

