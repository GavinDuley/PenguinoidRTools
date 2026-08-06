# Test file for CIELab colour functions
# Uses dummy wine transmission spectroscopy data for typical red, white, and rosé wines

library(testthat)
library(PenguinoidRTools)

# ==============================================================================
# Helper function to generate wine transmission spectra
# ==============================================================================

# Generate realistic wine transmission data (long format)
# Red wine: low transmission in blue/green, moderate in red
# White wine: high transmission across spectrum
# Rosé wine: intermediate between red and white
generate_wine_spectra <- function() {
  wavelengths <- seq(380, 780, by = 5)  # 5nm intervals as per OIV method

  # Red wine transmission profile (fraction 0-1)
  # Very low in blue/green, gradually increasing toward red
  red_T <- sapply(wavelengths, function(wl) {
    if (wl < 500) return(0.01 + 0.02 * (wl - 380) / 120)  # Very low in blue
    if (wl < 600) return(0.03 + 0.15 * (wl - 500) / 100)  # Increasing in green
    return(0.18 + 0.25 * (wl - 600) / 180)                # Higher in red
  })

  # White wine transmission profile
  # High transmission overall with slight blue absorption
  white_T <- sapply(wavelengths, function(wl) {
    if (wl < 450) return(0.70 + 0.10 * (wl - 380) / 70)   # Slight absorption in blue
    return(0.80 + 0.10 * sin((wl - 450) * pi / 660))      # High throughout
  })


  # Rosé wine transmission profile
  # Intermediate between red and white
  rose_T <- sapply(wavelengths, function(wl) {
    if (wl < 500) return(0.30 + 0.15 * (wl - 380) / 120)  # Moderate absorption in blue
    if (wl < 600) return(0.45 + 0.15 * (wl - 500) / 100)  # Increasing
    return(0.60 + 0.15 * (wl - 600) / 180)                # High in red
  })

  # Combine into long format data frame
  data.frame(
    WineName = rep(c("RedWine", "WhiteWine", "RoseWine"), each = length(wavelengths)),
    lambda = rep(wavelengths, 3),
    T = c(red_T, white_T, rose_T),
    stringsAsFactors = FALSE
  )
}

# ==============================================================================
# Tests for cielab_spectroscopy_percentage_to_fraction
# ==============================================================================

test_that("cielab_spectroscopy_percentage_to_fraction converts correctly", {
  # Single value
  expect_equal(cielab_spectroscopy_percentage_to_fraction(50), 0.5)
  expect_equal(cielab_spectroscopy_percentage_to_fraction(100), 1.0)
  expect_equal(cielab_spectroscopy_percentage_to_fraction(0), 0.0)

  # Vector of values
  result <- cielab_spectroscopy_percentage_to_fraction(c(10, 50, 90))
  expect_equal(result, c(0.1, 0.5, 0.9))
})

test_that("cielab_spectroscopy_percentage_to_fraction warns on invalid values", {
  # Negative values warning
  expect_warning(
    cielab_spectroscopy_percentage_to_fraction(-5),
    "Negative transmission values"
  )

  # Values > 100 warning
  expect_warning(
    cielab_spectroscopy_percentage_to_fraction(105),
    "Transmission values > 100%"
  )
})

# ==============================================================================
# Tests for lab_to_C (chroma calculation)
# ==============================================================================

test_that("lab_to_C calculates chroma correctly", {
  # 3-4-5 triangle
  expect_equal(lab_to_C(3, 4), 5)

  # Known values
  expect_equal(lab_to_C(30, 40), 50)
  expect_equal(lab_to_C(0, 0), 0)

  # Vectorized
  result <- lab_to_C(c(3, 0, 6), c(4, 5, 8))
  expect_equal(result, c(5, 5, 10))
})

# ==============================================================================
# Tests for hue_deg (hue angle calculation)
# ==============================================================================

test_that("hue_deg calculates hue angle correctly", {
  # Cardinal directions
  expect_equal(hue_deg(1, 0), 0)     # Pure +a (red)
  expect_equal(hue_deg(0, 1), 90)    # Pure +b (yellow)
  expect_equal(hue_deg(-1, 0), 180)  # Pure -a (green)
  expect_equal(hue_deg(0, -1), 270)  # Pure -b (blue)

  # 45 degree increments
  expect_equal(hue_deg(1, 1), 45)
  expect_equal(hue_deg(-1, 1), 135)
  expect_equal(hue_deg(-1, -1), 225)
  expect_equal(hue_deg(1, -1), 315)
})

# ==============================================================================
# Tests for delta_h_signed (signed hue difference)
# ==============================================================================

test_that("delta_h_signed handles wraparound correctly", {
  # Simple difference
  expect_equal(delta_h_signed(45, 30), 15)
  expect_equal(delta_h_signed(30, 45), -15)

  # Crossing 0/360 boundary
  expect_equal(delta_h_signed(10, 350), 20)
  expect_equal(delta_h_signed(350, 10), -20)

  # Large differences
  expect_equal(delta_h_signed(270, 90), 180)
})

# ==============================================================================
# Tests for deltaE76 (colour difference)
# ==============================================================================

test_that("deltaE76 calculates colour difference correctly", {
  # Identical colours
  expect_equal(deltaE76(50, 0, 0, 50, 0, 0), 0)

  # Simple differences
  expect_equal(deltaE76(50, 0, 0, 53, 4, 0), 5)  # 3-4-0 triangle gives sqrt(25)=5

  # Typical red vs white wine difference (should be large)
  dE_red_white <- deltaE76(30, 50, 30, 95, -2, 15)
  expect_true(dE_red_white > 50)  # Very different colours

  # Vectorized operation
  L1 <- c(50, 50)
  a1 <- c(0, 0)
  b1 <- c(0, 0)
  L2 <- c(50, 60)
  a2 <- c(3, 0)
  b2 <- c(4, 0)
  result <- deltaE76(L1, a1, b1, L2, a2, b2)
  expect_equal(result, c(5, 10))
})

# ==============================================================================
# Tests for cielab_from_spectrum (main conversion function)
# ==============================================================================

test_that("cielab_from_spectrum works with long format wine data", {
  wine_data <- generate_wine_spectra()

  result <- cielab_from_spectrum(wine_data, sample_col = "WineName")

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_true(all(c("WineName", "CIELab_L", "CIELab_a", "CIELab_b",
                    "CIELab_C", "CIELab_H") %in% names(result)))

  # Check wine names are preserved
  expect_true(all(c("RedWine", "WhiteWine", "RoseWine") %in% result$WineName))
})

test_that("cielab_from_spectrum produces reasonable wine colour values", {
  wine_data <- generate_wine_spectra()
  result <- cielab_from_spectrum(wine_data, sample_col = "WineName")

  red <- result[result$WineName == "RedWine", ]
  white <- result[result$WineName == "WhiteWine", ]
  rose <- result[result$WineName == "RoseWine", ]

  # Red wine should be darker than white
  expect_true(red$CIELab_L < white$CIELab_L)

  # Rosé should be intermediate
  expect_true(rose$CIELab_L > red$CIELab_L)
  expect_true(rose$CIELab_L < white$CIELab_L)

  # Red wine should have positive a* (red-shifted)
  expect_true(red$CIELab_a > 0)

  # White wine should have low chroma (less saturated)
  expect_true(white$CIELab_C < red$CIELab_C)

  # All values should be in valid ranges
  expect_true(all(result$CIELab_L >= 0 & result$CIELab_L <= 100))
  expect_true(all(result$CIELab_H >= 0 & result$CIELab_H <= 360))
  expect_true(all(result$CIELab_C >= 0))
})

test_that("cielab_from_spectrum works with wide format data", {
  # Create wide format test data (sparse for testing - full range needed)
  wavelengths <- seq(380, 780, by = 5)
  wave_cols <- paste0("X", wavelengths)

  # Create a simple white wine-like spectrum
  white_T <- rep(0.85, length(wavelengths))

  wide_data <- data.frame(
    WineName = "TestWhite",
    stringsAsFactors = FALSE
  )
  for (i in seq_along(wave_cols)) {
    wide_data[[wave_cols[i]]] <- white_T[i]
  }

  result <- cielab_from_spectrum(wide_data)

  # Check it produces output
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$WineName, "TestWhite")

  # High transmission should give high L*
  expect_true(result$CIELab_L > 80)
})

test_that("cielab_from_spectrum handles path length correction", {
  wine_data <- generate_wine_spectra()

  # 10mm path length (default)
  result_10mm <- cielab_from_spectrum(wine_data, path_mm = 10)

  # 5mm path length - when claiming data was measured at 5mm, the function
  # scales absorbance UP to 10mm equivalent, resulting in lower L* (darker)
  result_5mm <- cielab_from_spectrum(wine_data, path_mm = 5)

  # Red wine should appear darker when path length correction scales up absorbance
  red_10mm <- result_10mm[result_10mm$WineName == "RedWine", "CIELab_L"]
  red_5mm <- result_5mm[result_5mm$WineName == "RedWine", "CIELab_L"]

  expect_true(red_5mm < red_10mm)
})

test_that("cielab_from_spectrum auto-detects sample column",
{
  wine_data <- generate_wine_spectra()

  # Should auto-detect WineName
  result <- cielab_from_spectrum(wine_data)
  expect_true("WineName" %in% names(result))
})

test_that("cielab_from_spectrum errors on invalid data format", {
  bad_data <- data.frame(
    Name = "Test",
    Value = 42
  )

  expect_error(
    cielab_from_spectrum(bad_data),
    "Cannot detect data format"
  )
})

test_that("cielab_from_spectrum keeps only OIV 5 nm grid readings, with a message", {
  wine_data <- generate_wine_spectra()
  result_5nm <- cielab_from_spectrum(wine_data, sample_col = "WineName")

  # 1 nm resolution data: same values on the grid, linear interpolation between.
  # Off-grid readings must be discarded (with a message), giving identical results.
  fine_data <- do.call(rbind, lapply(unique(wine_data$WineName), function(w) {
    sub <- wine_data[wine_data$WineName == w, ]
    fine_lambda <- seq(380, 780, by = 1)
    data.frame(
      WineName = w,
      lambda = fine_lambda,
      T = approx(sub$lambda, sub$T, xout = fine_lambda)$y,
      stringsAsFactors = FALSE
    )
  }))

  expect_message(
    result_1nm <- cielab_from_spectrum(fine_data, sample_col = "WineName"),
    "OIV 5 nm grid"
  )
  expect_equal(result_1nm, result_5nm)
})

test_that("cielab_from_spectrum averages replicate readings at the same wavelength", {
  wine_data <- generate_wine_spectra()
  red <- wine_data[wine_data$WineName == "RedWine", ]

  # Two replicates of the red wine spectrum, slightly offset, under one name
  reps <- rbind(
    transform(red, T = pmin(T * 1.05, 1)),
    transform(red, T = T * 0.95)
  )
  result_reps <- cielab_from_spectrum(reps, sample_col = "WineName")

  # Manually averaged spectrum should give the same result
  averaged <- aggregate(T ~ WineName + lambda, data = reps, FUN = mean)
  result_avg <- cielab_from_spectrum(averaged, sample_col = "WineName")

  expect_equal(result_reps, result_avg)
})

test_that("cielab_from_spectrum warns and returns NA when grid wavelengths are missing", {
  wine_data <- generate_wine_spectra()
  wine_data <- wine_data[wine_data$WineName == "RedWine", ]
  # Remove a mid-spectrum grid wavelength
  gappy <- wine_data[wine_data$lambda != 550, ]

  expect_warning(
    result <- cielab_from_spectrum(gappy, sample_col = "WineName"),
    "missing readings"
  )
  expect_true(all(is.na(result$CIELab_L)))
})

test_that("cielab_from_spectrum errors when no readings fall on the 5 nm grid", {
  off_grid <- data.frame(
    WineName = "Bad",
    lambda = seq(381, 779, by = 5),
    T = 0.8
  )
  expect_error(
    cielab_from_spectrum(off_grid, sample_col = "WineName"),
    "No readings fall on the OIV 5 nm wavelength grid"
  )
})

test_that("calc_colour_diff and calc_pairwise_dE validate the reference level", {
  lab_data <- data.frame(
    EtOH = rep(c("CTRL", "T1"), each = 3),
    Wine = rep(c("W1", "W2", "W1"), 2),
    CIELab_L = c(50, 52, 51, 60, 61, 59),
    CIELab_a = c(10, 11, 9, 15, 14, 16),
    CIELab_b = c(5, 6, 4, 8, 7, 9)
  )

  expect_error(
    calc_colour_diff(lab_data, reference_level = "MISSING"),
    "Reference level 'MISSING' not found"
  )
  expect_error(
    calc_pairwise_dE(lab_data, reference_level = "MISSING"),
    "Reference level 'MISSING' not found"
  )
  expect_error(
    calc_pairwise_dE(lab_data, target_levels = "NOPE"),
    "Target level\\(s\\) not found"
  )

  # Groups with no reference observations produce a warning rather than silent NAs
  no_ref_group <- rbind(
    cbind(lab_data, Grp = "A"),
    cbind(lab_data[lab_data$EtOH != "CTRL", ], Grp = "B")
  )
  expect_warning(
    calc_colour_diff(no_ref_group, group_by = "Grp"),
    "no 'CTRL' observations"
  )
})

test_that("cielab_from_spectrum warns on NA transmittance instead of silent NA", {
  wine_data <- generate_wine_spectra()
  wine_data <- wine_data[wine_data$WineName == "RedWine", ]
  # Wavelength present, but its only reading is NA
  wine_data$T[wine_data$lambda == 550] <- NA

  expect_warning(
    result <- cielab_from_spectrum(wine_data, sample_col = "WineName"),
    "NA transmittance"
  )
  expect_true(is.na(result$CIELab_L))
})

test_that("cielab_from_spectrum failure paths return numeric (not logical) NA", {
  wine_data <- generate_wine_spectra()
  gappy <- wine_data[wine_data$lambda != 550, ]

  result <- suppressWarnings(
    cielab_from_spectrum(gappy, sample_col = "WineName")
  )
  # All samples fail, but the CIELab columns must stay numeric so downstream
  # functions (colorspace, dplyr summaries) see doubles, not logicals
  expect_true(all(is.na(result$CIELab_L)))
  for (col in c("CIELab_L", "CIELab_a", "CIELab_b", "CIELab_C", "CIELab_H")) {
    expect_type(result[[col]], "double")
  }
})

test_that("cielab_from_spectrum summarises how many samples failed", {
  wine_data <- generate_wine_spectra()

  # All samples missing a grid wavelength -> loud "All ... samples" summary
  gappy_all <- wine_data[wine_data$lambda != 550, ]
  w <- capture_warnings(cielab_from_spectrum(gappy_all, sample_col = "WineName"))
  expect_true(any(grepl("All 3 samples returned NA", w)))

  # One sample missing a wavelength -> "1 of 3 samples" summary
  gappy_one <- wine_data[!(wine_data$WineName == "RedWine" &
                             wine_data$lambda == 550), ]
  w <- capture_warnings(cielab_from_spectrum(gappy_one, sample_col = "WineName"))
  expect_true(any(grepl("1 of 3 samples returned NA", w)))
})

test_that("cielab_to_hex handles NA coordinates without crashing", {
  skip_if_not_installed("colorspace")

  # Logical NA (what an all-failed cielab_from_spectrum result used to give)
  expect_true(is.na(cielab_to_hex(NA, NA, NA)))

  # Mixed valid / NA values
  result <- cielab_to_hex(c(50, NA), c(10, NA), c(20, NA))
  expect_false(is.na(result[1]))
  expect_true(is.na(result[2]))

  # Non-numeric input gets a clear error
  expect_error(cielab_to_hex("fifty", 10, 20), "must be numeric")
})

test_that("cielab_swatch explains NA CIELab values instead of CheckMatrix error", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("colorspace")

  all_na <- data.frame(
    WineName = c("A", "B"),
    CIELab_L = NA, CIELab_a = NA, CIELab_b = NA
  )
  expect_error(
    cielab_swatch(all_na, file.path(tempdir(), "na_swatch.png")),
    "All rows have NA CIELab values"
  )

  some_na <- data.frame(
    WineName = c("Good", "Bad"),
    CIELab_L = c(50, NA),
    CIELab_a = c(10, NA),
    CIELab_b = c(20, NA)
  )
  out_png <- file.path(tempdir(), "partial_swatch.png")
  expect_warning(
    cielab_swatch(some_na, out_png),
    "Dropping 1 row\\(s\\) with NA CIELab values: Bad"
  )
  expect_true(file.exists(out_png))
  unlink(out_png)
})

test_that("colour difference functions drop NA rows with a warning", {
  lab_data <- data.frame(
    EtOH = rep(c("CTRL", "T1"), each = 3),
    CIELab_L = c(50, 52, NA, 60, 61, 59),
    CIELab_a = c(10, 11, NA, 15, 14, 16),
    CIELab_b = c(5, 6, NA, 8, 7, 9)
  )

  expect_warning(
    diffs <- calc_colour_diff(lab_data),
    "Dropping 1 of 6"
  )
  expect_false(any(is.na(diffs$dE76_cent)))

  expect_warning(
    pw <- calc_pairwise_dE(lab_data),
    "Dropping 1 of 6"
  )
  expect_false(any(is.na(pw$dE_mean)))

  # All-NA data gets an informative error, not an all-NA result
  all_na <- lab_data
  all_na[c("CIELab_L", "CIELab_a", "CIELab_b")] <- NA
  expect_error(
    calc_colour_diff(all_na),
    "All rows have NA CIELab values"
  )
  expect_error(
    calc_pairwise_dE(all_na),
    "All rows have NA CIELab values"
  )
})

test_that("full pipeline handles a 5 nm instrument scan end to end", {
  # Simulates a spectrophotometer set to a 5 nm interval over 300-800 nm:
  # 13 header lines, WL.nm. / X.T columns, percent transmission. This is the
  # scan setting the OIV grid requires, so it must work with no warnings.
  scan_dir <- file.path(tempdir(), "scan5nm_test")
  dir.create(scan_dir, showWarnings = FALSE)
  on.exit(unlink(scan_dir, recursive = TRUE), add = TRUE)

  wl <- seq(300, 800, by = 5)
  for (nm in c("Wine_A_1", "Wine_B_1")) {
    trans_pct <- 100 * pmin(1, 0.05 + 0.9 * (wl - 300) / 500)
    writeLines(
      c(rep("instrument header", 13), "WL.nm.,X.T",
        paste(wl, round(trans_pct, 3), sep = ",")),
      file.path(scan_dir, paste0(nm, ".csv"))
    )
  }

  suppressMessages(spectra <- process_spectrum(scan_dir, skip = 13))
  expect_equal(sort(unique(spectra$sample_name)), c("Wine_A_1", "Wine_B_1"))

  expect_no_warning(
    result <- cielab_from_spectrum(spectra, sample_col = "sample_name")
  )
  expect_equal(nrow(result), 2)
  expect_false(any(is.na(result$CIELab_L)))
  expect_true(all(result$CIELab_L >= 0 & result$CIELab_L <= 100))
})

test_that("cielab_from_spectrum warns on percentage values", {
  wine_data <- generate_wine_spectra()
  # Convert to percentages (0-100)
  wine_data$T <- wine_data$T * 100

  expect_warning(
    cielab_from_spectrum(wine_data),
    "Transmittance values significantly > 1"
  )
})
