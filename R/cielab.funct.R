# cielab.funct.R
# CIELab colour functions for PenguinoidRTools
# https://github.com/GavinDuley/PenguinoidRTools
# Copyright (c) 2024 onwards, Gavin Duley
# Licenced under the GPL-3.0 licence
#
# These functions implement CIELab colour calculations following the
# OIV-MA-AS2-11 (R2006) method using D65 illuminant / 10 degree observer.
#
# Dependencies: ggplot2, colorspace (for visualization functions)

# ==============================================================================
# OIV Method Coefficients (Table 1)
# ==============================================================================

# D65 / 10 degree observer coefficients (380-780 nm, 5 nm intervals)
.oiv_coefficients <- data.frame(
  lambda = seq(380, 780, by = 5),
  S  = c(50.0,52.3,54.6,68.7,82.8,87.1,91.5,92.5,93.4,90.1,86.7,95.8,104.9,110.9,117.0,117.4,117.8,116.3,114.9,115.4,115.9,112.4,108.8,109.1,109.4,108.6,107.8,106.3,104.8,106.2,107.7,106.0,104.4,104.2,104.0,102.0,100.0,98.2,96.3,96.1,95.8,92.2,88.7,89.3,90.0,89.8,89.6,88.6,87.7,85.5,83.3,83.5,83.7,81.9,80.0,80.1,80.2,81.2,82.3,80.3,78.3,74.0,69.7,70.7,71.6,73.0,74.3,68.0,61.6,65.7,69.9,72.5,75.1,69.3,63.6,55.0,46.4,56.6,66.8,65.1,63.4),
  x10 = c(0.0002,0.0007,0.0024,0.0072,0.0191,0.0434,0.0847,0.1406,0.2045,0.2647,0.3147,0.3577,0.3837,0.3867,0.3707,0.3430,0.3023,0.2541,0.1956,0.1323,0.0805,0.0411,0.0162,0.0051,0.0038,0.0154,0.0375,0.0714,0.1177,0.1730,0.2365,0.3042,0.3768,0.4516,0.5298,0.6161,0.7052,0.7938,0.8787,0.9512,1.0142,1.0743,1.1185,1.1343,1.1240,1.0891,1.0305,0.9507,0.8563,0.7549,0.6475,0.5351,0.4316,0.3437,0.2683,0.2043,0.1526,0.1122,0.0813,0.0579,0.0409,0.0286,0.0199,0.0138,0.0096,0.0066,0.0046,0.0031,0.0022,0.0015,0.0010,0.0007,0.0005,0.0004,0.0003,0.0002,0.0001,0.0001,0.0001,0.0000,0.0000),
  y10 = c(0.0000,0.0001,0.0003,0.0008,0.0020,0.0045,0.0088,0.0145,0.0214,0.0295,0.0387,0.0496,0.0621,0.0747,0.0895,0.1063,0.1282,0.1528,0.1852,0.2199,0.2536,0.2977,0.3391,0.3954,0.4608,0.5314,0.6067,0.6857,0.7618,0.8233,0.8752,0.9238,0.9620,0.9822,0.9918,0.9991,0.9973,0.9824,0.9556,0.9152,0.8689,0.8256,0.7774,0.7204,0.6583,0.5939,0.5280,0.4618,0.3981,0.3396,0.2835,0.2283,0.1798,0.1402,0.1076,0.0812,0.0603,0.0441,0.0318,0.0226,0.0159,0.0111,0.0077,0.0054,0.0037,0.0026,0.0018,0.0012,0.0008,0.0006,0.0004,0.0003,0.0002,0.0001,0.0001,0.0001,0.0000,0.0000,0.0000,0.0000,0.0000),
  z10 = c(0.0007,0.0029,0.0105,0.0323,0.0860,0.1971,0.3894,0.6568,0.9725,1.2825,1.5535,1.7985,1.9673,2.0273,1.9948,1.9007,1.7454,1.5549,1.3176,1.0302,0.7721,0.5701,0.4153,0.3024,0.2185,0.1592,0.1120,0.0822,0.0607,0.0431,0.0305,0.0206,0.0137,0.0079,0.0040,0.0011,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000)
)

# D65/10 degree reference white values
.D65_10_white <- list(Xn = 94.825, Yn = 100, Zn = 107.381)

# ==============================================================================
# Data Conversion Functions
# ==============================================================================

#' Convert spectroscopy transmission percentage to fraction
#'
#' Converts spectrophotometry transmission data from percentage (0-100) to
#' fraction (0-1) format required by cielab_from_spectrum().
#'
#' @param transmission_pct Numeric vector of transmission values as percentages
#'   (0-100 scale)
#' @return Numeric vector of transmission values as fractions (0-1 scale)
#'
#' @examples
#' # Convert 50% transmission to fraction
#' cielab_spectroscopy_percentage_to_fraction(50)
#' # Returns: 0.5
#'
#' # Convert a vector of percentages
#' cielab_spectroscopy_percentage_to_fraction(c(10, 50, 90))
#' # Returns: c(0.1, 0.5, 0.9)
#'
#' @export
cielab_spectroscopy_percentage_to_fraction <- function(transmission_pct) {
  if (any(transmission_pct < 0, na.rm = TRUE)) {
    warning("Negative transmission values detected; these are physically impossible.")
  }
  if (any(transmission_pct > 100, na.rm = TRUE)) {
    warning("Transmission values > 100% detected; check data for errors.")
  }
  transmission_pct / 100
}

# ==============================================================================
# Core CIELab Conversion Functions
# ==============================================================================

#' Convert transmittance to 10 mm path length equivalent
#'
#' Internal helper function to normalize transmittance measurements to the
#' standard 10 mm optical path length using Beer-Lambert law.
#'
#' @param T_meas Numeric vector of measured transmittance values (0-1)
#' @param path_mm Numeric path length in millimeters
#' @return Numeric vector of transmittance values normalized to 10 mm path
#'
#' @keywords internal
.to_T10 <- function(T_meas, path_mm) {
  A <- -log10(pmax(T_meas, .Machine$double.eps))
  A10 <- A * (10 / path_mm)
  10^(-A10)
}

#' Internal: Calculate CIELab for a single spectrum
#'
#' @param lambda Numeric vector of wavelengths (nm)
#' @param T Numeric vector of transmittance values (fraction 0-1)
#' @param path_mm Numeric path length in mm
#' @return List with L, a, b, C, H values
#' @keywords internal
.cielab_single <- function(lambda, T, path_mm) {
  # Express transmittance at 10 mm path length
  T10 <- .to_T10(T, path_mm)

  # Resample to the OIV method's 5 nm grid by averaging within +/- 2 nm
  grid <- .oiv_coefficients$lambda
  T10_on_grid <- vapply(grid, function(l) {
    idx <- which(lambda >= (l - 2) & lambda <= (l + 2))
    if (length(idx) == 0) return(NA_real_)
    mean(T10[idx], na.rm = TRUE)
  }, numeric(1))

  if (any(is.na(T10_on_grid))) {
    return(list(L = NA, a = NA, b = NA, C = NA, H = NA))
  }

  # Calculate tristimulus values
  df <- cbind(.oiv_coefficients, T10 = T10_on_grid)
  dlam <- 5  # nm interval

  # Normalization constant K (OIV Method Annex I)
  K <- 100 / sum(df$S * df$y10 * dlam)

  X <- K * sum(df$S * df$x10 * df$T10 * dlam)
  Y <- K * sum(df$S * df$y10 * df$T10 * dlam)
  Z <- K * sum(df$S * df$z10 * df$T10 * dlam)

  # Convert XYZ to Lab
  Xn <- .D65_10_white$Xn
  Yn <- .D65_10_white$Yn
  Zn <- .D65_10_white$Zn

  f <- function(t) {
    ifelse(t > 0.008856, t^(1/3), 7.787 * t + 16/116)
  }

  fx <- f(X / Xn)
  fy <- f(Y / Yn)
  fz <- f(Z / Zn)

  L <- if (Y / Yn > 0.008856) 116 * fy - 16 else 903.3 * (Y / Yn)
  a <- 500 * (fx - fy)
  b <- 200 * (fy - fz)

  C <- sqrt(a^2 + b^2)
  H <- (atan2(b, a) * 180 / pi) %% 360

  list(
    L = as.numeric(L),
    a = as.numeric(a),
    b = as.numeric(b),
    C = as.numeric(C),
    H = as.numeric(H)
  )
}

#' Convert spectrophotometry data to CIELab colour values
#'
#' Calculates CIELab colour coordinates from spectrophotometry transmission
#' data using the OIV-MA-AS2-11 (R2006) method with D65 illuminant and
#' 10 degree standard observer.
#'
#' This function accepts data in two formats:
#' \enumerate{
#'   \item \strong{Wide format}: Columns like X780, X779, ... X380 with transmission values,
#'         plus an identifier column (e.g., WineName, Sample, etc.)
#'   \item \strong{Long format}: Columns for sample ID, lambda (wavelength), and T (transmission)
#' }
#'
#' @param data A data.frame containing spectroscopy data in either wide or long format
#' @param sample_col Character name of the column to group samples by. This controls
#'   how results are aggregated:
#'   \itemize{
#'     \item Use "WineName" to get one result per wine (averages replicates)
#'     \item Use "Replicate" to get one result per replicate (no averaging)
#'   }
#'   Default: auto-detects "WineName", "sample_name", "Sample", or first column.
#' @param path_mm Numeric optical path length in millimeters (default: 10)
#'
#' @return A data.frame with one row per unique value of sample_col, containing:
#'   \describe{
#'     \item{<sample_col>}{The sample identifier (uses original column name)}
#'     \item{CIELab_L}{L* lightness (0-100)}
#'     \item{CIELab_a}{a* green-red axis}
#'     \item{CIELab_b}{b* blue-yellow axis}
#'     \item{CIELab_C}{Chroma (saturation)}
#'     \item{CIELab_H}{Hue angle in degrees (0-360)}
#'   }
#'
#' @details
#' The function automatically detects the data format:
#' \itemize{
#'   \item \strong{Wide format}: Has columns starting with "X" followed by numbers (e.g., X780, X500)
#'   \item \strong{Long format}: Has columns named "lambda" and "T"
#' }
#'
#' Transmission values should be fractions (0-1), not percentages. Small values
#' slightly > 1 (e.g., 1.003) are normal instrument noise and ignored. Values
#' significantly > 1 (> 1.1) trigger a warning - use
#' \code{cielab_spectroscopy_percentage_to_fraction()} to convert percentages.
#'
#' @examples
#' # Wide format example
#' wide_data <- data.frame(
#'   WineName = c("Sample1", "Sample2"),
#'   X780 = c(0.95, 0.80),
#'   X700 = c(0.90, 0.75),
#'   X600 = c(0.85, 0.70),
#'   X500 = c(0.80, 0.65),
#'   X400 = c(0.75, 0.60),
#'   X380 = c(0.70, 0.55)
#' )
#' # result <- cielab_from_spectrum(wide_data)
#'
#' # Long format example
#' long_data <- data.frame(
#'   WineName = rep(c("Sample1", "Sample2"), each = 81),
#'   lambda = rep(seq(380, 780, by = 5), 2),
#'   T = runif(162, 0.5, 1.0)
#' )
#' # result <- cielab_from_spectrum(long_data)
#'
#' # Control grouping with sample_col:
#' # - Default uses WineName -> averages replicates together
#' # result <- cielab_from_spectrum(data)
#'
#' # - Use Replicate to get individual results per replicate
#' # result <- cielab_from_spectrum(data, sample_col = "Replicate")
#'
#' @export
cielab_from_spectrum <- function(data, sample_col = NULL, path_mm = 10) {

  # Detect data format
  has_wavelength_cols <- any(grepl("^X[0-9]+$", names(data)))
  has_long_format <- all(c("lambda", "T") %in% names(data))

  if (!has_wavelength_cols && !has_long_format) {
    stop("Cannot detect data format. Expected either:\n",
         "  - Wide format: columns like X780, X700, X600, etc.\n",
         "  - Long format: columns 'lambda' and 'T'\n",
         "Found columns: ", paste(head(names(data), 10), collapse = ", "))
  }

  # Auto-detect sample column if not specified
  if (is.null(sample_col)) {
    # Prefer WineName if it exists
    if ("WineName" %in% names(data)) {
      sample_col <- "WineName"
    } else if ("sample_name" %in% names(data)) {
      sample_col <- "sample_name"
    } else if ("Sample" %in% names(data)) {
      sample_col <- "Sample"
    } else {
      # Find first non-numeric, non-wavelength column
      non_wave_cols <- names(data)[!grepl("^X[0-9]+$", names(data))]
      non_wave_cols <- setdiff(non_wave_cols, c("lambda", "T"))
      if (length(non_wave_cols) > 0) {
        sample_col <- non_wave_cols[1]
      } else {
        stop("Cannot auto-detect sample column. Please specify 'sample_col'.")
      }
    }
  }

  if (!sample_col %in% names(data)) {
    stop("Sample column '", sample_col, "' not found in data")
  }

  # Convert wide format to long format if needed
  if (has_wavelength_cols && !has_long_format) {
    # Get wavelength columns
    wave_cols <- grep("^X[0-9]+$", names(data), value = TRUE)
    wavelengths <- as.numeric(sub("^X", "", wave_cols))

    # Filter to 380-780 nm range
    keep <- wavelengths >= 380 & wavelengths <= 780
    wave_cols <- wave_cols[keep]
    wavelengths <- wavelengths[keep]

    if (length(wave_cols) == 0) {
      stop("No wavelength columns found in 380-780 nm range")
    }

    # Pivot to long format
    data_long <- do.call(rbind, lapply(seq_len(nrow(data)), function(i) {
      data.frame(
        .sample_id = as.character(data[[sample_col]][i]),
        lambda = wavelengths,
        T = as.numeric(data[i, wave_cols]),
        stringsAsFactors = FALSE
      )
    }))
  } else {
    # Already long format
    data_long <- data.frame(
      .sample_id = as.character(data[[sample_col]]),
      lambda = data$lambda,
      T = data$T,
      stringsAsFactors = FALSE
    )
  }


  # Check for percentage values (only warn if clearly percentages, not instrument noise)
  # Values like 1.003 are normal measurement noise, only warn if > 1.1
  max_T <- max(data_long$T, na.rm = TRUE)
  if (max_T > 1.1) {
    warning("Transmittance values significantly > 1 detected (max = ", round(max_T, 2), "). ",
            "Ensure T is fraction (0-1), not percentage (0-100). ",
            "Use cielab_spectroscopy_percentage_to_fraction() to convert.")
  }

  # Filter to 5nm intervals for efficiency
  data_long <- data_long[data_long$lambda %% 5 == 0, ]

  # Process each sample
  samples <- unique(data_long$.sample_id)
  results <- lapply(samples, function(s) {
    subset_data <- data_long[data_long$.sample_id == s, ]

    # Check wavelength coverage
    if (min(subset_data$lambda) > 380 || max(subset_data$lambda) < 780) {
      warning("Sample '", s, "' has incomplete wavelength coverage (",
              min(subset_data$lambda), "-", max(subset_data$lambda), " nm)")
      return(data.frame(
        .sample_id = s,
        CIELab_L = NA, CIELab_a = NA, CIELab_b = NA,
        CIELab_C = NA, CIELab_H = NA,
        stringsAsFactors = FALSE
      ))
    }

    lab <- .cielab_single(subset_data$lambda, subset_data$T, path_mm)

    data.frame(
      .sample_id = s,
      CIELab_L = lab$L,
      CIELab_a = lab$a,
      CIELab_b = lab$b,
      CIELab_C = lab$C,
      CIELab_H = lab$H,
      stringsAsFactors = FALSE
    )
  })

  result <- do.call(rbind, results)

  # Rename sample column back to original name
  names(result)[names(result) == ".sample_id"] <- sample_col

  result
}

# ==============================================================================
# Colour Difference Functions
# ==============================================================================

#' Calculate chroma from CIELab a* and b* values
#'
#' Computes the chroma (C*) value, representing colour saturation in
#' the CIELab colour space.
#'
#' @param a Numeric CIELab a* value(s)
#' @param b Numeric CIELab b* value(s)
#' @return Numeric chroma value(s)
#'
#' @examples
#' lab_to_C(30, 40)  # Returns 50
#'
#' @export
lab_to_C <- function(a, b) {

  sqrt(a^2 + b^2)
}

#' Calculate hue angle from CIELab a* and b* values
#'
#' Computes the hue angle (h) in degrees from CIELab a* and b* coordinates.
#'
#' @param a Numeric CIELab a* value(s)
#' @param b Numeric CIELab b* value(s)
#' @return Numeric hue angle(s) in degrees (0-360)
#'
#' @examples
#' hue_deg(1, 1)   # Returns 45
#' hue_deg(-1, 1)  # Returns 135
#'
#' @export
hue_deg <- function(a, b) {
  (atan2(b, a) * 180 / pi) %% 360
}

#' Calculate signed hue difference
#'
#' Computes the signed difference between two hue angles, handling the
#' wraparound at 360 degrees correctly.
#'
#' @param h2 Numeric hue angle(s) of second colour (degrees)
#' @param h1 Numeric hue angle(s) of first colour (degrees)
#' @return Numeric signed hue difference(s) in degrees (-180 to 180)
#'
#' @examples
#' delta_h_signed(10, 350)   # Returns 20 (crossing 0)
#' delta_h_signed(350, 10)   # Returns -20
#'
#' @export
delta_h_signed <- function(h2, h1) {
  d <- (h2 - h1 + 180) %% 360 - 180
  ifelse(d == -180, 180, d)
}

#' Calculate CIE76 colour difference (Delta E)
#'
#' Computes the Euclidean distance between two colours in CIELab space,
#' also known as Delta E (CIE 1976).
#'
#' @param L1,a1,b1 CIELab coordinates of first colour
#' @param L2,a2,b2 CIELab coordinates of second colour
#' @return Numeric Delta E value(s)
#'
#' @details
#' Interpretation of Delta E values:
#' \itemize{
#'   \item 0-1: Not perceptible by human eye
#'   \item 1-2: Perceptible through close observation
#'   \item 2-10: Perceptible at a glance
#'   \item 11-49: Colours are more similar than opposite
#'   \item 100: Colours are exact opposites
#' }
#'
#' @examples
#' deltaE76(50, 10, 20, 55, 15, 25)
#'
#' @export
deltaE76 <- function(L1, a1, b1, L2, a2, b2) {
  sqrt((L2 - L1)^2 + (a2 - a1)^2 + (b2 - b1)^2)
}

#' Calculate centroid-to-centroid colour differences
#'
#' Computes colour differences between group centroids in CIELab space.
#' This function calculates the mean L*, a*, b* values for each group and
#' then computes differences (deltaL, deltaC, deltaH, deltaE76, deltaE00)
#' between a reference level and all other levels.
#'
#' @param data A data.frame containing CIELab colour data with columns
#'   \code{CIELab_L}, \code{CIELab_a}, and \code{CIELab_b}
#' @param compare_by Character name of the column containing the factor
#'   to compare (default: "EtOH")
#' @param reference_level Character value within \code{compare_by} to use
#'   as the reference for comparisons (default: "CTRL")
#' @param group_by Character vector of column names to group by before
#'   computing centroids. Use this for nested designs (e.g., group_by = "Wine"
#'   to compute separate differences for each wine). NULL for no grouping.
#'
#' @return A data.frame with columns:
#'   \describe{
#'     \item{<group_by columns>}{Grouping variables (if specified)}
#'     \item{<compare_by>}{The comparison level}
#'     \item{contrast}{Character description of the comparison (e.g., "Treatment – CTRL")}
#'     \item{dL}{Difference in L* (lightness)}
#'     \item{dC}{Difference in C* (chroma)}
#'     \item{dh}{Signed difference in hue angle (degrees)}
#'     \item{dE76_cent}{CIE76 Delta E between centroids}
#'     \item{dE00_cent}{CIE2000 Delta E between centroids}
#'   }
#'
#' @details
#' This function is useful for comparing colour changes between experimental
#' conditions by reducing multiple observations to their centroids first.
#' This approach is robust to outliers and gives a single summary value
#' per comparison.
#'
#' The function requires the \pkg{farver} package for CIE2000 calculations.
#'
#' @examples
#' \dontrun{
#' # Compare ethanol treatments to control
#' results <- calc_colour_diff(
#'   data = wine_colours,
#'   compare_by = "EtOH",
#'   reference_level = "CTRL"
#' )
#'
#' # Compare within each wine variety
#' results <- calc_colour_diff(
#'   data = wine_colours,
#'   compare_by = "Treatment",
#'   reference_level = "Control",
#'   group_by = "WineType"
#' )
#' }
#'
#' @seealso \code{\link{calc_pairwise_dE}} for observation-level comparisons
#'
#' @importFrom farver compare_colour
#' @export
calc_colour_diff <- function(data,
                            compare_by = "EtOH",
                            reference_level = "CTRL",
                            group_by = NULL) {

  # Build grouping variables
  group_vars <- c(group_by, compare_by)

  # Calculate centroids
  centroids <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::summarise(
      L = mean(CIELab_L),
      a = mean(CIELab_a),
      b = mean(CIELab_b),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      C = lab_to_C(a, b),
      h = hue_deg(a, b)
    )

  # Get reference centroids
  ref_centroids <- centroids %>%
    dplyr::filter(.data[[compare_by]] == reference_level)

  # Calculate differences for non-reference levels
  results <- centroids %>%
    dplyr::filter(.data[[compare_by]] != reference_level) %>%
    {
      if (!is.null(group_by)) {
        dplyr::left_join(.,
                  ref_centroids %>%
                    dplyr::select(dplyr::all_of(c(group_by, "L", "a", "b", "C", "h"))),
                  by = group_by,
                  suffix = c("", "_ref"))
      } else {
        dplyr::mutate(.,
               L_ref = ref_centroids$L[1],
               a_ref = ref_centroids$a[1],
               b_ref = ref_centroids$b[1],
               C_ref = ref_centroids$C[1],
               h_ref = ref_centroids$h[1])
      }
    } %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      contrast = paste(.data[[compare_by]], "\u2013", reference_level),
      dL = L - L_ref,
      dC = C - C_ref,
      dh = delta_h_signed(h, h_ref),
      dE76_cent = deltaE76(L_ref, a_ref, b_ref, L, a, b),
      dE00_cent = farver::compare_colour(
        from = matrix(c(L_ref, a_ref, b_ref), ncol = 3),
        to = matrix(c(L, a, b), ncol = 3),
        from_space = "lab", method = "CIE2000"
      )[1]
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(c(group_by, compare_by, "contrast",
                    "dL", "dC", "dh", "dE76_cent", "dE00_cent")))

  return(results)
}

#' Calculate mean pairwise colour differences
#'
#' Computes pairwise colour differences between all observations in a
#' reference group and all observations in target groups. This gives
#' a distribution of Delta E values rather than a single centroid-based value.
#'
#' @param data A data.frame containing CIELab colour data with columns
#'   \code{CIELab_L}, \code{CIELab_a}, and \code{CIELab_b}
#' @param compare_by Character name of the column containing the factor
#'   to compare (default: "EtOH")
#' @param reference_level Character value within \code{compare_by} to use
#'   as the reference for comparisons (default: "CTRL")
#' @param target_levels Character vector of levels to compare against reference.
#'   If NULL, all non-reference levels are used.
#' @param group_by Character vector of column names to group by before
#'   computing pairwise differences. NULL for no grouping.
#' @param method Character specifying the Delta E formula to use.
#'   Either "CIE1976" (default) or "CIE2000".
#'
#' @return A data.frame with columns:
#'   \describe{
#'     \item{<group_by columns>}{Grouping variables (if specified)}
#'     \item{comparison}{Character description of the comparison}
#'     \item{dE_mean}{Mean Delta E across all pairwise comparisons}
#'     \item{dE_sd}{Standard deviation of Delta E values}
#'   }
#'
#' @details
#' This function performs all-pairs comparisons between observations in
#' different groups. For each pair of (reference observation, target observation),
#' it computes Delta E, then summarises these values.
#'
#' This approach captures the full variability in colour differences and is
#' useful when you need confidence intervals or want to assess the spread
#' of colour differences.
#'
#' The function requires the \pkg{farver} package for CIE2000 calculations.
#'
#' @examples
#' \dontrun{
#' # Compare all treatments to control using CIE76
#' results <- calc_pairwise_dE(
#'   data = wine_colours,
#'   compare_by = "EtOH",
#'   reference_level = "CTRL"
#' )
#'
#' # Use CIE2000 for more perceptually uniform differences
#' results <- calc_pairwise_dE(
#'   data = wine_colours,
#'   compare_by = "Treatment",
#'   reference_level = "Control",
#'   method = "CIE2000"
#' )
#'
#' # Compare specific treatments only
#' results <- calc_pairwise_dE(
#'   data = wine_colours,
#'   compare_by = "Treatment",
#'   reference_level = "Control",
#'   target_levels = c("Low", "High")
#' )
#' }
#'
#' @seealso \code{\link{calc_colour_diff}} for centroid-based comparisons
#'
#' @export
calc_pairwise_dE <- function(data,
                             compare_by = "EtOH",
                             reference_level = "CTRL",
                             target_levels = NULL,
                             group_by = NULL,
                             method = "CIE1976") {

  if (is.null(target_levels)) {
    target_levels <- setdiff(unique(data[[compare_by]]), reference_level)
  }

  # Prepare reference data
  ref_data <- data %>%
    dplyr::filter(.data[[compare_by]] == reference_level) %>%
    dplyr::select(dplyr::all_of(c(group_by, "CIELab_L", "CIELab_a", "CIELab_b"))) %>%
    dplyr::mutate(key = 1)

  # Calculate for each target level
  results <- lapply(target_levels, function(target) {
    target_data <- data %>%
      dplyr::filter(.data[[compare_by]] == target) %>%
      dplyr::select(dplyr::all_of(c(group_by, "CIELab_L", "CIELab_a", "CIELab_b"))) %>%
      dplyr::mutate(key = 1)

    crossed <- dplyr::inner_join(ref_data, target_data,
                          by = c(group_by, "key"),
                          suffix = c("_ref", "_target"),
                          relationship = "many-to-many")

    if (method == "CIE2000") {
      crossed <- crossed %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          dE = farver::compare_colour(
            from = matrix(c(CIELab_L_ref, CIELab_a_ref, CIELab_b_ref), ncol = 3),
            to = matrix(c(CIELab_L_target, CIELab_a_target, CIELab_b_target), ncol = 3),
            from_space = "lab", method = "CIE2000"
          )[1]
        ) %>%
        dplyr::ungroup()
    } else {
      crossed <- crossed %>%
        dplyr::mutate(
          dE = deltaE76(CIELab_L_ref, CIELab_a_ref, CIELab_b_ref,
                        CIELab_L_target, CIELab_a_target, CIELab_b_target)
        )
    }

    crossed %>%
      {if (!is.null(group_by)) dplyr::group_by(., dplyr::across(dplyr::all_of(group_by))) else .} %>%
      dplyr::summarise(
        comparison = paste(target, "\u2013", reference_level),
        dE_mean = mean(dE),
        dE_sd = sd(dE),
        .groups = "drop"
      )
  }) %>%
    dplyr::bind_rows()

  return(results)
}

# ==============================================================================
# Visualization Functions
# ==============================================================================

#' Create and save CIELab colour swatch visualization
#'
#' Generates a grid of colour swatches from CIELab data and exports to
#' various image formats (PNG, PDF, SVG, etc.).
#'
#' @param CIELab A data.frame containing columns:
#'   \describe{
#'     \item{WineName}{Character labels for each colour swatch}
#'     \item{CIELab_L}{Numeric L* values (0-100)}
#'     \item{CIELab_a}{Numeric a* values}
#'     \item{CIELab_b}{Numeric b* values}
#'   }
#' @param file Character output filename. The extension determines the format
#'   (e.g., ".png", ".pdf", ".svg", ".tiff", ".eps")
#' @param chips_per_row Integer number of swatches per row (NULL = auto square)
#' @param cm_per_chip Numeric size of each tile in centimeters
#' @param border_col Character colour for tile borders
#' @param font_size_pt Numeric font size in points for labels
#' @param dpi Integer resolution for raster formats (PNG, TIFF)
#' @param ... Additional arguments passed to ggplot2::ggsave()
#'
#' @return Invisibly returns the ggplot object
#'
#' @details
#' Supported output formats depend on your system's graphics devices:
#' \itemize{
#'   \item PNG, JPEG, TIFF: Raster formats (use dpi for quality)
#'   \item PDF, EPS, PS: Vector formats (scalable)
#'   \item SVG: Web-friendly vector format (requires svglite package)
#' }
#'
#' @examples
#' df <- data.frame(
#'   WineName = c("Red", "White", "Rose"),
#'   CIELab_L = c(30, 90, 70),
#'   CIELab_a = c(50, -5, 20),
#'   CIELab_b = c(30, 10, 15)
#' )
#' cielab_swatch(df, "swatches.png")
#' cielab_swatch(df, "swatches.pdf")
#' cielab_swatch(df, "swatches.svg")
#'
#' @export
cielab_swatch <- function(CIELab,
                          file = "CIELab_swatches.png",
                          chips_per_row = NULL,
                          cm_per_chip = 2,
                          border_col = "grey30",
                          font_size_pt = 6,
                          dpi = 300,
                          ...) {

  # Check for required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required. Install with: install.packages('ggplot2')")
  }
  if (!requireNamespace("colorspace", quietly = TRUE)) {
    stop("Package 'colorspace' is required. Install with: install.packages('colorspace')")
  }

  # Check for SVG support
  ext <- tolower(tools::file_ext(file))
  if (ext == "svg" && !requireNamespace("svglite", quietly = TRUE)) {
    stop("Package 'svglite' is required for SVG output. ",
         "Install with: install.packages('svglite')")
  }

  # Validate input
  required_cols <- c("WineName", "CIELab_L", "CIELab_a", "CIELab_b")
  missing_cols <- setdiff(required_cols, names(CIELab))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  n <- nrow(CIELab)
  if (n == 0) {
    stop("Input data frame has no rows")
  }

  if (is.null(chips_per_row)) {
    chips_per_row <- ceiling(sqrt(n))
  }
  n_rows <- ceiling(n / chips_per_row)

  # Build plotting data frame
  df <- data.frame(
    idx = seq_len(n),
    label = as.character(CIELab$WineName),
    hex = colorspace::hex(
      colorspace::LAB(CIELab$CIELab_L, CIELab$CIELab_a, CIELab$CIELab_b),
      fixup = TRUE
    ),
    label_col = ifelse(CIELab$CIELab_L > 60, "black", "white"),
    stringsAsFactors = FALSE
  )

  df$col <- ((df$idx - 1) %% chips_per_row) + 1
  df$row <- n_rows - ((df$idx - 1) %/% chips_per_row)  # top-to-bottom

  # Create plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = col, y = row)) +
    ggplot2::geom_tile(
      ggplot2::aes(fill = hex),
      colour = border_col,
      linewidth = 0.3
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = label, colour = label_col),
      size = font_size_pt / ggplot2::.pt
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_colour_identity() +
    ggplot2::coord_equal(expand = FALSE) +
    ggplot2::scale_x_continuous(NULL, breaks = NULL) +
    ggplot2::scale_y_continuous(NULL, breaks = NULL) +
    ggplot2::theme_void()

  # Save to file
  ggplot2::ggsave(
    filename = file,
    plot = p,
    width = chips_per_row * cm_per_chip,
    height = n_rows * cm_per_chip,
    units = "cm",
    dpi = dpi,
    bg = "white",
    ...
  )

  message("Saved colour swatch to: ", file)
  invisible(p)
}

# ==============================================================================
# Data Processing Functions
# ==============================================================================

#' Process spectrophotometer CSV files into a combined data frame
#'
#' Reads a folder of individual CSV files from a spectrophotometer (one file
#' per reading) and combines them into a single wide-format data frame
#' suitable for use with cielab_from_spectrum().
#'
#' @param folder_path Character path to folder containing CSV files
#' @param skip Integer number of header rows to skip in each CSV file
#'   (default 13, typical for many spectrophotometers)
#' @param wavelength_col Character name of the wavelength column in source files
#'   (default "WL.nm.")
#' @param transmission_col Character name of the transmission column in source files
#'   (default "X.T")
#' @param wl_min Numeric minimum wavelength to include (default 300)
#' @param wl_max Numeric maximum wavelength to include (default 800)
#' @param pattern Character regex pattern to match CSV files (default "\\.csv$")
#'
#' @return A data.frame with columns:
#'   \describe{
#'     \item{sample_name}{Sample identifier (from filename)}
#'     \item{lambda}{Wavelength in nm}
#'     \item{T}{Transmission as fraction (0-1)}
#'   }
#'   Data is in long format, ready for grouping and processing.
#'
#' @details
#' This function:
#' \enumerate{
#'   \item Scans the folder for CSV files matching the pattern
#'   \item Reads each file, skipping header rows
#'   \item Filters to the specified wavelength range
#'   \item Converts transmission from percentage to fraction
#'   \item Combines all files with sample identifiers from filenames
#' }
#'
#' @examples
#' \dontrun{
#' # Process all CSV files in a folder
#' spectra <- process_spectrum("path/to/spectrophotometer/data")
#'
#' # Process with custom settings
#' spectra <- process_spectrum(
#'   "path/to/data",
#'   skip = 10,
#'   wavelength_col = "Wavelength",
#'   transmission_col = "Transmittance"
#' )
#'
#' # Then use with cielab_from_spectrum for each sample
#' library(dplyr)
#' results <- spectra %>%
#'   group_by(sample_name) %>%
#'   group_map(~ {
#'     spec_df <- data.frame(
#'       lambda = .x$lambda,
#'       T = .x$T,
#'       path_mm = 10
#'     )
#'     cielab_from_spectrum(spec_df)
#'   })
#' }
#'
#' @export
process_spectrum <- function(folder_path,
                             skip = 13,
                             wavelength_col = "WL.nm.",
                             transmission_col = "X.T",
                             wl_min = 300,
                             wl_max = 800,
                             pattern = "\\.csv$") {

  # Validate folder exists
  if (!dir.exists(folder_path)) {
    stop("Folder does not exist: ", folder_path)
  }

  # Find CSV files
  csv_files <- list.files(
    folder_path,
    pattern = pattern,
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(csv_files) == 0) {
    stop("No CSV files found in: ", folder_path)
  }

  message("Found ", length(csv_files), " CSV files to process")

  # Process each file
  process_single_file <- function(filepath) {
    sample_name <- tools::file_path_sans_ext(basename(filepath))

    # Read CSV with specified skip
    spec_data <- tryCatch(
      read.csv(filepath, skip = skip),
      error = function(e) {
        warning("Failed to read file: ", filepath, "\n  ", e$message)
        return(NULL)
      }
    )

    if (is.null(spec_data)) return(NULL)

    # Check for required columns
    if (!wavelength_col %in% names(spec_data)) {
      warning("Missing wavelength column '", wavelength_col, "' in: ", filepath)
      return(NULL)
    }
    if (!transmission_col %in% names(spec_data)) {
      warning("Missing transmission column '", transmission_col, "' in: ", filepath)
      return(NULL)
    }

    # Extract and filter data
    wl <- spec_data[[wavelength_col]]
    trans <- spec_data[[transmission_col]]

    valid_idx <- !is.na(wl) & wl >= wl_min & wl <= wl_max

    data.frame(
      sample_name = sample_name,
      lambda = wl[valid_idx],
      T = cielab_spectroscopy_percentage_to_fraction(trans[valid_idx]),
      stringsAsFactors = FALSE
    )
  }

  # Process all files and combine
  results <- lapply(csv_files, process_single_file)
  results <- results[!sapply(results, is.null)]

  if (length(results) == 0) {
    stop("No files were successfully processed")
  }

  combined <- do.call(rbind, results)
  message("Processed ", length(results), " files with ",
          nrow(combined), " total observations")

  combined
}
