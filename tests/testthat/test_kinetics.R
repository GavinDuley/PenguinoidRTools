# Tests for colour kinetics and the standalone Delta E / hex helpers

library(testthat)
library(PenguinoidRTools)

# ==============================================================================
# deltaE00 / deltaE94
# ==============================================================================

test_that("deltaE00 and deltaE94 match farver and are vectorised", {
  skip_if_not_installed("farver")

  expect_equal(
    deltaE00(50, 10, 20, 55, 15, 25),
    farver::compare_colour(matrix(c(50, 10, 20), ncol = 3),
                           matrix(c(55, 15, 25), ncol = 3),
                           from_space = "lab", method = "cie2000")[1]
  )
  expect_equal(
    deltaE94(50, 10, 20, 55, 15, 25),
    farver::compare_colour(matrix(c(50, 10, 20), ncol = 3),
                           matrix(c(55, 15, 25), ncol = 3),
                           from_space = "lab", method = "cie94")[1]
  )

  # Identical colours give zero
  expect_equal(deltaE00(50, 10, 20, 50, 10, 20), 0)
  expect_equal(deltaE94(50, 10, 20, 50, 10, 20), 0)

  # Vectorised over pairs (arguments recycled): pair 1 differs in chroma,
  # pair 2 differs in lightness only
  v <- deltaE00(c(50, 50), c(0, 0), c(0, 0), c(50, 60), c(3, 0), c(4, 0))
  expect_length(v, 2)
  expect_true(all(v > 0))
  expect_true(all(is.finite(v)))
})

# ==============================================================================
# cielab_to_hex
# ==============================================================================

test_that("cielab_to_hex returns valid hex strings", {
  skip_if_not_installed("colorspace")

  hex <- cielab_to_hex(c(30, 90, 70), c(50, -5, 20), c(30, 10, 15))
  expect_length(hex, 3)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", hex)))

  # Matches the conversion cielab_swatch uses
  expect_equal(
    cielab_to_hex(50, 40, 30),
    colorspace::hex(colorspace::LAB(50, 40, 30), fixup = TRUE)
  )
})

# ==============================================================================
# cielab_kinetics
# ==============================================================================

# Helper: build a wine x treatment x time series with replicates. Lightness
# decreases over time so colour-change direction is predictable.
make_kinetics_data <- function() {
  set.seed(123)
  drift <- c("0M" = 0, "3M" = -3, "12M" = -8)
  do.call(rbind, lapply(c("WineA", "WineB"), function(wine) {
    base_L <- if (wine == "WineA") 40 else 55
    do.call(rbind, lapply(c("CTRL", "T1"), function(trt) {
      do.call(rbind, lapply(names(drift), function(tp) {
        data.frame(
          Wine = wine, Treatment = trt, Months = tp,
          CIELab_L = base_L + drift[[tp]] + rnorm(3, 0, 0.1),
          CIELab_a = 30 + rnorm(3, 0, 0.1),
          CIELab_b = 15 + rnorm(3, 0, 0.1),
          stringsAsFactors = FALSE
        )
      }))
    }))
  }))
}

test_that("cielab_kinetics orders timepoints chronologically despite alphabetical factor levels", {
  d <- make_kinetics_data()
  d$Months <- factor(d$Months)  # default levels are alphabetical: 0M, 12M, 3M

  k <- cielab_kinetics(d, time_col = "Months", group_by = c("Wine", "Treatment"))

  bl <- k[k$comparison_type == "baseline" &
            k$Wine == "WineA" & k$Treatment == "CTRL", ]
  # Baseline comparisons should be 0M->3M then 0M->12M, not alphabetical
  expect_equal(bl$from_time, c("0M", "0M"))
  expect_equal(bl$to_time, c("3M", "12M"))

  con <- k[k$comparison_type == "consecutive" &
             k$Wine == "WineA" & k$Treatment == "CTRL", ]
  expect_equal(con$from_time, c("0M", "3M"))
  expect_equal(con$to_time, c("3M", "12M"))
})

test_that("cielab_kinetics reports colour getting darker over time", {
  d <- make_kinetics_data()

  k <- cielab_kinetics(d, time_col = "Months", group_by = c("Wine", "Treatment"))

  # Lightness decreases, so dL (later - earlier) should be negative throughout
  expect_true(all(k$dL < 0))
  # dE columns are non-negative and CIE2000 <= CIE76 here (typical)
  expect_true(all(k$dE76 >= 0))
  expect_true(all(k$dE00 >= 0))
})

test_that("cielab_kinetics respects the comparison argument", {
  d <- make_kinetics_data()

  kb <- cielab_kinetics(d, "Months", group_by = c("Wine", "Treatment"),
                        comparison = "baseline")
  kc <- cielab_kinetics(d, "Months", group_by = c("Wine", "Treatment"),
                        comparison = "consecutive")
  kboth <- cielab_kinetics(d, "Months", group_by = c("Wine", "Treatment"),
                           comparison = "both")

  expect_true(all(kb$comparison_type == "baseline"))
  expect_true(all(kc$comparison_type == "consecutive"))
  # 4 groups, 3 timepoints -> 2 comparisons each per type
  expect_equal(nrow(kb), 4 * 2)
  expect_equal(nrow(kc), 4 * 2)
  expect_equal(nrow(kboth), nrow(kb) + nrow(kc))
})

test_that("cielab_kinetics works without grouping and with a time_levels override", {
  d <- make_kinetics_data()

  k_auto <- cielab_kinetics(d, "Months", group_by = c("Wine", "Treatment"))
  k_expl <- cielab_kinetics(d, "Months", group_by = c("Wine", "Treatment"),
                            time_levels = c("0M", "3M", "12M"))
  expect_equal(k_auto, k_expl)

  k_ng <- cielab_kinetics(d, "Months")
  expect_false("Wine" %in% names(k_ng))
  expect_true(nrow(k_ng) > 0)
})

test_that("cielab_kinetics handles numeric timepoints and a group missing a timepoint", {
  d <- make_kinetics_data()
  d$MonthsNum <- as.numeric(sub("M", "", d$Months))

  k_num <- cielab_kinetics(d, "MonthsNum", group_by = c("Wine", "Treatment"))
  bl <- k_num[k_num$comparison_type == "baseline" &
                k_num$Wine == "WineA" & k_num$Treatment == "CTRL", ]
  expect_equal(bl$from_time, c("0", "0"))
  expect_equal(bl$to_time, c("3", "12"))

  # Drop the 12M timepoint for one group: it should simply have fewer rows
  d2 <- d[!(d$Wine == "WineB" & d$Treatment == "T1" & d$Months == "12M"), ]
  k2 <- cielab_kinetics(d2, "Months", group_by = c("Wine", "Treatment"))
  wb_t1 <- k2[k2$Wine == "WineB" & k2$Treatment == "T1" &
                k2$comparison_type == "baseline", ]
  expect_equal(wb_t1$to_time, "3M")  # only 0M and 3M present
})

test_that("cielab_kinetics errors on missing columns or too few timepoints", {
  d <- make_kinetics_data()

  expect_error(
    cielab_kinetics(d, "NotAColumn"),
    "Missing required columns"
  )

  one_time <- d[d$Months == "0M", ]
  expect_error(
    cielab_kinetics(one_time, "Months"),
    "at least two distinct timepoints"
  )
})
