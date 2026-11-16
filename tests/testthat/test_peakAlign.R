# Tests for read_agilent_dad_peaks() and align_peaks_by_rt().
#
# The alignment tests run against a deterministic synthetic fixture with known
# ground truth (see helper-peaks.R). Its nine compounds include a close pair
# (P5 = 8.00, P6 = 8.05 min) whose observed RTs overlap between samples once
# per-sample drift is applied, peaks absent from the reference sample, and a
# peak missing from one sample. The original greedy, single-reference aligner
# recovered seven of the nine; the drift-correcting, reference-free aligner
# must recover all of them.

# ==============================================================================
# read_agilent_dad_peaks
# ==============================================================================

# Write a minimal two-sample MassHunter-style UV-DAD export to a temp file.
write_example_export <- function(sep = ",") {
  block <- function(name, rows) {
    hdr <- paste0("DAD1 A, Sig=280,4 Ref=off ", name, ".d")
    col <- paste(c("", "Peak", "RT", "Height", "Area"), collapse = sep)
    dat <- vapply(rows, function(r) paste(c("data.d", r), collapse = sep), character(1))
    c(hdr, col, dat, "")
  }
  lines <- c(
    block("01.WINE_A", list(c(1, 2.10, 100, 1000), c(2, 5.00, 80, 800))),
    block("02.WINE_B", list(c(1, 2.12, 110, 1100), c(2, 5.03, 82, 820)))
  )
  f <- tempfile(fileext = ".csv")
  writeLines(lines, f)
  f
}

test_that("read_agilent_dad_peaks parses a multi-sample export", {
  f <- write_example_export()
  on.exit(unlink(f))

  peaks <- suppressMessages(read_agilent_dad_peaks(f))

  expect_s3_class(peaks, "data.frame")
  expect_equal(names(peaks), c("sample", "peak", "rt", "height", "area"))
  expect_equal(nrow(peaks), 4)
  # Sample names come from the text after "Ref=off", with ".d" stripped, in
  # order of appearance (first sample becomes the alignment reference)
  expect_equal(unique(peaks$sample), c("01.WINE_A", "02.WINE_B"))
  expect_equal(peaks$peak, c(1L, 2L, 1L, 2L))
  expect_equal(peaks$rt, c(2.10, 5.00, 2.12, 5.03))
  expect_type(peaks$height, "double")
  expect_type(peaks$area, "double")
})

test_that("read_agilent_dad_peaks auto-detects the separator and honours an override", {
  f <- write_example_export()
  on.exit(unlink(f))

  expect_message(read_agilent_dad_peaks(f), "separator: comma")
  # Explicit separator suppresses detection and still parses
  peaks <- read_agilent_dad_peaks(f, sep = ",")
  expect_equal(nrow(peaks), 4)
})

test_that("read_agilent_dad_peaks errors when no peak data is present", {
  f <- tempfile(fileext = ".csv")
  writeLines(c("not a peak table", "", "just some text"), f)
  on.exit(unlink(f))

  expect_error(
    suppressMessages(read_agilent_dad_peaks(f)),
    "No peak data found"
  )
})

# ==============================================================================
# align_peaks_by_rt - structure and input handling
# ==============================================================================

test_that("align_peaks_by_rt returns the documented structure", {
  fx <- make_peak_fixture()
  res <- suppressMessages(align_peaks_by_rt(fx$peaks, max_drift = 0.15))

  expect_named(res, c("aligned", "summary", "drift_stats", "params"))
  expect_named(res$aligned,
               c("sample", "peak", "rt", "height", "area", "peak_group",
                 "ref_rt", "rt_corrected", "rt_diff", "flag", "flag_reason"))
  expect_named(res$summary,
               c("peak_group", "ref_rt", "in_reference", "mean_rt", "sd_rt",
                 "cv_pct", "n_matched", "n_ambiguous", "n_missing"))
  expect_true(all(c("sample", "is_reference", "n_peaks", "n_matched",
                    "n_ambiguous", "n_missing", "mean_rt_diff", "n_anchors",
                    "drift_model") %in% names(res$drift_stats)))
  expect_true(all(res$aligned$flag %in% c("OK", "AMBIGUOUS", "MISSING")))
  expect_named(res$params, c("reference", "max_drift", "rt_tol", "drift_model"))
  expect_equal(res$params$reference, "S1")
})

test_that("aligned has exactly one row per peak group and sample", {
  fx <- make_peak_fixture()
  res <- suppressMessages(align_peaks_by_rt(fx$peaks, max_drift = 0.15))
  al <- res$aligned

  expect_equal(nrow(al), nrow(res$summary) * 5)
  expect_false(any(duplicated(al[c("peak_group", "sample")])))
  # Every input peak appears exactly once; the rest are MISSING rows
  expect_equal(sum(!is.na(al$rt)), nrow(fx$peaks))
  expect_true(all(is.na(al$rt) == (al$flag == "MISSING")))
  # ref_rt identifies the group and increases with the group number
  expect_equal(length(unique(al$ref_rt)), nrow(res$summary))
  expect_false(is.unsorted(res$summary$ref_rt, strictly = TRUE))
})

test_that("align_peaks_by_rt requires at least two samples", {
  fx <- make_peak_fixture()
  one <- fx$peaks[fx$peaks$sample == "S1", ]
  expect_error(align_peaks_by_rt(one), "at least two samples")
})

test_that("align_peaks_by_rt validates rt_tol and drift_model", {
  fx <- make_peak_fixture()
  expect_error(align_peaks_by_rt(fx$peaks, rt_tol = -1), "rt_tol")
  expect_error(align_peaks_by_rt(fx$peaks, drift_model = "wobbly"), "should be one of")
  res <- suppressMessages(align_peaks_by_rt(fx$peaks, max_drift = 0.15))
  expect_error(align_peaks_by_rt(res$aligned), "previous alignment result")
})

test_that("the reference sample is the first sample and its peaks anchor groups", {
  fx <- make_peak_fixture()
  res <- suppressMessages(align_peaks_by_rt(fx$peaks, max_drift = 0.15))

  ref_rows <- res$aligned[res$aligned$sample == "S1" & !is.na(res$aligned$rt), ]
  # Every reference-sample peak defines its group's ref_rt and has no drift
  expect_true(all(ref_rows$rt_diff == 0))
  expect_true(all(ref_rows$rt_corrected == ref_rows$rt))
  expect_setequal(res$summary$ref_rt[res$summary$in_reference],
                  fx$peaks$rt[fx$peaks$sample == "S1"])
})

# ==============================================================================
# Drift estimation
# ==============================================================================

test_that("per-sample drift is estimated and removed", {
  fx <- make_peak_fixture()
  res <- suppressMessages(align_peaks_by_rt(fx$peaks, max_drift = 0.15))

  # The fixture's systematic offsets (see helper-peaks.R)
  offsets <- c(S1 = 0, S2 = 0.02, S3 = -0.03, S4 = 0.05, S5 = -0.04)
  ds <- res$drift_stats
  expect_equal(ds$mean_rt_diff, unname(offsets[ds$sample]), tolerance = 1e-6)
  expect_equal(ds$drift_model[ds$is_reference], "reference")
  expect_true(all(ds$n_anchors[!ds$is_reference] >= 4))

  # After correction every compound sits at a single RT across samples
  al <- label_aligned_with_truth(res$aligned[!is.na(res$aligned$rt), ], fx$truth)
  spread <- tapply(al$rt_corrected, al$true_peak, function(v) diff(range(v)))
  expect_true(all(spread < 1e-6))
})

test_that("drift that changes along the run is followed", {
  # 30 well-separated compounds; each sample's drift is an offset plus a slope
  true_rt <- seq(2, 31, by = 1)
  mk <- function(name, offset, slope) {
    data.frame(sample = name, peak = seq_along(true_rt),
               rt = round(true_rt + offset + slope * (true_rt - 15), 4),
               height = 1, area = 10)
  }
  peaks <- rbind(mk("A", 0, 0), mk("B", 0.05, 0.004), mk("C", -0.04, -0.006))
  res <- suppressMessages(align_peaks_by_rt(peaks, max_drift = 0.25))

  expect_equal(nrow(res$summary), 30)
  expect_true(all(res$summary$n_matched == 3))
  expect_true(all(res$aligned$flag == "OK"))
  expect_true(all(res$drift_stats$drift_model[-1] == "smooth"))
  # Raw drift reaches 0.14 min at the ends but little is left after correction
  expect_gt(max(abs(res$aligned$rt_diff)), 0.1)
  expect_lt(max(abs(res$aligned$rt_corrected - res$aligned$ref_rt)), 0.01)
})

test_that("drift_model = 'none' groups raw retention times", {
  fx <- make_peak_fixture()
  res <- suppressMessages(align_peaks_by_rt(fx$peaks, max_drift = 0.15,
                                            drift_model = "none"))
  al <- res$aligned[!is.na(res$aligned$rt), ]
  expect_equal(al$rt_corrected, al$rt)
  expect_equal(res$params$rt_tol, 0.15)
  # Without drift correction the close pair cannot be resolved
  expect_lt(score_alignment(res$aligned, fx$truth)$n_recovered, 9)
})

# ==============================================================================
# Grouping against ground truth
# ==============================================================================

test_that("all nine compounds in the fixture are recovered exactly", {
  fx <- make_peak_fixture()
  for (md in c(0.1, 0.15, 0.25, 0.5)) {
    res <- suppressMessages(align_peaks_by_rt(fx$peaks, max_drift = md))
    score <- score_alignment(res$aligned, fx$truth)
    expect_equal(score$n_true_peaks, 9)
    expect_equal(score$n_groups, 9)
    expect_equal(score$n_recovered, 9)
    expect_equal(score$n_impure_groups, 0)
    expect_equal(score$n_fragmented, 0)
  }
})

test_that("well-separated peaks present in every sample form complete groups", {
  fx <- make_peak_fixture()
  res <- suppressMessages(align_peaks_by_rt(fx$peaks, max_drift = 0.15))

  # P1, P2, P4 are far from any neighbour and present in all five samples
  for (rrt in c(2.10, 3.50, 5.00)) {
    grp <- res$summary[abs(res$summary$ref_rt - rrt) < 1e-6, ]
    expect_equal(grp$n_matched, 5)
    expect_equal(grp$n_missing, 0)
    expect_true(grp$in_reference)
    expect_lt(grp$cv_pct, 5)
  }
})

test_that("the close pair P5/P6 is separated despite overlapping raw RTs", {
  fx <- make_peak_fixture()
  res <- suppressMessages(align_peaks_by_rt(fx$peaks, max_drift = 0.15))
  recovered <- recovered_true_peaks(res$aligned, fx$truth)
  expect_true(all(c("P5", "P6") %in% recovered))

  al <- label_aligned_with_truth(res$aligned[!is.na(res$aligned$rt), ], fx$truth)
  near8 <- al[al$ref_rt > 7.5 & al$ref_rt < 8.5, ]
  mixed <- tapply(near8$true_peak, near8$ref_rt, function(tp) length(unique(tp)) > 1)
  expect_false(any(mixed))

  # The raw RTs really do overlap: S3's P6 (8.02) elutes before S4's P5 (8.05)
  expect_lt(near8$rt[near8$sample == "S3" & near8$true_peak == "P6"],
            near8$rt[near8$sample == "S4" & near8$true_peak == "P5"])

  p5 <- res$summary[abs(res$summary$ref_rt - 8.00) < 1e-6, ]
  p6 <- res$summary[abs(res$summary$ref_rt - 8.05) < 1e-6, ]
  expect_equal(c(p5$n_matched, p5$n_missing), c(5, 0))
  expect_equal(c(p6$n_matched, p6$n_missing), c(3, 2))
  expect_false(p6$in_reference)
})

test_that("peaks absent from the reference form their own groups", {
  fx <- make_peak_fixture()
  res <- suppressMessages(align_peaks_by_rt(fx$peaks, max_drift = 0.15))
  recovered <- recovered_true_peaks(res$aligned, fx$truth)

  # P3 (3.70) is in S2-S5; X1 (6.50) in S5 only; X2 (9.80) in S2 and S4
  expect_true(all(c("P3", "X1", "X2") %in% recovered))
  new_groups <- res$summary[!res$summary$in_reference, ]
  # ref_rt of a non-reference group is its RT on the reference time scale
  expect_equal(new_groups$ref_rt, c(3.70, 6.50, 8.05, 9.80), tolerance = 1e-6)
  expect_equal(new_groups$n_matched, c(4, 1, 3, 2))
})

test_that("every group is padded with MISSING rows for absent samples", {
  fx <- make_peak_fixture()
  res <- suppressMessages(align_peaks_by_rt(fx$peaks, max_drift = 0.15))

  # P7 (12.00) is a reference-sample peak missing from S2
  p7 <- res$aligned[abs(res$aligned$ref_rt - 12.000) < 1e-6, ]
  expect_setequal(p7$sample, paste0("S", 1:5))
  expect_equal(p7$flag[p7$sample == "S2"], "MISSING")
  expect_true(is.na(p7$area[p7$sample == "S2"]))
  expect_equal(res$summary$n_missing[abs(res$summary$ref_rt - 12.000) < 1e-6], 1)

  # X2 (9.80) appears only in S2 and S4
  x2 <- res$aligned[abs(res$aligned$ref_rt - 9.80) < 1e-6, ]
  expect_setequal(x2$sample[x2$flag == "MISSING"], c("S1", "S3", "S5"))

  # Per-sample counts agree with the table
  expect_equal(res$drift_stats$n_missing,
               as.integer(table(factor(res$aligned$sample[res$aligned$flag == "MISSING"],
                                       levels = res$drift_stats$sample))))
  expect_equal(res$drift_stats$n_matched + res$drift_stats$n_ambiguous,
               res$drift_stats$n_peaks)
})

test_that("a sample's two peaks never share a group", {
  fx <- make_peak_fixture()
  # A generous tolerance would merge the close pair if this were not enforced
  res <- suppressMessages(align_peaks_by_rt(fx$peaks, max_drift = 0.15, rt_tol = 0.15))
  real <- res$aligned[!is.na(res$aligned$rt), ]
  expect_false(any(duplicated(real[c("peak_group", "sample")])))
  expect_true(all(c("P5", "P6") %in% recovered_true_peaks(res$aligned, fx$truth)))
})

# ==============================================================================
# Ambiguity flag
# ==============================================================================

test_that("clean, well-resolved data raises no ambiguity flags", {
  fx <- make_peak_fixture()
  res <- suppressMessages(align_peaks_by_rt(fx$peaks, max_drift = 0.15))
  expect_equal(sum(res$aligned$flag == "AMBIGUOUS"), 0)
  expect_true(all(is.na(res$aligned$flag_reason)))
})

test_that("a peak that could belong to either of two close groups is flagged", {
  mk <- function(name, rt) {
    data.frame(sample = name, peak = seq_along(rt), rt = rt, height = 1, area = 10)
  }
  # S3 has both members of a close pair (8.00, 8.03), which fixes two groups.
  # S4 has a single peak midway between them: RT alone cannot place it.
  peaks <- rbind(
    mk("S1", c(2, 5, 8.00, 11)),
    mk("S2", c(2, 5, 8.00, 11)),
    mk("S3", c(2, 5, 8.00, 8.03, 11)),
    mk("S4", c(2, 5, 8.015, 11))
  )
  res <- suppressMessages(align_peaks_by_rt(peaks, max_drift = 0.2, rt_tol = 0.05))
  al <- res$aligned

  s3 <- al[al$sample == "S3" & !is.na(al$rt) & al$rt > 7 & al$rt < 9, ]
  expect_equal(length(unique(s3$peak_group)), 2)

  s4 <- al[al$sample == "S4" & !is.na(al$rt) & al$rt > 7 & al$rt < 9, ]
  expect_equal(s4$flag, "AMBIGUOUS")
  expect_match(s4$flag_reason, "neighbouring peak group")

  # Nothing else is flagged, and flagged peaks are left out of n_matched
  expect_equal(sum(al$flag == "AMBIGUOUS"), 1)
  grp <- res$summary[res$summary$peak_group == s4$peak_group, ]
  expect_equal(grp$n_ambiguous, 1)
  expect_equal(res$drift_stats$n_ambiguous[res$drift_stats$sample == "S4"], 1)
})

# Regression tests for input handling -----------------------------------------

test_that("read_agilent_dad_peaks handles a reference wavelength in the header", {
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f))
  writeLines(c(
    "D:\\data\\a.d,DAD1 A, Sig=280,4 Ref=360,100 a.d",
    "D:\\data\\a.d,Peak,RT,Height,Area",
    "D:\\data\\a.d,1,2.5,10,100",
    "",
    "D:\\data\\b.d,DAD1 A, Sig=280,4 Ref=off b.d",
    "D:\\data\\b.d,Peak,RT,Height,Area",
    "D:\\data\\b.d,1,2.6,11,110"
  ), f)
  res <- suppressMessages(read_agilent_dad_peaks(f))
  expect_equal(res$sample, c("a", "b"))
  expect_equal(res$rt, c(2.5, 2.6))
  expect_type(res$peak, "integer")
})

test_that("align_peaks_by_rt validates its input", {
  fx <- make_peak_fixture()$peaks
  expect_error(align_peaks_by_rt(fx[c("sample", "rt")]), "missing required column")
  expect_error(suppressMessages(align_peaks_by_rt(fx, max_drift = -1)), "max_drift")
})

test_that("align_peaks_by_rt drops peaks with a missing retention time", {
  fx <- make_peak_fixture()$peaks
  expected <- suppressMessages(align_peaks_by_rt(fx, max_drift = 0.25))
  extra <- fx[1, ]
  extra$sample <- "S3"
  extra$rt <- NA_real_
  expect_warning(
    res <- suppressMessages(align_peaks_by_rt(rbind(fx, extra), max_drift = 0.25)),
    "missing retention time"
  )
  expect_equal(res, expected)
})

test_that("extra columns in peaks_long are carried through alignment", {
  fx <- make_peak_fixture()$peaks
  fx$note <- "x"
  res <- suppressMessages(align_peaks_by_rt(fx, max_drift = 0.25))
  expect_true("note" %in% names(res$aligned))
  base <- suppressMessages(align_peaks_by_rt(make_peak_fixture()$peaks, max_drift = 0.25))
  expect_equal(res$summary, base$summary)
})
