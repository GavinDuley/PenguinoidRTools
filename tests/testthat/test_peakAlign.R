# Tests for read_agilent_dad_peaks() and align_peaks_by_rt().
#
# The alignment tests run against a deterministic synthetic fixture with known
# ground truth (see helper-peaks.R). They fall into two groups:
#
#   * GUARD tests      - behaviour the current aligner gets right. These must
#                        keep passing through any future rewrite.
#   * REWRITE TARGET   - behaviour the current greedy, single-reference aligner
#                        gets wrong. Each asserts the *current* (imperfect)
#                        outcome and carries a note on the correct outcome to
#                        assert once the reference-free, order-aware rewrite
#                        lands. When the rewrite starts passing these, the
#                        assertion fails loudly and is flipped.

library(testthat)
library(PenguinoidRTools)

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

  expect_named(res, c("aligned", "summary", "drift_stats"))
  expect_true(all(c("sample", "peak", "rt", "height", "area",
                    "ref_rt", "rt_diff", "flag", "flag_reason") %in%
                    names(res$aligned)))
  expect_true(all(c("ref_rt", "in_reference", "mean_rt", "sd_rt", "cv_pct",
                    "n_matched", "n_ambiguous", "n_missing") %in%
                    names(res$summary)))
  expect_true(all(res$aligned$flag %in% c("OK", "AMBIGUOUS", "UNMATCHED")))
})

test_that("align_peaks_by_rt requires at least two samples", {
  fx <- make_peak_fixture()
  one <- fx$peaks[fx$peaks$sample == "S1", ]
  expect_error(align_peaks_by_rt(one), "at least two samples")
})

test_that("the reference sample is the first sample and its peaks anchor groups", {
  fx <- make_peak_fixture()
  res <- suppressMessages(align_peaks_by_rt(fx$peaks, max_drift = 0.15))

  ref_rows <- res$aligned[res$aligned$sample == "S1", ]
  # Every reference-sample peak matches itself exactly
  expect_true(all(ref_rows$rt_diff == 0, na.rm = TRUE))
  expect_true(all(res$summary$ref_rt[res$summary$in_reference] %in%
                    fx$peaks$rt[fx$peaks$sample == "S1"]))
})

# ==============================================================================
# GUARD tests - well-behaved peaks the aligner must always get right
# ==============================================================================

test_that("well-separated peaks present in every sample are recovered cleanly", {
  fx <- make_peak_fixture()
  res <- suppressMessages(align_peaks_by_rt(fx$peaks, max_drift = 0.15))

  recovered <- recovered_true_peaks(res$aligned, fx$truth)
  # P1, P2, P4 are far from any neighbour and present in all five samples
  expect_true(all(c("P1", "P2", "P4") %in% recovered))

  # Each forms one group of five samples with a small, sane CV
  for (tp in c("P1", "P2", "P4")) {
    rrt <- unique(fx$truth$rt[fx$truth$true_peak == tp][1])
    grp <- res$summary[abs(res$summary$ref_rt - fx$truth$rt[fx$truth$true_peak == tp][1]) < 1e-6, ]
    expect_equal(grp$n_matched, 5)
    expect_lt(grp$cv_pct, 5)
  }
})

test_that("a peak absent from the reference is still recovered as its own group", {
  fx <- make_peak_fixture()
  res <- suppressMessages(align_peaks_by_rt(fx$peaks, max_drift = 0.15))

  # P3 (3.70) is missing from the reference S1 but clean in S2-S5
  recovered <- recovered_true_peaks(res$aligned, fx$truth)
  expect_true("P3" %in% recovered)

  p3 <- res$aligned[!is.na(res$aligned$rt), ]
  p3 <- label_aligned_with_truth(p3, fx$truth)
  p3 <- p3[p3$true_peak == "P3" & !is.na(p3$true_peak), ]
  expect_equal(sort(p3$sample), c("S2", "S3", "S4", "S5"))
  expect_equal(length(unique(p3$ref_rt)), 1)  # one group, not fragmented
})

test_that("a singleton peak and a two-sample non-reference peak form their own groups", {
  fx <- make_peak_fixture()
  res <- suppressMessages(align_peaks_by_rt(fx$peaks, max_drift = 0.15))
  recovered <- recovered_true_peaks(res$aligned, fx$truth)

  # X1 (6.50) appears in S5 only; X2 (9.80) in S2 and S4, neither in reference
  expect_true(all(c("X1", "X2") %in% recovered))
})

test_that("the current aligner recovers exactly the well-separated compounds (scorecard snapshot)", {
  fx <- make_peak_fixture()
  res <- suppressMessages(align_peaks_by_rt(fx$peaks, max_drift = 0.15))
  score <- score_alignment(res$aligned, fx$truth)

  # Baseline snapshot of the CURRENT greedy, single-reference aligner. The
  # reference-free, order-aware rewrite should push n_recovered to 9 and both
  # n_impure_groups and n_fragmented to 0. Update this snapshot when it does.
  expect_equal(score$n_true_peaks, 9)
  expect_equal(score$n_recovered, 7)      # REWRITE TARGET: should become 9
  expect_equal(score$n_impure_groups, 2)  # REWRITE TARGET: should become 0
  expect_equal(score$n_fragmented, 2)     # REWRITE TARGET: should become 0
})

# ==============================================================================
# REWRITE TARGET tests - concrete failures of the current algorithm
# ==============================================================================

test_that("REWRITE TARGET: the close pair P5/P6 is mis-grouped", {
  fx <- make_peak_fixture()
  res <- suppressMessages(align_peaks_by_rt(fx$peaks, max_drift = 0.15))
  recovered <- recovered_true_peaks(res$aligned, fx$truth)

  # The 8.00/8.05 close pair overlaps once per-sample drift is applied, and the
  # greedy matcher splits each compound across two mixed groups.
  #
  # After the rewrite (order-aware, reference-free) both should be recovered;
  # flip these to expect_true() then.
  expect_false("P5" %in% recovered)
  expect_false("P6" %in% recovered)

  # Concretely: the ~8.0 min region yields >= 1 group mixing P5 and P6 members.
  al <- label_aligned_with_truth(res$aligned[!is.na(res$aligned$rt), ], fx$truth)
  near8 <- al[al$ref_rt > 7.5 & al$ref_rt < 8.5, ]
  mixed <- tapply(near8$true_peak, near8$ref_rt, function(tp) length(unique(tp)) > 1)
  expect_true(any(mixed))  # REWRITE TARGET: should be !any(mixed)
})

test_that("REWRITE TARGET: a mis-grouped close pair can show a deceptively low CV", {
  fx <- make_peak_fixture()
  res <- suppressMessages(align_peaks_by_rt(fx$peaks, max_drift = 0.15))

  # The reference group at ref_rt = 8.000 grabbed a tight but WRONG cluster
  # (mixing P5 and P6), so its CV looks excellent despite being incorrect.
  # This is why CV alone can't be trusted to flag bad alignment.
  grp8 <- res$summary[abs(res$summary$ref_rt - 8.000) < 1e-6, ]
  expect_equal(grp8$n_matched, 5)
  expect_lt(grp8$cv_pct, 0.5)  # looks great, but the group is impure
})

test_that("REWRITE TARGET: reference groups do not pad absent samples with NA rows", {
  fx <- make_peak_fixture()
  res <- suppressMessages(align_peaks_by_rt(fx$peaks, max_drift = 0.15))

  # P7 (12.00) is a reference-sample peak missing from S2. Reference groups
  # (unlike second-pass "new" groups) get no NA row for the absent sample, and
  # the summary reports n_missing = 0 -- so the group looks complete at four
  # samples when one is genuinely absent.
  p7 <- res$aligned[!is.na(res$aligned$ref_rt) &
                      abs(res$aligned$ref_rt - 12.000) < 1e-6, ]
  expect_false("S2" %in% p7$sample)                 # REWRITE TARGET: S2 present as NA row
  grp7 <- res$summary[abs(res$summary$ref_rt - 12.000) < 1e-6, ]
  expect_equal(grp7$n_missing, 0)                   # REWRITE TARGET: should be 1

  # Contrast: a second-pass group DOES pad. X2 (9.80) appears only in S2, S4.
  x2 <- res$aligned[!is.na(res$aligned$ref_rt) &
                      abs(res$aligned$ref_rt - 9.835) < 1e-6, ]
  expect_true(all(c("S1", "S3", "S5") %in% x2$sample[is.na(x2$rt)]))
})

test_that("REWRITE TARGET: the ambiguity flag does not fire on the genuinely ambiguous close pair", {
  fx <- make_peak_fixture()
  res <- suppressMessages(align_peaks_by_rt(fx$peaks, max_drift = 0.15))

  # No row is flagged AMBIGUOUS even though the P5/P6 assignment is exactly the
  # ambiguous case manual review should catch. The rewrite's flagging should
  # surface this region.
  expect_equal(sum(res$aligned$flag == "AMBIGUOUS"), 0)  # REWRITE TARGET: should be > 0
})
