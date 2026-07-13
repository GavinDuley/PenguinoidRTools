# Synthetic peak-table fixture with known ground truth, for testing and
# benchmarking align_peaks_by_rt(). Deterministic (no random noise), so
# assertions about which peaks *should* group together are exact.
#
# The fixture models a small UV-DAD run: five samples (S1-S5), each measured
# with a small systematic retention-time drift. Nine "true" compounds are
# defined; each is present in a specified subset of samples. Two of them
# (P5 = 8.00, P6 = 8.05 min) form a deliberately nasty close pair whose
# per-sample observed RTs overlap once drift is applied -- the case the
# current greedy, single-reference aligner mishandles.

# Return list(peaks, truth):
#   peaks  - long data frame as produced by read_agilent_dad_peaks():
#            columns sample, peak, rt, height, area (the aligner's input).
#   truth  - ground-truth map: sample, true_peak, rt, peak.
make_peak_fixture <- function() {
  true_rt <- c(
    P1 = 2.10,   # well separated, all samples
    P2 = 3.50,   # well separated, all samples
    P3 = 3.70,   # absent from reference (S1); present in the rest
    P4 = 5.00,   # well separated, all samples
    P5 = 8.00,   # close pair (lower)
    P6 = 8.05,   # close pair (upper); absent from reference
    P7 = 12.00,  # missing from one sample (S2)
    X1 = 6.50,   # singleton: one sample only
    X2 = 9.80    # real peak absent from reference; two samples
  )
  presence <- list(
    P1 = c("S1", "S2", "S3", "S4", "S5"),
    P2 = c("S1", "S2", "S3", "S4", "S5"),
    P3 = c("S2", "S3", "S4", "S5"),
    P4 = c("S1", "S2", "S3", "S4", "S5"),
    P5 = c("S1", "S2", "S3", "S4", "S5"),
    P6 = c("S3", "S4", "S5"),
    P7 = c("S1", "S3", "S4", "S5"),
    X1 = c("S5"),
    X2 = c("S2", "S4")
  )
  # Per-sample systematic RT drift (minutes)
  offsets <- c(S1 = 0.000, S2 = 0.020, S3 = -0.030, S4 = 0.050, S5 = -0.040)

  rows <- list()
  for (tp in names(true_rt)) {
    for (s in presence[[tp]]) {
      rows[[length(rows) + 1L]] <- data.frame(
        sample    = s,
        true_peak = tp,
        rt        = round(true_rt[[tp]] + offsets[[s]], 4),
        stringsAsFactors = FALSE
      )
    }
  }
  truth <- do.call(rbind, rows)

  # Assign MassHunter-style peak indices in elution (RT) order within each
  # sample, plus deterministic positive height/area. split() orders samples
  # S1..S5, so S1 appears first and becomes the aligner's reference.
  peaks <- do.call(rbind, lapply(split(truth, truth$sample), function(d) {
    d <- d[order(d$rt), ]
    d$peak   <- seq_len(nrow(d))
    d$height <- round(500 - 20 * d$rt, 1)
    d$area   <- d$height * 10
    d
  }))
  rownames(peaks) <- NULL

  list(
    peaks = peaks[, c("sample", "peak", "rt", "height", "area")],
    truth = peaks[, c("sample", "true_peak", "rt", "peak")]
  )
}

# Attach the ground-truth compound label to each aligned row by matching on
# (sample, rt). NA-rt rows (padding for absent samples) get true_peak = NA.
label_aligned_with_truth <- function(aligned, truth) {
  key_truth <- paste(truth$sample, format(truth$rt, nsmall = 4))
  key_al    <- paste(aligned$sample, format(aligned$rt, nsmall = 4))
  aligned$true_peak <- truth$true_peak[match(key_al, key_truth)]
  aligned
}

# Names of the true compounds an aligner recovered exactly: all of the
# compound's observations fall in a single group, and that group contains no
# other compound.
recovered_true_peaks <- function(aligned, truth) {
  a <- label_aligned_with_truth(aligned, truth)
  a <- a[!is.na(a$rt) & !is.na(a$ref_rt), ]
  tps <- unique(a$true_peak)
  recovered <- vapply(tps, function(tp) {
    groups_for_tp <- unique(a$ref_rt[a$true_peak == tp])
    if (length(groups_for_tp) != 1) return(FALSE)
    all(a$true_peak[a$ref_rt == groups_for_tp] == tp)
  }, logical(1))
  sort(tps[recovered])
}

# Score an aligned table against ground truth. Returns a one-row data frame:
#   n_groups          - number of distinct peak groups (unique ref_rt)
#   n_impure_groups   - groups whose matched members span >1 true compound
#                       (i.e. two different compounds merged into one group)
#   n_fragmented      - true compounds split across >1 group
#   n_recovered       - true compounds recovered exactly (all and only their
#                       observations in a single pure group)
#   n_true_peaks      - total distinct true compounds in the fixture
score_alignment <- function(aligned, truth) {
  a <- label_aligned_with_truth(aligned, truth)
  a <- a[!is.na(a$rt) & !is.na(a$ref_rt), ]  # matched observations only

  by_group <- split(a$true_peak, a$ref_rt)
  n_impure  <- sum(vapply(by_group, function(tp) length(unique(tp)) > 1, logical(1)))

  by_truth  <- split(a$ref_rt, a$true_peak)
  n_frag    <- sum(vapply(by_truth, function(g) length(unique(g)) > 1, logical(1)))

  data.frame(
    n_groups        = length(by_group),
    n_impure_groups = n_impure,
    n_fragmented    = n_frag,
    n_recovered     = length(recovered_true_peaks(aligned, truth)),
    n_true_peaks    = length(unique(truth$true_peak)),
    stringsAsFactors = FALSE
  )
}
