# peakTableAlign.R
# Agilent MassHunter UV-DAD peak table import and RT alignment for PenguinoidRTools
# https://github.com/GavinDuley/PenguinoidRTools
# Copyright (c) 2024 onwards, Gavin Duley
# Licenced under the GPL-3.0 licence

# ==============================================================================
# read_agilent_dad_peaks — parser
# ==============================================================================

#' Read an Agilent MassHunter UV-DAD peak table export
#'
#' Parses the multi-sample peak table file exported from Agilent MassHunter
#' Quantitative Analysis for UV-DAD data. Returns a long-format data frame
#' with one row per detected peak per sample, ready to pass to
#' \code{\link{align_peaks_by_rt}}.
#'
#' @param file_path Character. Path to the exported CSV or tab-delimited file.
#' @param sep Character or \code{NULL}. Field separator. \code{NULL} (default)
#'   auto-detects tab vs comma from the first non-blank line.
#' @param col_peak Character. Column name for the peak index in the export
#'   header row (default \code{"Peak"}).
#' @param col_rt Character. Column name for retention time (default
#'   \code{"RT"}).
#' @param col_height Character. Column name for peak height (default
#'   \code{"Height"}).
#' @param col_area Character. Column name for peak area (default
#'   \code{"Area"}).
#'
#' @return A long-format data frame with one row per detected peak:
#'   \describe{
#'     \item{sample}{Sample name (filename stem; path and \code{.d} suffix
#'       removed).}
#'     \item{peak}{Peak index as assigned by MassHunter (integer).}
#'     \item{rt}{Retention time (numeric, minutes).}
#'     \item{height}{Peak height (numeric).}
#'     \item{area}{Peak area (numeric).}
#'   }
#'   Sample order in the output matches order of appearance in the file.
#'   The first sample is used as the reference for drift estimation by
#'   \code{\link{align_peaks_by_rt}}, so file order matters.
#'
#' @details
#' **File format.** The export contains one block of rows per sample, separated
#' by blank lines. Each block starts with a sample-header line (containing
#' \code{DAD} and \code{Sig=}), followed by a column-name row, then one row per
#' detected peak. The first field of every row is the data-file path.
#'
#' **Separator auto-detection.** The first non-blank line is inspected; if it
#' contains more tab characters than commas the file is treated as
#' tab-delimited, otherwise as comma-separated. Override with \code{sep} if
#' needed.
#'
#' **Sample name.** Extracted from the text after the \code{Ref=} setting on
#' the sample-header line, with the \code{.d} suffix removed. For example,
#' \code{Ref=off 10.PBDEAL_CTRL_RT_0M_2.d} yields
#' \code{10.PBDEAL_CTRL_RT_0M_2}. A reference wavelength
#' (e.g. \code{Ref=360,100}) is handled in the same way as \code{Ref=off}.
#'
#' **Column positions.** The column-name row within each sample block is used
#' to locate \code{col_rt}, \code{col_height}, and \code{col_area} by name,
#' so the function is robust to exports with different column ordering or
#' subsets.
#'
#' @seealso \code{\link{align_peaks_by_rt}}
#'
#' @examples
#' \dontrun{
#' peaks_long <- read_agilent_dad_peaks("polyphenols_peaktable.csv")
#' result     <- align_peaks_by_rt(peaks_long, max_drift = 0.25)
#' result$summary      # mean RT, SD, CV% per peak group
#' result$drift_stats  # per-sample drift vs reference
#' }
#'
#' @export
read_agilent_dad_peaks <- function(
    file_path,
    sep        = NULL,
    col_peak   = "Peak",
    col_rt     = "RT",
    col_height = "Height",
    col_area   = "Area"
) {

  raw <- readLines(file_path, warn = FALSE)

  # Auto-detect separator from the first non-blank line
  if (is.null(sep)) {
    first_line <- raw[which(nchar(trimws(raw)) > 0)[1]]
    n_tab   <- nchar(first_line) - nchar(gsub("\t", "", first_line, fixed = TRUE))
    n_comma <- nchar(first_line) - nchar(gsub(",",  "", first_line, fixed = TRUE))
    sep <- if (n_tab >= n_comma) "\t" else ","
    message(sprintf("Detected separator: %s", if (sep == "\t") "tab" else "comma"))
  }

  is_blank     <- function(x) grepl("^[\\t ,\"]*$", x, perl = TRUE)
  strip_quotes <- function(x) gsub('^"|"$', "", trimws(x))

  # Parsed fields are collected as character vectors and converted once at
  # the end; building a data frame per peak is very slow on large exports.
  out_sample <- out_peak <- out_rt <- out_height <- out_area <- list()
  n_out          <- 0L
  current_sample <- NA_character_
  col_idx        <- NULL   # named integer: column name -> position

  for (line in raw) {

    if (is_blank(line)) {
      current_sample <- NA_character_
      col_idx        <- NULL
      next
    }

    fields <- strip_quotes(strsplit(line, sep, fixed = TRUE)[[1]])

    # Sample-header line: contains both "DAD" and "Sig=" anywhere in the line.
    # We test the raw line (not a single split field) so that CSV files with
    # "Sig=280,4" split across two fields are still detected correctly.
    if (grepl("DAD", line, fixed = TRUE) && grepl("Sig=", line, fixed = TRUE)) {
      sample_raw     <- sub(".*Ref=\\S+\\s+", "", line)
      sample_raw     <- gsub('[",\\s]+$', "", trimws(sample_raw), perl = TRUE)
      current_sample <- sub("\\.d$", "", trimws(sample_raw))
      col_idx        <- NULL
      next
    }

    # Column-header line: second field equals the peak column name exactly.
    if (length(fields) >= 2 && fields[2] == col_peak) {
      col_idx <- stats::setNames(seq_along(fields), fields)
      next
    }

    # Data line: sample and column header both known; second field is an integer.
    if (!is.na(current_sample) && !is.null(col_idx) && length(fields) >= 2) {
      if (!grepl("^[0-9]+$", fields[2])) next

      get_field <- function(col_name) {
        if (col_name %in% names(col_idx)) fields[col_idx[[col_name]]] else NA_character_
      }

      n_out <- n_out + 1L
      out_sample[[n_out]] <- current_sample
      out_peak[[n_out]]   <- get_field(col_peak)
      out_rt[[n_out]]     <- get_field(col_rt)
      out_height[[n_out]] <- get_field(col_height)
      out_area[[n_out]]   <- get_field(col_area)
    }
  }

  if (n_out == 0L) {
    stop(
      "No peak data found in '", file_path, "'. ",
      "Ensure the file is a MassHunter UV-DAD peak table export."
    )
  }

  data.frame(
    sample = unlist(out_sample),
    peak   = as.integer(unlist(out_peak)),
    rt     = as.numeric(unlist(out_rt)),
    height = as.numeric(unlist(out_height)),
    area   = as.numeric(unlist(out_area)),
    stringsAsFactors = FALSE
  )
}

# ==============================================================================
# align_peaks_by_rt — aligner
# ==============================================================================

# Internal: anchor pairs between one sample's RTs `x` and the reference RTs
# `ref` (both sorted ascending). Every sample/reference pair within
# `max_drift` is a candidate. The anchors are the best chain of candidates
# that preserves elution order in both samples (so each peak is used at most
# once) and along which the RT difference changes smoothly: each pair in the
# chain scores 1, less a penalty proportional to the change in RT difference
# from the previous pair. This follows the sample's drift along the run and
# passes over chance pairings, which would need a jump in drift. Found exactly
# by dynamic programming. Returns a two-column integer matrix (index into x,
# index into ref).
.chain_anchors <- function(x, ref, max_drift, scale) {
  none <- matrix(integer(0), ncol = 2)
  if (length(x) == 0 || length(ref) == 0) return(none)
  cand <- which(abs(outer(x, ref, FUN = "-")) <= max_drift, arr.ind = TRUE)
  if (nrow(cand) == 0) return(none)
  cand <- cand[order(cand[, 1], cand[, 2]), , drop = FALSE]
  pi <- cand[, 1]
  pj <- cand[, 2]
  pd <- x[pi] - ref[pj]
  n_pairs <- length(pi)

  # `scale` is the change in RT difference that cancels the gain of one pair
  max_cost <- 2                           # cap, so a chain can resume after a gap
  lookback <- 30L                         # sample peaks that may be skipped

  first_of <- match(seq_along(x), pi)     # first candidate of each sample peak
  score <- numeric(n_pairs)
  back  <- integer(n_pairs)
  for (p in seq_len(n_pairs)) {
    score[p] <- 1
    if (pi[p] == 1L) next
    lo_peak <- max(1L, pi[p] - lookback)
    from <- first_of[lo_peak:(pi[p] - 1L)]
    from <- from[!is.na(from)]
    if (length(from) == 0) next
    q <- from[1]:(first_of[pi[p]] - 1L)   # candidates of earlier sample peaks
    q <- q[pj[q] < pj[p]]
    if (length(q) == 0) next
    val <- score[q] - pmin(abs(pd[p] - pd[q]) / scale, max_cost)
    k <- which.max(val)
    if (val[k] > 0) {
      score[p] <- 1 + val[k]
      back[p]  <- q[k]
    }
  }

  chain <- integer(0)
  p <- which.max(score)
  while (p > 0L) {
    chain <- c(p, chain)
    p <- back[p]
  }
  cand[chain, , drop = FALSE]
}

# Internal: signed residuals (x minus ref) of the mutual nearest-neighbour
# pairs between drift-corrected sample RTs `x` and reference RTs `ref`, for
# pairs no further apart than `max_d`. Unlike the chain anchors these are not
# selected for smoothness, so their spread is an honest measure of the
# scatter left after drift correction.
.mnn_resid <- function(x, ref, max_d) {
  if (length(x) == 0 || length(ref) == 0) return(numeric(0))
  d <- abs(outer(x, ref, FUN = "-"))
  nn_ref <- max.col(-d, ties.method = "first")
  nn_x   <- max.col(-t(d), ties.method = "first")
  i <- which(nn_x[nn_ref] == seq_along(x))
  r <- x[i] - ref[nn_ref[i]]
  r[abs(r) <= max_d]
}

# Internal: estimate the retention-time drift of one sample relative to the
# reference. `x` are the sample's raw RTs, `ref` the reference RTs (sorted).
# Returns a list with `fn` (function giving the drift at a raw RT),
# `n_anchors`, `model` ("smooth", "linear", "constant" or "none") and `resid`
# (anchor residuals after correction, used to choose the grouping tolerance).
.estimate_drift <- function(x, ref, max_drift, drift_model) {
  zero <- function(rt) rep(0, length(rt))
  if (drift_model == "none") {
    return(list(fn = zero, n_anchors = 0L, model = "none", resid = numeric(0)))
  }

  xs <- sort(x)

  # Fit the drift model to a set of anchors (index into xs, index into ref).
  # With "auto" the model grows with the evidence: one offset (fewer than 4
  # anchors), a robust straight line in RT (4 or more), plus a smooth curve
  # for what the line leaves over (10 or more). Beyond the outermost anchors
  # the smooth model is held constant rather than extrapolated.
  fit_model <- function(anchors) {
    ax <- xs[anchors[, 1]]
    ad <- ax - ref[anchors[, 2]]
    if (drift_model == "auto" && length(unique(ax)) >= 4) {
      # Theil-Sen line: median of pairwise slopes, robust to a few bad anchors
      k <- if (length(ax) > 300) round(seq(1, length(ax), length.out = 300)) else
        seq_along(ax)
      dx <- outer(ax[k], ax[k], FUN = "-")
      dy <- outer(ad[k], ad[k], FUN = "-")
      slope     <- stats::median(dy[dx > 0] / dx[dx > 0])
      intercept <- stats::median(ad - slope * ax)
      if (length(ax) >= 10) {
        span <- min(2 / 3, max(0.2, 15 / length(ax)))
        lw   <- stats::lowess(ax, ad - (intercept + slope * ax), f = span, iter = 3)
        smooth_part <- stats::approxfun(lw$x, lw$y, rule = 2, ties = mean)
        lo <- min(ax)
        hi <- max(ax)
        fn <- function(rt) {
          rt <- pmin(pmax(rt, lo), hi)
          intercept + slope * rt + smooth_part(rt)
        }
        return(list(fn = fn, model = "smooth"))
      }
      return(list(fn = function(rt) intercept + slope * rt, model = "linear"))
    }
    med <- stats::median(ad)
    list(fn = function(rt) rep(med, length(rt)), model = "constant")
  }

  scale   <- max(max_drift / 10, 0.005)
  anchors <- .chain_anchors(xs, ref, max_drift, scale)
  if (nrow(anchors) == 0) {
    return(list(fn = zero, n_anchors = 0L, model = "none", resid = numeric(0)))
  }
  fit   <- fit_model(anchors)
  resid <- .mnn_resid(xs - fit$fn(xs), ref, max_drift)

  # If the peak-to-peak scatter turns out to be large relative to the
  # smoothness the chain demanded, the chain will have skipped genuine pairs;
  # repeat it once with a tolerance matched to the scatter.
  noise <- if (length(resid) >= 5) stats::mad(resid) else 0
  if (3 * noise > scale) {
    anchors2 <- .chain_anchors(xs, ref, max_drift, min(3 * noise, max_drift))
    if (nrow(anchors2) >= nrow(anchors)) {
      anchors <- anchors2
      fit     <- fit_model(anchors)
      resid   <- .mnn_resid(xs - fit$fn(xs), ref, max_drift)
    }
  }

  list(fn = fit$fn, n_anchors = nrow(anchors), model = fit$model, resid = resid)
}

# Internal: group drift-corrected RTs `x` into peak groups. `smp` is an integer
# sample index per peak. Because the corrected RTs lie on one axis, each group
# is a run of neighbouring values, which also keeps the elution order of the
# peaks within every sample. Groups must (a) contain at most one peak per
# sample and (b) span no more than `tol`. Among all valid partitions the one
# minimising (within-group sum of squares + penalty per group) is found
# exactly by dynamic programming. The penalty makes a few peaks within `tol`
# join one group, while two well-populated clusters are kept apart when they
# are clearly separated. Returns an integer group id per peak, numbered in
# RT order.
.group_peaks_1d <- function(x, smp, tol, n_samples) {
  n <- length(x)
  o <- order(x)
  xs <- x[o]
  ss <- smp[o]
  lambda <- n_samples * tol^2 / 8

  # Position of the previous peak from the same sample (0 if none)
  prev <- integer(n)
  last <- integer(n_samples)
  for (k in seq_len(n)) {
    prev[k] <- last[ss[k]]
    last[ss[k]] <- k
  }

  grp_sorted <- integer(n)
  n_groups   <- 0L
  # A gap wider than tol can never be bridged, so each stretch between such
  # gaps is solved on its own.
  region_id <- cumsum(c(TRUE, diff(xs) > tol))
  starts <- which(!duplicated(region_id))
  ends   <- c(starts[-1] - 1L, n)

  for (r in seq_along(starts)) {
    lo <- starts[r]
    m  <- ends[r] - lo + 1L
    if (m == 1L) {
      n_groups <- n_groups + 1L
      grp_sorted[lo] <- n_groups
      next
    }
    xr <- xs[lo:ends[r]] - xs[lo]            # centred for numerical accuracy
    pr <- pmax(prev[lo:ends[r]] - lo + 1L, 0L)
    c1 <- c(0, cumsum(xr))
    c2 <- c(0, cumsum(xr^2))
    best <- c(0, rep(Inf, m))                # best[j + 1]: optimal cost of 1..j
    back <- integer(m)
    for (j in seq_len(m)) {
      latest_prev <- 0L
      for (i in j:1) {
        if (pr[i] > latest_prev) latest_prev <- pr[i]
        if (latest_prev >= i) break          # a sample would appear twice
        if (xr[j] - xr[i] > tol + 1e-9) break
        cnt  <- j - i + 1L
        sse  <- (c2[j + 1L] - c2[i]) - (c1[j + 1L] - c1[i])^2 / cnt
        cost <- best[i] + lambda + max(sse, 0)
        if (cost < best[j + 1L]) {
          best[j + 1L] <- cost
          back[j] <- i
        }
      }
    }
    # Trace back the group boundaries
    bounds <- integer(0)
    j <- m
    while (j >= 1L) {
      bounds <- c(back[j], bounds)
      j <- back[j] - 1L
    }
    is_start <- logical(m)
    is_start[bounds] <- TRUE
    grp_sorted[lo:ends[r]] <- n_groups + cumsum(is_start)
    n_groups <- n_groups + length(bounds)
  }

  grp <- integer(n)
  grp[o] <- grp_sorted
  grp
}

#' Align peaks across samples by retention time
#'
#' Aligns peaks across samples in three steps: (1) the retention-time drift of
#' each sample is estimated relative to the reference sample (the first sample
#' in \code{peaks_long}) and removed; (2) the drift-corrected retention times
#' of all samples are grouped into peak groups, with at most one peak per
#' sample in a group; (3) peaks whose assignment is a close call are flagged
#' for manual review. Peaks that are absent from the reference sample form
#' groups in exactly the same way as those that are present.
#'
#' @param peaks_long Long-format data frame from
#'   \code{\link{read_agilent_dad_peaks}}, with columns \code{sample},
#'   \code{peak}, \code{rt}, \code{height} and \code{area}. Any further
#'   columns are carried through.
#' @param max_drift Numeric. The largest retention-time shift (minutes)
#'   expected between a sample and the reference for the same compound. It
#'   limits which peak pairs are considered when estimating each sample's
#'   drift; it is no longer the width of the matching window. Default
#'   \code{0.5}.
#' @param rt_tol Numeric or \code{NULL}. The largest spread (minutes) of
#'   drift-corrected retention times allowed within one peak group. If
#'   \code{NULL} (default) it is estimated from the scatter left after drift
#'   correction (eight robust standard deviations of the RT differences
#'   between each sample's peaks and their nearest reference peaks, but at
#'   least \code{max_drift / 20} and at most \code{max_drift}). The value used is reported in a message and returned
#'   in \code{params}.
#' @param drift_model Character. How drift is modelled for each sample:
#'   \code{"auto"} (default) lets drift vary along the run when a sample has
#'   enough anchor peaks (a straight line in RT with four or more anchors,
#'   plus a smooth curve with ten or more) and otherwise uses a single offset;
#'   \code{"constant"} always uses a single offset per sample; \code{"none"}
#'   applies no drift correction (then \code{rt_tol} defaults to
#'   \code{max_drift}).
#'
#' @return A named list with four elements:
#'   \describe{
#'     \item{\code{aligned}}{Long-format data frame with one row for every
#'       combination of peak group and sample. Columns: \code{sample},
#'       \code{peak}, \code{rt}, \code{height}, \code{area} (as supplied;
#'       \code{NA} where the sample has no peak in the group),
#'       \code{peak_group} (integer group number in elution order),
#'       \code{ref_rt} (the group's retention time on the reference sample's
#'       time scale: the reference sample's own peak RT if it has one in the
#'       group, otherwise the mean drift-corrected RT of the group),
#'       \code{rt_corrected} (RT after removing the sample's drift),
#'       \code{rt_diff} (raw \code{rt} minus \code{ref_rt}, i.e. including
#'       drift), \code{flag} (\code{"OK"}, \code{"AMBIGUOUS"} or
#'       \code{"MISSING"}) and \code{flag_reason} (plain-English explanation
#'       for ambiguous rows).}
#'     \item{\code{summary}}{One row per peak group, with \code{peak_group},
#'       \code{ref_rt}, \code{in_reference} (whether the reference sample has
#'       a peak in the group), mean RT, SD and CV\% of the raw retention times
#'       of the \code{"OK"} peaks, and \code{n_matched} (\code{"OK"} peaks),
#'       \code{n_ambiguous} and \code{n_missing} (samples with no peak in the
#'       group).}
#'     \item{\code{drift_stats}}{One row per sample: \code{n_peaks},
#'       \code{n_matched}, \code{n_ambiguous}, \code{n_missing}, the mean, SD,
#'       mean absolute and maximum absolute \code{rt_diff} of its \code{"OK"}
#'       peaks, the number of anchor peaks used to estimate its drift
#'       (\code{n_anchors}) and the drift model applied (\code{drift_model}).}
#'     \item{\code{params}}{The settings used: \code{reference},
#'       \code{max_drift}, \code{rt_tol} and \code{drift_model}.}
#'   }
#'
#' @details
#' **Drift estimation.** For each sample, every pairing of one of its peaks
#' with a reference peak within \code{max_drift} is a candidate. The anchors
#' are the longest chain of candidates that keeps the elution order in both
#' samples and along which the RT difference changes smoothly; chance
#' pairings would need a jump in drift and are passed over. A drift model is
#' then fitted to the anchors. With \code{drift_model = "auto"} the model depends
#' on the number of anchors: the median anchor difference (fewer than four),
#' a robust straight line in RT (four or more), and additionally a robust
#' smooth curve (\code{\link[stats]{lowess}}) for what the line leaves over
#' (ten or more), so that drift may change along the run. The model used for
#' each sample is reported in \code{drift_stats}. A sample with no anchors
#' gets a warning.
#'
#' **Grouping.** The drift-corrected retention times of all samples are
#' partitioned into groups such that no group contains two peaks from the same
#' sample or spans more than \code{rt_tol}. Among all such partitions, the one
#' with the smallest within-group scatter (plus a penalty per group, so that
#' peaks are not split needlessly) is found exactly. Because groups are
#' consecutive stretches of the corrected RT axis, the elution order of peaks
#' within each sample is always respected. A sample containing two close peaks
#' therefore forces them into two groups, which is what separates close pairs.
#'
#' **Ambiguity flag.** A peak is flagged \code{AMBIGUOUS} when it could
#' plausibly belong to a neighbouring group instead: its corrected RT is also
#' within \code{rt_tol} of that group, the group has no peak from the same
#' sample, and the peak is not clearly closer to the other members of its own
#' group (a peak that is alone in its group never is).
#' Ambiguous peaks stay in their assigned group but are left out of the
#' group's mean, SD and CV\%.
#'
#' **Limits.** Retention time alone cannot separate two compounds whose
#' spacing is similar to the random peak-to-peak scatter remaining after
#' drift correction. Such regions are flagged rather than resolved, and
#' should be checked by hand (e.g. against spectra). The result also depends
#' on the choice of reference sample for drift estimation: a reference with
#' few peaks gives few anchors.
#'
#' @seealso \code{\link{read_agilent_dad_peaks}}
#'
#' @examples
#' \dontrun{
#' peaks_long <- read_agilent_dad_peaks("polyphenols_peaktable.csv")
#' result     <- align_peaks_by_rt(peaks_long, max_drift = 0.25)
#'
#' result$aligned      # full table with flags
#' result$summary      # mean RT, SD, CV% per peak group
#' result$drift_stats  # per-sample drift statistics
#' result$params       # settings used, including the estimated rt_tol
#'
#' # Pivot to wide format for further analysis
#' library(tidyr)
#' area_wide <- tidyr::pivot_wider(
#'   result$aligned,
#'   id_cols     = c(peak_group, ref_rt),
#'   names_from  = sample,
#'   values_from = area
#' )
#' }
#'
#' @importFrom stats sd
#' @export
align_peaks_by_rt <- function(peaks_long, max_drift = 0.5, rt_tol = NULL,
                              drift_model = c("auto", "constant", "none")) {

  drift_model <- match.arg(drift_model)

  required_cols <- c("sample", "peak", "rt", "height", "area")
  missing_cols  <- setdiff(required_cols, names(peaks_long))
  if (length(missing_cols) > 0) {
    stop("peaks_long is missing required column(s): ",
         paste(missing_cols, collapse = ", "))
  }
  reserved <- intersect(c("peak_group", "ref_rt", "rt_corrected", "rt_diff",
                          "flag", "flag_reason"), names(peaks_long))
  if (length(reserved) > 0) {
    stop("peaks_long already has column(s) that align_peaks_by_rt() creates: ",
         paste(reserved, collapse = ", "),
         ". Pass the peak table, not a previous alignment result.")
  }
  if (!is.numeric(max_drift) || length(max_drift) != 1 || is.na(max_drift) ||
      max_drift < 0) {
    stop("'max_drift' must be a single non-negative number (minutes).")
  }
  if (!is.null(rt_tol) && (!is.numeric(rt_tol) || length(rt_tol) != 1 ||
                           is.na(rt_tol) || rt_tol < 0)) {
    stop("'rt_tol' must be NULL or a single non-negative number (minutes).")
  }

  # A peak without a retention time cannot be aligned.
  na_rt <- is.na(peaks_long$rt)
  if (any(na_rt)) {
    warning("Dropping ", sum(na_rt), " peak(s) with a missing retention time.")
    peaks_long <- peaks_long[!na_rt, ]
  }
  peaks_long <- as.data.frame(peaks_long)
  rownames(peaks_long) <- NULL

  smp_chr <- as.character(peaks_long$sample)
  samples <- unique(smp_chr)

  if (length(samples) < 2) {
    stop("Need at least two samples to align.")
  }

  ref_sample <- samples[1]
  ref_rts    <- sort(peaks_long$rt[smp_chr == ref_sample])

  message(sprintf("Reference sample: %s  (%d peaks)", ref_sample, length(ref_rts)))

  # --------------------------------------------------------------------------
  # Step 1: estimate and remove each sample's drift relative to the reference
  # --------------------------------------------------------------------------

  rt_corrected <- peaks_long$rt
  n_anchors    <- stats::setNames(integer(length(samples)), samples)
  model_used   <- stats::setNames(character(length(samples)), samples)
  resid_all    <- numeric(0)

  n_anchors[ref_sample]  <- length(ref_rts)
  model_used[ref_sample] <- "reference"

  for (smp in samples[-1]) {
    idx <- which(smp_chr == smp)
    est <- .estimate_drift(peaks_long$rt[idx], ref_rts, max_drift, drift_model)
    rt_corrected[idx] <- peaks_long$rt[idx] - est$fn(peaks_long$rt[idx])
    n_anchors[smp]    <- est$n_anchors
    model_used[smp]   <- est$model
    resid_all         <- c(resid_all, est$resid)
  }

  no_anchor <- samples[-1][n_anchors[samples[-1]] == 0]
  if (drift_model != "none" && length(no_anchor) > 0) {
    warning("No unambiguous anchor peaks were found for: ",
            paste(no_anchor, collapse = ", "),
            ". Their drift could not be refined; check max_drift and the ",
            "choice of reference sample.")
  }

  # Grouping tolerance: from the scatter left after correction unless given
  if (is.null(rt_tol)) {
    if (drift_model != "none" && length(resid_all) >= 5) {
      rt_tol <- min(max_drift, max(8 * stats::mad(resid_all), max_drift / 20))
    } else {
      rt_tol <- max_drift
    }
  }

  # --------------------------------------------------------------------------
  # Step 2: group the drift-corrected retention times
  # --------------------------------------------------------------------------

  grp <- .group_peaks_1d(rt_corrected, match(smp_chr, samples), rt_tol,
                         length(samples))
  n_groups <- max(grp)

  # Group RT on the reference time scale: the reference sample's own peak if
  # it has one in the group, otherwise the mean corrected RT of the members.
  group_rt <- as.numeric(tapply(rt_corrected, grp, mean))
  is_ref   <- smp_chr == ref_sample
  in_reference <- seq_len(n_groups) %in% grp[is_ref]
  group_rt[grp[is_ref]] <- peaks_long$rt[is_ref]

  # --------------------------------------------------------------------------
  # Step 3: flag close calls
  # --------------------------------------------------------------------------
  # A peak is ambiguous when it is also within rt_tol of a neighbouring
  # group's RT, that group has no peak from the same sample, and the peak is
  # not clearly closer to the rest of its own group (the neighbour is less
  # than three times as far away as the mean of the other members). A peak
  # alone in its group has no such support, so any neighbour within rt_tol
  # makes it ambiguous.

  n_peaks_all <- nrow(peaks_long)
  flag        <- rep("OK", n_peaks_all)
  flag_reason <- rep(NA_character_, n_peaks_all)
  member_key  <- paste(grp, smp_chr, sep = "\r")
  grp_n       <- tabulate(grp, n_groups)
  grp_sum     <- as.numeric(tapply(rt_corrected, grp, sum))
  d_own       <- ifelse(grp_n[grp] > 1,
                        abs(rt_corrected - (grp_sum[grp] - rt_corrected) /
                              pmax(grp_n[grp] - 1, 1)),
                        Inf)

  for (step in c(-1L, 1L)) {
    other  <- grp + step
    valid  <- other >= 1L & other <= n_groups
    d_oth  <- rep(Inf, n_peaks_all)
    d_oth[valid] <- abs(rt_corrected[valid] - group_rt[other[valid]])
    taken  <- paste(other, smp_chr, sep = "\r") %in% member_key
    amb    <- valid & d_oth <= rt_tol & d_oth < 3 * d_own & !taken & flag == "OK"
    flag[amb] <- "AMBIGUOUS"
    flag_reason[amb] <- sprintf(
      "Corrected RT %.3f is within rt_tol (%.3f min) of the neighbouring peak group at %.3f min as well as its own at %.3f min \u2014 manual review needed",
      rt_corrected[amb], rt_tol, group_rt[other[amb]], group_rt[grp[amb]]
    )
  }

  aligned <- peaks_long
  aligned$peak_group   <- grp
  aligned$ref_rt       <- group_rt[grp]
  aligned$rt_corrected <- rt_corrected
  aligned$rt_diff      <- aligned$rt - aligned$ref_rt
  aligned$flag         <- flag
  aligned$flag_reason  <- flag_reason

  # One row per group x sample: add MISSING rows for samples without a peak
  full_grid <- expand.grid(sample = samples, peak_group = seq_len(n_groups),
                           stringsAsFactors = FALSE)
  absent <- !(paste(full_grid$peak_group, full_grid$sample, sep = "\r") %in%
                member_key)
  if (any(absent)) {
    # All-NA rows with the same columns and types as `aligned`, so any extra
    # columns in peaks_long are carried through.
    na_rows            <- aligned[rep(NA_integer_, sum(absent)), ]
    na_rows$sample     <- full_grid$sample[absent]
    na_rows$peak_group <- full_grid$peak_group[absent]
    na_rows$ref_rt     <- group_rt[na_rows$peak_group]
    na_rows$flag       <- "MISSING"
    aligned <- rbind(aligned, na_rows)
  }
  aligned <- aligned[order(aligned$peak_group, as.character(aligned$sample)), ]
  rownames(aligned) <- NULL

  # --------------------------------------------------------------------------
  # Summary table: one row per peak group
  # --------------------------------------------------------------------------

  ok_rows <- aligned[aligned$flag == "OK", ]
  count_by_group <- function(which_flag) {
    as.integer(table(factor(aligned$peak_group[aligned$flag == which_flag],
                            levels = seq_len(n_groups))))
  }
  n_matched <- count_by_group("OK")
  ok_group  <- factor(ok_rows$peak_group, levels = seq_len(n_groups))
  mean_rt   <- as.numeric(tapply(ok_rows$rt, ok_group, mean))
  sd_rt     <- as.numeric(tapply(ok_rows$rt, ok_group, stats::sd))
  mean_rt[n_matched == 0] <- group_rt[n_matched == 0]

  summary_tbl <- data.frame(
    peak_group   = seq_len(n_groups),
    ref_rt       = group_rt,
    in_reference = in_reference,
    mean_rt      = round(mean_rt, 4),
    sd_rt        = round(sd_rt, 4),
    cv_pct       = round(sd_rt / mean_rt * 100, 4),
    n_matched    = n_matched,
    n_ambiguous  = count_by_group("AMBIGUOUS"),
    n_missing    = count_by_group("MISSING"),
    stringsAsFactors = FALSE
  )

  # --------------------------------------------------------------------------
  # Per-sample drift statistics
  # --------------------------------------------------------------------------

  drift_stats <- do.call(rbind, lapply(samples, function(smp) {
    in_smp <- as.character(aligned$sample) == smp
    diffs  <- aligned$rt_diff[in_smp & aligned$flag == "OK"]

    data.frame(
      sample        = smp,
      is_reference  = smp == ref_sample,
      n_peaks       = sum(smp_chr == smp),
      n_matched     = length(diffs),
      n_ambiguous   = sum(in_smp & aligned$flag == "AMBIGUOUS"),
      n_missing     = sum(in_smp & aligned$flag == "MISSING"),
      mean_rt_diff  = if (length(diffs) > 0) round(mean(diffs),       4) else NA_real_,
      sd_rt_diff    = if (length(diffs) > 1) round(stats::sd(diffs),  4) else NA_real_,
      mean_abs_diff = if (length(diffs) > 0) round(mean(abs(diffs)),  4) else NA_real_,
      max_abs_diff  = if (length(diffs) > 0) round(max(abs(diffs)),   4) else NA_real_,
      n_anchors     = n_anchors[[smp]],
      drift_model   = model_used[[smp]],
      stringsAsFactors = FALSE
    )
  }))

  rownames(drift_stats) <- NULL

  message(sprintf(
    "Grouped %d peaks into %d peak groups (%d not in the reference) with rt_tol = %.4f min; %d peak(s) flagged AMBIGUOUS.",
    n_peaks_all, n_groups, sum(!in_reference), rt_tol, sum(flag == "AMBIGUOUS")
  ))

  list(
    aligned     = aligned,
    summary     = summary_tbl,
    drift_stats = drift_stats,
    params      = list(reference = ref_sample, max_drift = max_drift,
                       rt_tol = rt_tol, drift_model = drift_model)
  )
}
