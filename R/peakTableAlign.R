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
#'   The first sample is used as the alignment reference by
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
#' **Sample name.** Extracted from the text after \code{Ref=off} on the
#' sample-header line, with the \code{.d} suffix removed. For example,
#' \code{Ref=off 10.PBDEAL_CTRL_RT_0M_2.d} yields
#' \code{10.PBDEAL_CTRL_RT_0M_2}.
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

  # Parse one CSV line into a character vector, handling quoted fields
  # (including quoted fields that contain commas, as in "Sig=280,4").
  parse_csv_line <- function(line) {
    con <- textConnection(line)
    on.exit(close(con))
    tryCatch(
      trimws(unlist(read.csv(con, header = FALSE, stringsAsFactors = FALSE,
                             strip.white = TRUE, na.strings = ""))),
      error = function(e) character(0)
    )
  }

  # --------------------------------------------------------------------------
  # Step 1: Parse raw lines into a list of per-sample peak data frames
  # --------------------------------------------------------------------------

  is_blank     <- function(x) grepl("^[\\t ,\"]*$", x, perl = TRUE)
  strip_quotes <- function(x) gsub('^"|"$', "", trimws(x))

  results        <- list()
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
      sample_raw     <- sub(".*Ref=off\\s+", "", line)
      sample_raw     <- gsub('[",\\s]+$', "", trimws(sample_raw), perl = TRUE)
      current_sample <- sub("\\.d$", "", trimws(sample_raw))
      col_idx        <- NULL
      next
    }

    # Parse as quoted CSV so that commas inside quoted fields are handled
    # correctly (e.g. "Sig=280,4" in the sample-header line).
    parts <- parse_csv_line(line)
    if (length(parts) < 2) next
    second <- parts[2]

    # Sample-header line: second field contains "DAD" or "Sig="
    if (grepl("DAD|Sig=", second, perl = TRUE)) {
      flush_sample()
      tokens <- strsplit(second, "\\s+")[[1]]
      dot_d  <- grep("\\.d$", tokens, value = TRUE)
      current_name <- if (length(dot_d) > 0) {
        sub("\\.d$", "", tail(dot_d, 1))
      } else {
        second  # fallback: use the whole second field
      }
      next
    }

    # Data line: sample and column header both known; second field is an integer.
    if (!is.na(current_sample) && !is.null(col_idx) && length(fields) >= 2) {
      if (!grepl("^[0-9]+$", fields[2])) next

      get_field <- function(col_name) {
        if (col_name %in% names(col_idx)) fields[col_idx[[col_name]]] else NA_character_
      }

    # Column order: filepath | Peak | Start | RT | End | Height | Area | ...
    if (length(parts) < 7) next
    current_peaks[[length(current_peaks) + 1]] <- c(
      Peak   = second,
      RT     = parts[4],
      Height = parts[6],
      Area   = parts[7]
    )
  }

  if (length(results) == 0) {
    stop(
      "No peak data found in '", file_path, "'. ",
      "Ensure the file is a MassHunter UV-DAD peak table export."
    )
  }

  do.call(rbind, results)
}

# ==============================================================================
# align_peaks_by_rt — aligner
# ==============================================================================

#' Align peaks across samples by retention time
#'
#' Aligns peaks across samples by matching each sample's peaks to the nearest
#' RT in the reference sample (the first sample in \code{peaks_long}). Uses
#' greedy one-to-one nearest-neighbour matching, flags ambiguous and unmatched
#' peaks, and clusters unmatched peaks into new groups for peaks absent from
#' the reference.
#'
#' @param peaks_long Long-format data frame from \code{\link{read_agilent_dad_peaks}}.
#' @param max_drift Numeric. Maximum RT difference (minutes) allowed for a
#'   valid match. Peaks with no reference peak within this window are reported
#'   as \code{UNMATCHED}. Set generously — the ambiguity flagging handles hard
#'   cases. Default \code{0.5}.
#'
#' @return A named list with three elements:
#'   \describe{
#'     \item{\code{aligned}}{Long-format data frame, one row per peak per
#'       sample, with columns \code{sample}, \code{peak}, \code{rt},
#'       \code{height}, \code{area}, \code{ref_rt} (RT of the matched reference
#'       peak), \code{rt_diff} (signed difference from reference, minutes),
#'       \code{flag} (\code{"OK"}, \code{"UNMATCHED"}, or \code{"AMBIGUOUS"}),
#'       and \code{flag_reason} (plain-English explanation for flagged rows).}
#'     \item{\code{summary}}{One row per peak group (reference peaks plus any
#'       new groups found only in non-reference samples), with \code{ref_rt},
#'       \code{in_reference}, mean RT, SD, CV\%, and counts of matched,
#'       ambiguous, and missing observations.}
#'     \item{\code{drift_stats}}{One row per sample with overall drift
#'       statistics vs the reference: mean signed RT diff, SD, mean absolute
#'       diff, max absolute diff, and counts of matched/ambiguous/missing peaks.}
#'   }
#'
#' @details
#' **Matching.** For each sample, all pairwise absolute RT differences within
#' \code{max_drift} are computed. Pairs are then assigned greedily from closest
#' to furthest, with each reference peak and each sample peak used at most once.
#'
#' **Ambiguity flag.** A match is flagged \code{AMBIGUOUS} when the absolute RT
#' difference equals or exceeds the spacing between the matched reference peak
#' and its nearest neighbouring reference peak. This indicates the assignment
#' may be unreliable and warrants manual review.
#'
#' **Unmatched peaks.** Sample peaks with no reference peak within
#' \code{max_drift} are flagged \code{UNMATCHED}. In a second pass these are
#' clustered by RT proximity into new peak groups (peaks genuine to
#' non-reference samples). Each group is assigned a representative \code{ref_rt}
#' equal to the mean RT of its founding members; samples lacking a peak in the
#' group receive \code{NA} rows.
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
#'
#' # Pivot to wide format for further analysis
#' library(tidyr)
#' area_wide <- tidyr::pivot_wider(
#'   result$aligned[!is.na(result$aligned$ref_rt), ],
#'   id_cols     = ref_rt,
#'   names_from  = sample,
#'   values_from = area
#' )
#' }
#'
#' @importFrom stats sd mean
#' @export
align_peaks_by_rt <- function(peaks_long, max_drift = 0.5) {

  samples <- unique(peaks_long$sample)

  if (length(samples) < 2) {
    stop("Need at least two samples to align.")
  }

  ref_sample <- samples[1]
  ref_peaks  <- peaks_long[peaks_long$sample == ref_sample, ]

  message(sprintf("Reference sample: %s  (%d peaks)", ref_sample, nrow(ref_peaks)))

  # Greedy one-to-one nearest-neighbour matching of one sample against the
  # reference. Returns a data frame with the same rows as smp_peaks, augmented
  # with ref_rt, rt_diff, flag, and flag_reason.
  match_sample_to_ref <- function(smp_peaks, ref_peaks, max_drift) {

    n_smp <- nrow(smp_peaks)
    n_ref <- nrow(ref_peaks)

    # Build distance matrix (sample rows x reference cols)
    dist_mat   <- outer(smp_peaks$rt, ref_peaks$rt, FUN = function(a, b) abs(a - b))
    candidates <- which(dist_mat <= max_drift, arr.ind = TRUE)
    if (nrow(candidates) > 0) {
      candidates <- candidates[order(dist_mat[candidates]), , drop = FALSE]
    }

    assigned_smp <- logical(n_smp)
    assigned_ref <- logical(n_ref)
    matches      <- vector("list", n_smp)

    for (k in seq_len(nrow(candidates))) {
      si <- candidates[k, 1]
      ri <- candidates[k, 2]
      if (!assigned_smp[si] && !assigned_ref[ri]) {
        assigned_smp[si] <- TRUE
        assigned_ref[ri] <- TRUE

        ref_rt_i  <- ref_peaks$rt[ri]
        rt_diff_i <- smp_peaks$rt[si] - ref_rt_i
        abs_d     <- abs(rt_diff_i)

        other_ref_rts   <- ref_peaks$rt[-ri]
        min_ref_spacing <- if (length(other_ref_rts) > 0)
          min(abs(ref_rt_i - other_ref_rts)) else Inf
        ambiguous <- abs_d >= min_ref_spacing

        row             <- smp_peaks[si, ]
        row$ref_rt      <- ref_rt_i
        row$rt_diff     <- rt_diff_i
        if (ambiguous) {
          row$flag        <- "AMBIGUOUS"
          row$flag_reason <- sprintf(
            "RT diff to ref (%.3f min) >= spacing between adjacent ref peaks (%.3f min) \u2014 manual review needed",
            abs_d, min_ref_spacing
          )
        } else {
          row$flag        <- "OK"
          row$flag_reason <- NA_character_
        }
        matches[[si]] <- row
      }
    }

    # Unassigned sample peaks
    for (si in which(!assigned_smp)) {
      abs_all     <- dist_mat[si, ]
      nearest_idx <- which.min(abs_all)
      row             <- smp_peaks[si, ]
      row$ref_rt      <- NA_real_
      row$rt_diff     <- NA_real_
      row$flag        <- "UNMATCHED"
      row$flag_reason <- sprintf(
        "No reference peak within %.3f min (nearest ref RT = %.3f, diff = %.3f min)",
        max_drift, ref_peaks$rt[nearest_idx], abs_all[nearest_idx]
      )
      matches[[si]] <- row
    }

    do.call(rbind, matches)
  }

  results <- list()

  # Reference sample: rt_diff = 0, all OK
  ref_out             <- ref_peaks
  ref_out$ref_rt      <- ref_peaks$rt
  ref_out$rt_diff     <- 0
  ref_out$flag        <- "OK"
  ref_out$flag_reason <- NA_character_
  results[[1]]        <- ref_out

  for (smp in samples[-1]) {
    smp_peaks <- peaks_long[peaks_long$sample == smp, ]
    results[[length(results) + 1L]] <- match_sample_to_ref(smp_peaks, ref_peaks, max_drift)
  }

  aligned          <- do.call(rbind, results)
  rownames(aligned) <- NULL

  # --------------------------------------------------------------------------
  # Second pass: cluster UNMATCHED peaks into new peak groups
  # --------------------------------------------------------------------------
  # Peaks absent from the reference are clustered by RT proximity. Each group
  # can contain at most one peak per sample. The representative RT is the mean
  # of the founding members and is fixed at group creation to prevent drift.

  unmatched <- aligned[aligned$flag == "UNMATCHED", ]

  if (nrow(unmatched) > 0) {

    um_sorted <- unmatched[order(unmatched$rt), ]
    n_um      <- nrow(um_sorted)
    um_group  <- integer(n_um)
    group_rt  <- numeric(0)
    group_smp <- list()

    for (i in seq_len(n_um)) {
      rt_i  <- um_sorted$rt[i]
      smp_i <- um_sorted$sample[i]

      in_window  <- which(abs(group_rt - rt_i) <= max_drift)
      candidates <- in_window[!vapply(group_smp[in_window],
                                      function(s) smp_i %in% s, logical(1))]

      if (length(candidates) == 0) {
        group_rt  <- c(group_rt, rt_i)
        group_smp <- c(group_smp, list(smp_i))
        um_group[i] <- length(group_rt)
      } else {
        best             <- candidates[which.min(abs(group_rt[candidates] - rt_i))]
        um_group[i]      <- best
        group_smp[[best]] <- c(group_smp[[best]], smp_i)
        # group_rt is intentionally not updated to prevent floating-mean drift
      }
    }

    um_sorted$new_group <- um_group
    new_group_rows      <- list()

    for (g in unique(um_sorted$new_group)) {
      grp         <- um_sorted[um_sorted$new_group == g, ]
      grp_mean_rt <- round(mean(grp$rt), 4)

      grp$ref_rt      <- grp_mean_rt
      grp$rt_diff     <- round(grp$rt - grp_mean_rt, 4)
      grp$flag        <- "OK"
      grp$flag_reason <- NA_character_
      grp$new_group   <- NULL

      missing_samples <- setdiff(samples, grp$sample)
      if (length(missing_samples) > 0) {
        na_rows <- data.frame(
          sample      = missing_samples,
          peak        = NA_integer_,
          rt          = NA_real_,
          height      = NA_real_,
          area        = NA_real_,
          ref_rt      = grp_mean_rt,
          rt_diff     = NA_real_,
          flag        = "OK",
          flag_reason = NA_character_,
          stringsAsFactors = FALSE
        )
        grp <- rbind(grp, na_rows)
      }
      new_group_rows[[length(new_group_rows) + 1L]] <- grp
    }

    new_peaks_df      <- do.call(rbind, new_group_rows)
    rownames(new_peaks_df) <- NULL

    aligned <- rbind(
      aligned[aligned$flag != "UNMATCHED", ],
      new_peaks_df
    )
    rownames(aligned) <- NULL

    message(sprintf(
      "UNMATCHED peaks: %d observations grouped into %d new peak groups; NA inserted for absent samples.",
      nrow(unmatched), length(unique(um_group))
    ))
  }

  # --------------------------------------------------------------------------
  # Summary table: one row per unique ref_rt
  # --------------------------------------------------------------------------

  all_ref_rts <- sort(unique(aligned$ref_rt[!is.na(aligned$ref_rt)]))
  ok_matches  <- aligned[aligned$flag == "OK" & !is.na(aligned$ref_rt), ]

  summary_tbl <- do.call(rbind, lapply(all_ref_rts, function(rrt) {
    grp <- ok_matches[!is.na(ok_matches$ref_rt) & ok_matches$ref_rt == rrt &
                        !is.na(ok_matches$rt), ]
    n_matched   <- nrow(grp)
    n_ambiguous <- sum(aligned$flag == "AMBIGUOUS" & !is.na(aligned$ref_rt) &
                         aligned$ref_rt == rrt)
    n_missing   <- sum(aligned$flag == "OK" & !is.na(aligned$ref_rt) &
                         aligned$ref_rt == rrt & is.na(aligned$rt))
    in_reference <- rrt %in% ref_peaks$rt

    if (n_matched >= 2) {
      mean_rt <- mean(grp$rt)
      sd_rt   <- stats::sd(grp$rt)
      cv_rt   <- (sd_rt / mean_rt) * 100
    } else if (n_matched == 1) {
      mean_rt <- grp$rt
      sd_rt   <- NA_real_
      cv_rt   <- NA_real_
    } else {
      mean_rt <- rrt
      sd_rt   <- NA_real_
      cv_rt   <- NA_real_
    }

    data.frame(
      ref_rt       = rrt,
      in_reference = in_reference,
      mean_rt      = round(mean_rt, 4),
      sd_rt        = round(sd_rt, 4),
      cv_pct       = round(cv_rt, 4),
      n_matched    = n_matched,
      n_ambiguous  = n_ambiguous,
      n_missing    = n_missing,
      stringsAsFactors = FALSE
    )
  }))

  rownames(summary_tbl) <- NULL

  # --------------------------------------------------------------------------
  # Per-sample drift statistics
  # --------------------------------------------------------------------------

  drift_stats <- do.call(rbind, lapply(samples, function(smp) {
    smp_rows <- aligned[aligned$sample == smp & aligned$flag == "OK" &
                          !is.na(aligned$rt), ]
    diffs    <- smp_rows$rt_diff

    data.frame(
      sample        = smp,
      is_reference  = smp == ref_sample,
      n_peaks       = sum(peaks_long$sample == smp),
      n_matched     = nrow(smp_rows),
      n_ambiguous   = sum(aligned$sample == smp & aligned$flag == "AMBIGUOUS"),
      n_missing     = sum(aligned$sample == smp & aligned$flag == "OK" &
                            is.na(aligned$rt)),
      mean_rt_diff  = if (length(diffs) > 0) round(mean(diffs),       4) else NA_real_,
      sd_rt_diff    = if (length(diffs) > 1) round(stats::sd(diffs),  4) else NA_real_,
      mean_abs_diff = if (length(diffs) > 0) round(mean(abs(diffs)),  4) else NA_real_,
      max_abs_diff  = if (length(diffs) > 0) round(max(abs(diffs)),   4) else NA_real_,
      stringsAsFactors = FALSE
    )
  }))

  rownames(drift_stats) <- NULL

  aligned <- aligned[order(aligned$ref_rt, aligned$sample), ]
  rownames(aligned) <- NULL

  list(
    aligned     = aligned,
    summary     = summary_tbl,
    drift_stats = drift_stats
  )
}
