# peakTableAlign.R
# Agilent MassHunter UV-DAD peak table import and RT alignment for PenguinoidRTools
# https://github.com/GavinDuley/PenguinoidRTools
# Copyright (c) 2024 onwards, Gavin Duley
# Licenced under the GPL-3.0 licence

# ==============================================================================
# Agilent MassHunter UV-DAD peak table reader
# ==============================================================================

#' Read and align an Agilent MassHunter UV-DAD peak table export
#'
#' Reads a UV-DAD peak table CSV file exported by Agilent MassHunter
#' Quantitative Analysis, parses all sample blocks, and aligns retention times
#' (RTs) across samples using the first sample as the reference.
#'
#' @param file_path Character. Path to the Agilent MassHunter peak table file.
#'   Although MassHunter labels the export as CSV, the file is tab-separated.
#' @param rt_window Numeric. Maximum RT difference (in minutes) allowed between
#'   a sample peak and a reference peak for them to be treated as the same
#'   compound. Default is \code{0.4}.
#'
#' @return A wide-format data frame with one row per sample. Columns:
#'   \describe{
#'     \item{SampleName}{Sample name extracted from the MassHunter header line.}
#'     \item{Peak\emph{n}_RT}{Retention time (min) of aligned peak \emph{n}.}
#'     \item{Peak\emph{n}_Area}{Peak area of aligned peak \emph{n}.}
#'     \item{Peak\emph{n}_Height}{Peak height of aligned peak \emph{n}.}
#'     \item{Peak\emph{n}_AlignCV}{Coefficient of variation (\%) of the RT pair
#'       (reference RT, sample RT). Lower values indicate better alignment.
#'       \code{0} for the reference sample; \code{NA} for unmatched peaks.}
#'   }
#'   Peaks not detected in a given sample are \code{NA} across all four columns.
#'
#' @details
#' **File format.** The export contains one block of rows per sample, separated
#' by blank lines. Each block starts with a sample-header row (containing
#' \code{DAD} or \code{Sig=}), followed by a column-name row, then one row per
#' detected peak. The first tab-delimited field of every row is the data-file
#' path; the remaining fields hold the values described in the column-name row.
#'
#' **Sample name.** Extracted from the last whitespace-delimited token that ends
#' in \code{.d} on the sample-header line, with the \code{.d} suffix removed.
#' For example, \code{Ref=off 10.PBDEAL_CTRL_RT_0M_2.d} yields
#' \code{10.PBDEAL_CTRL_RT_0M_2}.
#'
#' **RT alignment.** The first sample's peaks are used as the reference. For
#' each reference peak, the nearest unmatched peak in each subsequent sample
#' within \code{rt_window} minutes is selected as its match. Alignment quality
#' is reported as the coefficient of variation of the two RTs:
#' \deqn{CV\% = \frac{SD(RT_{ref},\, RT_{sample})}{mean(RT_{ref},\, RT_{sample})} \times 100}
#' Values below ~0.5\% indicate excellent alignment; values above ~5\% suggest
#' the match may be unreliable and should be reviewed.
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' df <- read_agilent_dad_peaks("data/polyphenols_peaktable.csv")
#'
#' # Tighter alignment window (e.g. isocratic runs with stable RT)
#' df <- read_agilent_dad_peaks("data/polyphenols_peaktable.csv", rt_window = 0.2)
#' }
#'
#' @importFrom stats sd
#' @export
read_agilent_dad_peaks <- function(file_path, rt_window = 0.4) {

  lines <- readLines(file_path, warn = FALSE)

  # --------------------------------------------------------------------------
  # Step 1: Parse raw lines into a list of per-sample peak data frames
  # --------------------------------------------------------------------------

  samples_raw    <- list()
  current_name   <- NULL
  current_peaks  <- list()

  # Append the current sample to samples_raw and reset accumulators
  flush_sample <- function() {
    if (!is.null(current_name) && length(current_peaks) > 0) {
      mat <- do.call(rbind, current_peaks)
      df  <- data.frame(
        Peak   = as.integer(mat[, "Peak"]),
        RT     = as.numeric(mat[, "RT"]),
        Area   = as.numeric(mat[, "Area"]),
        Height = as.numeric(mat[, "Height"]),
        stringsAsFactors = FALSE
      )
      samples_raw[[length(samples_raw) + 1]] <<- list(
        sample_name = current_name,
        peaks       = df
      )
    }
    current_name  <<- NULL
    current_peaks <<- list()
  }

  for (line in lines) {

    # Blank line signals the end of a sample block
    if (trimws(line) == "") {
      flush_sample()
      next
    }

    # All lines are tab-delimited; field 1 is always the data-file path
    parts <- strsplit(line, "\t", fixed = TRUE)[[1]]
    if (length(parts) < 2) next
    second <- trimws(parts[2])

    # Sample-header line: second field contains "DAD" or "Sig="
    if (grepl("DAD|Sig=", second, perl = TRUE)) {
      flush_sample()
      tokens <- strsplit(trimws(second), "\\s+")[[1]]
      dot_d  <- grep("\\.d$", tokens, value = TRUE)
      current_name <- if (length(dot_d) > 0) {
        sub("\\.d$", "", tail(dot_d, 1))
      } else {
        second  # fallback: use the whole second field
      }
      next
    }

    # Column-header line: second field starts with "Peak"
    if (grepl("^Peak\\b", second)) next

    # Data line: second field must be a peak number (integer)
    peak_num <- suppressWarnings(as.integer(second))
    if (is.na(peak_num)) next

    # Column order: filepath | Peak | Start | RT | End | Height | Area | ...
    if (length(parts) < 7) next
    current_peaks[[length(current_peaks) + 1]] <- c(
      Peak   = second,
      RT     = trimws(parts[4]),
      Height = trimws(parts[6]),
      Area   = trimws(parts[7])
    )
  }

  flush_sample()  # handle a file with no trailing blank line

  if (length(samples_raw) == 0) {
    stop(
      "No sample blocks found in '", file_path, "'. ",
      "Ensure the file is a tab-delimited MassHunter UV-DAD peak table export."
    )
  }

  # --------------------------------------------------------------------------
  # Step 2: Align peaks in each sample to the reference (first sample)
  # --------------------------------------------------------------------------

  reference <- samples_raw[[1]]$peaks
  ref_rts   <- reference$RT
  n_ref     <- nrow(reference)

  # For a given sample, find the best match for each reference peak.
  # Returns a data frame with n_ref rows (one per reference peak) and columns
  # RT, Area, Height, CV. Rows are NA where no match was found.
  match_to_reference <- function(sample_peaks) {
    out  <- data.frame(
      RT     = rep(NA_real_, n_ref),
      Area   = rep(NA_real_, n_ref),
      Height = rep(NA_real_, n_ref),
      CV     = rep(NA_real_, n_ref),
      stringsAsFactors = FALSE
    )
    used <- rep(FALSE, nrow(sample_peaks))

    for (i in seq_len(n_ref)) {
      # Candidate sample peaks: unmatched and within the RT window
      candidates <- which(!used & abs(sample_peaks$RT - ref_rts[i]) <= rt_window)
      if (length(candidates) == 0) next

      # Select the candidate closest in RT to the reference peak
      diffs <- abs(sample_peaks$RT[candidates] - ref_rts[i])
      best  <- candidates[which.min(diffs)]

      rt_pair    <- c(ref_rts[i], sample_peaks$RT[best])
      out$RT[i]     <- sample_peaks$RT[best]
      out$Area[i]   <- sample_peaks$Area[best]
      out$Height[i] <- sample_peaks$Height[best]
      out$CV[i]     <- (stats::sd(rt_pair) / mean(rt_pair)) * 100
      used[best]    <- TRUE
    }
    out
  }

  # --------------------------------------------------------------------------
  # Step 3: Build the wide-format output data frame
  # --------------------------------------------------------------------------

  peak_ids    <- paste0("Peak", seq_len(n_ref))
  result_rows <- vector("list", length(samples_raw))

  for (s in seq_along(samples_raw)) {
    sname <- samples_raw[[s]]$sample_name
    row   <- list(SampleName = sname)

    if (s == 1) {
      # Reference sample: self-match, so AlignCV is defined as 0
      for (i in seq_len(n_ref)) {
        pid <- peak_ids[i]
        row[[paste0(pid, "_RT")]]      <- reference$RT[i]
        row[[paste0(pid, "_Area")]]    <- reference$Area[i]
        row[[paste0(pid, "_Height")]]  <- reference$Height[i]
        row[[paste0(pid, "_AlignCV")]] <- 0
      }
    } else {
      matched <- match_to_reference(samples_raw[[s]]$peaks)
      for (i in seq_len(n_ref)) {
        pid <- peak_ids[i]
        row[[paste0(pid, "_RT")]]      <- matched$RT[i]
        row[[paste0(pid, "_Area")]]    <- matched$Area[i]
        row[[paste0(pid, "_Height")]]  <- matched$Height[i]
        row[[paste0(pid, "_AlignCV")]] <- matched$CV[i]
      }
    }

    result_rows[[s]] <- as.data.frame(row, stringsAsFactors = FALSE)
  }

  do.call(rbind, result_rows)
}
