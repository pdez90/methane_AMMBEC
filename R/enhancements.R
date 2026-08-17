# enhancements.R -------------------------------------------------------------
# Background estimation and plume/enhancement detection for a methane time
# series (works for both the aircraft CH4_ppb and the mobile CH4_ppmv). Base R.
# -----------------------------------------------------------------------------

#' Rolling-quantile baseline.
#'
#' Estimates the local "background" as a low quantile within a moving window,
#' the standard trick for separating a slowly varying regional background from
#' short-lived plume spikes.
#'
#' @param x Numeric vector (a concentration time series, assumed ~1 Hz order).
#' @param window Window length in samples (default 300 ~ 5 min at 1 Hz).
#' @param q Quantile treated as background (default 0.05).
rolling_baseline <- function(x, window = 300, q = 0.05) {
  n <- length(x)
  half <- window %/% 2
  bg <- numeric(n)
  for (i in seq_len(n)) {
    lo <- max(1, i - half); hi <- min(n, i + half)
    bg[i] <- stats::quantile(x[lo:hi], probs = q, na.rm = TRUE, names = FALSE)
  }
  bg
}

#' Add background + enhancement columns and flag plumes.
#'
#' @param df data.frame containing the concentration column.
#' @param conc Name of the concentration column (e.g. "CH4_ppb").
#' @param window,q Passed to rolling_baseline().
#' @param n_sigma Enhancement flagged when it exceeds n_sigma * MAD of the
#'   below-median residuals (robust noise estimate). Default 3.
#' @return df with added `<conc>_bg`, `<conc>_enh`, and `plume` (logical).
add_enhancements <- function(df, conc, window = 300, q = 0.05, n_sigma = 3) {
  x <- df[[conc]]
  bg <- rolling_baseline(x, window = window, q = q)
  enh <- x - bg
  # Robust noise scale from the quiet part of the record.
  quiet <- enh[enh <= stats::median(enh, na.rm = TRUE)]
  sigma <- stats::mad(quiet, na.rm = TRUE)
  if (!is.finite(sigma) || sigma == 0) sigma <- stats::sd(enh, na.rm = TRUE)

  df[[paste0(conc, "_bg")]]  <- bg
  df[[paste0(conc, "_enh")]] <- enh
  df$plume <- !is.na(enh) & enh > n_sigma * sigma
  attr(df, "enh_sigma") <- sigma
  df
}
