# ratios.R -------------------------------------------------------------------
# Ethane:methane analysis — the key discriminator between thermogenic
# (oil & gas, ethane-rich) and biogenic (landfill/wastewater/agriculture,
# ethane-free) methane. See the AMMBEC research-questions memo, sections 2 & 5.
# Base R.
# -----------------------------------------------------------------------------

#' Reduced major axis (orthogonal) regression slope of y on x.
#'
#' Both ethane and methane carry measurement error, so an ordinary least
#' squares slope is biased. RMA regression is the standard choice for
#' enhancement-ratio work. Returns slope, intercept, and Pearson r.
rma_slope <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  if (length(x) < 10) return(list(slope = NA, intercept = NA, r = NA, n = length(x)))
  r <- stats::cor(x, y)
  slope <- sign(r) * stats::sd(y) / stats::sd(x)
  intercept <- mean(y) - slope * mean(x)
  list(slope = slope, intercept = intercept, r = r, n = length(x))
}

#' York (2004) bivariate least-squares slope, using known instrument precisions.
#'
#' Unlike RMA, this weights each axis by its measurement-error variance, so the
#' slope reflects the actual instrument sensitivities. Errors assumed constant
#' and uncorrelated. Defaults are the AMMBEC ICARTT precisions: CH4 (x) 1 ppb,
#' C2H6 (y) 0.2 ppb.
#'
#' @param x,y paired enhancements (x = CH4, y = C2H6), ppb.
#' @param sx,sy 1-sigma instrument precisions of x and y (ppb).
#' @return list(slope, intercept, se_slope, r, n).
york_slope <- function(x, y, sx = 1.0, sy = 0.2) {
  ok <- is.finite(x) & is.finite(y); x <- x[ok]; y <- y[ok]
  n <- length(x)
  if (n < 10) return(list(slope = NA, intercept = NA, se_slope = NA, r = NA, n = n))
  wx <- 1/sx^2; wy <- 1/sy^2                       # constant weights
  b <- stats::cov(x, y) / stats::var(x)            # OLS start
  for (it in 1:50) {
    # per-point York weight (r_i = 0); length n so the weighted means below are
    # correct even when the precisions (and hence W) are the same for every point.
    W <- rep((wx*wy) / (wx + b^2*wy), n)
    xb <- sum(W*x)/sum(W); yb <- sum(W*y)/sum(W)
    U <- x - xb; V <- y - yb
    beta <- W*(U/wy + b*V/wx)
    b_new <- sum(W*beta*V) / sum(W*beta*U)
    if (is.finite(b_new) && abs(b_new - b) < 1e-10) { b <- b_new; break }
    b <- b_new
  }
  a <- yb - b*xb
  # slope uncertainty (York 2004, eqs. 16-19)
  xbar <- sum(W*x)/sum(W); u <- x - xbar
  se <- sqrt(1 / sum(W * u^2))
  list(slope = b, intercept = a, se_slope = se, r = stats::cor(x, y), n = n)
}

#' Bootstrap confidence interval for a York slope.
#'
#' 1 Hz data are autocorrelated, so a naive point bootstrap underestimates
#' uncertainty. If `blocks` (e.g. leg_id) is supplied, resamples whole blocks
#' (a block bootstrap), which captures between-plume variability — the honest
#' uncertainty for an enhancement ratio. Returns the point slope and 2.5/97.5%
#' bootstrap limits.
york_boot <- function(x, y, sx = 1.0, sy = 0.2, blocks = NULL, B = 2000) {
  ok <- is.finite(x) & is.finite(y)
  if (!is.null(blocks)) ok <- ok & !is.na(blocks) & blocks > 0   # drop unassigned (0/NA) legs
  x <- x[ok]; y <- y[ok]; if (!is.null(blocks)) blocks <- blocks[ok]
  if (length(x) < 10) return(list(slope = NA, lo = NA, hi = NA, n = length(x), n_blocks = NA))
  base <- york_slope(x, y, sx, sy)$slope
  ub <- if (!is.null(blocks)) unique(blocks) else NULL
  sl <- numeric(B)
  for (b in seq_len(B)) {
    idx <- if (!is.null(blocks)) {
      unlist(lapply(sample(ub, length(ub), replace = TRUE), function(g) which(blocks == g)))
    } else sample(length(x), replace = TRUE)
    sl[b] <- york_slope(x[idx], y[idx], sx, sy)$slope
  }
  list(slope = base, lo = stats::quantile(sl, 0.025, na.rm = TRUE, names = FALSE),
       hi = stats::quantile(sl, 0.975, na.rm = TRUE, names = FALSE), n = length(x),
       n_blocks = if (!is.null(ub)) length(ub) else NA_integer_)
}

#' Ethane-to-methane enhancement ratio for a flight/segment.
#'
#' Regresses ethane enhancement on methane enhancement (or raw values if
#' enhancements are absent). Slope is the molar C2H6:CH4 enhancement ratio
#' (both species in ppb -> dimensionless).
#'
#' @param df data.frame with methane and ethane columns.
#' @param ch4 Methane column (ppb). Prefers "<ch4>_enh" if present.
#' @param c2h6 Ethane column (ppb). Prefers "<c2h6>_enh" if present.
#' @param min_enh Only use points with methane enhancement above this (ppb)
#'   so the slope reflects plumes, not background scatter. Default 20.
#' @param method "york" (default; instrument-weighted, uses sx/sy precisions) or
#'   "rma" (geometric-mean; ignores instrument precisions).
#' @param sx,sy 1-sigma instrument precisions for CH4 and C2H6 (ppb); used by
#'   the York method. Defaults are the AMMBEC ICARTT values (1.0, 0.2).
ethane_methane_ratio <- function(df, ch4 = "CH4_ppb", c2h6 = "C2H6_ppb",
                                  min_enh = 20, method = "york", sx = 1.0, sy = 0.2) {
  xcol <- if (paste0(ch4, "_enh") %in% names(df)) paste0(ch4, "_enh") else ch4
  ycol <- if (paste0(c2h6, "_enh") %in% names(df)) paste0(c2h6, "_enh") else c2h6
  x <- df[[xcol]]; y <- df[[ycol]]
  if (grepl("_enh$", xcol)) {
    keep <- is.finite(x) & is.finite(y) & x > min_enh
    x <- x[keep]; y <- y[keep]
  }
  fit <- if (method == "york") york_slope(x, y, sx, sy) else rma_slope(x, y)
  fit$method <- method
  fit$ratio_pct <- 100 * fit$slope   # % ethane relative to methane
  fit
}

#' Crude fossil (thermogenic) fraction of methane from the ethane ratio.
#'
#' Two-endmember mix: observed ratio vs. a source-gas reference ratio.
#' fossil_fraction = observed_ratio / source_ratio (clamped to [0,1]).
#'
#' NOTE: `source_ratio` is a REGIONAL CALIBRATION CONSTANT, not a universal
#' value — the C2H6:CH4 of raw DJB natural gas. Set it from co-located source
#' sampling or the literature for the Wattenberg/DJB field before trusting the
#' number. The default here is a placeholder to make the pipeline runnable.
fossil_fraction <- function(observed_ratio, source_ratio) {
  stopifnot(length(source_ratio) == 1L, is.finite(source_ratio), source_ratio > 0)
  pmin(1, pmax(0, observed_ratio / source_ratio))
}
