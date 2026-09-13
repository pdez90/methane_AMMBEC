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
  # cor() warns when a degenerate subset has zero variance; the slope is still
  # well defined, and r is reported as NA, so the warning is noise not signal.
  r <- suppressWarnings(stats::cor(x, y))
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
  list(slope = b, intercept = a, se_slope = se, r = suppressWarnings(stats::cor(x, y)), n = n)
}

#' Within-leg (fixed-effects) York slope.
#'
#' A flight samples several air masses, one per level leg, and each carries its own
#' ethane offset. A York fit pooled across legs is then an exact variance-weighted
#' blend of the WITHIN-leg slope (the source ratio) and the BETWEEN-leg slope
#' through the leg means (does the leg with more methane also have more ethane?).
#' When one leg crosses a landfill plume and another the industrial corridor the
#' between-leg term is negative and can flip the pooled sign even though every leg's
#' own slope is positive (scripts 48/49, 20240708_R0_L1 and 20240710_R0_L1).
#'
#' This estimator removes the between-leg term: each leg's points are centred on
#' that leg's mean (a separate intercept per leg, i.e. fixed effects) and ONE York
#' slope is fitted to the centred points. Legs weight themselves by their methane
#' variance, so a leg with no methane range contributes nothing, and no clamping
#' is needed. Legs with fewer than `min_pts` usable points are dropped, as in
#' scripts 48/49.
#'
#' @param x,y paired enhancements (x = CH4, y = C2H6), ppb.
#' @param blocks leg id per point; 0/NA = unassigned, dropped.
#' @return list(slope, intercept = 0, se_slope, r, n, n_blocks); `r` is the
#'   correlation of the centred points.
york_within_leg <- function(x, y, blocks, sx = 1.0, sy = 0.2, min_pts = 10) {
  if (is.factor(blocks)) stop("blocks must be numeric leg ids, not a factor")
  ok <- is.finite(x) & is.finite(y) & !is.na(blocks) & blocks > 0
  x <- x[ok]; y <- y[ok]; blocks <- blocks[ok]
  big <- names(which(table(blocks) >= min_pts))
  keep <- blocks %in% big
  x <- x[keep]; y <- y[keep]; blocks <- blocks[keep]
  if (length(x) < min_pts)
    return(list(slope = NA, intercept = 0, se_slope = NA, r = NA, n = length(x), n_blocks = 0L))
  xc <- x - stats::ave(x, blocks); yc <- y - stats::ave(y, blocks)
  f <- york_slope(xc, yc, sx, sy)
  f$intercept <- 0; f$n_blocks <- length(unique(blocks)); f
}

#' The per-flight urban fossil-fraction slope under config.R's FOSSIL_ESTIMATOR.
#' Every script that reports a fossil fraction per flight (11, 12, 15, 19, 21, 22)
#' goes through this, so the switch cannot be applied to Table 1 and missed elsewhere.
#' Returns the york_slope()/york_within_leg() list.
.fossil_est <- function() {
  if (!exists("FOSSIL_ESTIMATOR", inherits = TRUE))
    stop("FOSSIL_ESTIMATOR is not defined: source config.R before R/ratios.R is used")
  match.arg(get("FOSSIL_ESTIMATOR"), c("within", "pooled"))
}
fossil_slope_fit <- function(x, y, blocks = NULL, sx = 1.0, sy = 0.2) {
  est <- .fossil_est()
  if (est == "within") {
    if (is.null(blocks)) stop("fossil_slope_fit(): FOSSIL_ESTIMATOR = 'within' needs leg ids")
    york_within_leg(x, y, blocks, sx, sy)
  } else york_slope(x, y, sx, sy)
}
fossil_method <- function() if (.fossil_est() == "within") "york_within" else "york"

#' Bootstrap confidence interval for a York slope.
#'
#' 1 Hz data are autocorrelated, so a naive point bootstrap underestimates
#' uncertainty. If `blocks` (e.g. leg_id) is supplied, resamples whole blocks
#' (a block bootstrap), which captures between-plume variability — the honest
#' uncertainty for an enhancement ratio. Returns the point slope and 2.5/97.5%
#' bootstrap limits.
york_boot <- function(x, y, sx = 1.0, sy = 0.2, blocks = NULL, B = 2000, within = FALSE,
                      seed = 42, min_pts = 10) {
  # seed: fixed PER CALL, so each flight's interval depends only on that flight's data,
  # not on how many random draws earlier flights or an edited early-return consumed
  # from the stream config.R seeds at start-up. (That is how 0713_R0_L1's bracket
  # moved 17 -> 18 on 13 Sep 2026: config.R had always seeded, but adding the
  # single-leg early return below changed how much of the stream 0703_R0_L2 used.)
  # The caller's RNG state is restored on exit so nothing else is disturbed.
  if (!is.null(seed)) {
    old <- if (exists(".Random.seed", envir = .GlobalEnv)) get(".Random.seed", envir = .GlobalEnv) else NULL
    on.exit(if (is.null(old)) suppressWarnings(rm(".Random.seed", envir = .GlobalEnv)) else
              assign(".Random.seed", old, envir = .GlobalEnv), add = TRUE)
    set.seed(seed)
  }
  # within = TRUE: resample legs, then fit the within-leg (fixed-effects) York slope
  # to each resample. Requires blocks. A leg drawn twice is centred on its own mean
  # both times, so the duplicate contributes the same centred points twice, which is
  # the ordinary block-bootstrap weighting.
  if (within && is.null(blocks)) stop("york_boot(within = TRUE) needs blocks (leg ids)")
  if (!is.null(blocks) && is.factor(blocks)) stop("blocks must be numeric leg ids, not a factor")
  ok <- is.finite(x) & is.finite(y)
  if (!is.null(blocks)) ok <- ok & !is.na(blocks) & blocks > 0   # drop unassigned (0/NA) legs
  x <- x[ok]; y <- y[ok]; if (!is.null(blocks)) blocks <- blocks[ok]
  empty <- function(base = NA_real_, nb = NA_integer_, nf = NA_integer_)
    list(slope = base, lo = NA_real_, hi = NA_real_, n = length(x), n_blocks = nb, n_blocks_fit = nf)
  if (length(x) < 10) return(empty())
  fit1 <- function(xx, yy, bb) if (within) york_within_leg(xx, yy, bb, sx, sy, min_pts)$slope else
                                           york_slope(xx, yy, sx, sy)$slope
  base <- fit1(x, y, blocks)
  ub <- if (!is.null(blocks)) unique(blocks) else NULL
  n_all <- if (!is.null(ub)) length(ub) else NA_integer_
  n_fit <- if (!is.null(ub)) sum(table(blocks) >= min_pts) else NA_integer_
  if (!is.finite(base)) return(empty(base, n_all, n_fit))
  if (within) {
    # Only legs with >= min_pts usable points enter the fit, so only those are
    # resampled: otherwise the resampling population (all legs) and the fitted
    # population differ, resamples made entirely of small legs come back NA and are
    # silently dropped, and the big legs' multiplicities follow the wrong binomial.
    ub <- ub[ub %in% as.numeric(names(which(table(blocks) >= min_pts)))]
    # If fewer than two legs are fittable every resample refits the same leg and the
    # "interval" collapses onto the point value (0703_R0_L2 printed 20 [20-20]). That
    # is not an interval; return NA so the table says so instead.
    if (n_fit < 2) return(empty(base, n_all, n_fit))
  }
  sl <- numeric(B)
  for (b in seq_len(B)) {
    idx <- if (!is.null(blocks)) {
      # sample.int, not sample(ub, ...): with a single leg id `sample(5, ...)` would
      # draw from 1:5 instead of from the one leg.
      unlist(lapply(ub[sample.int(length(ub), length(ub), replace = TRUE)],
                    function(g) which(blocks == g)))
    } else sample.int(length(x), replace = TRUE)
    sl[b] <- fit1(x[idx], y[idx], if (!is.null(blocks)) blocks[idx] else NULL)
  }
  list(slope = base, lo = stats::quantile(sl, 0.025, na.rm = TRUE, names = FALSE),
       hi = stats::quantile(sl, 0.975, na.rm = TRUE, names = FALSE), n = length(x),
       n_blocks = n_all, n_blocks_fit = n_fit)
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
  # method "york_within": the within-leg (fixed-effects) York slope; needs df$leg_id.
  # An unknown method is an error, not a silent fall-through to RMA.
  method <- match.arg(method, c("york", "york_within", "rma"))
  xcol <- if (paste0(ch4, "_enh") %in% names(df)) paste0(ch4, "_enh") else ch4
  ycol <- if (paste0(c2h6, "_enh") %in% names(df)) paste0(c2h6, "_enh") else c2h6
  x <- df[[xcol]]; y <- df[[ycol]]
  blocks <- if ("leg_id" %in% names(df)) df$leg_id else NULL
  if (grepl("_enh$", xcol)) {
    keep <- is.finite(x) & is.finite(y) & x > min_enh
    x <- x[keep]; y <- y[keep]; if (!is.null(blocks)) blocks <- blocks[keep]
  }
  if (method == "york_within" && is.null(blocks))
    stop("ethane_methane_ratio(method = 'york_within') needs a leg_id column")
  fit <- switch(method,
    york        = york_slope(x, y, sx, sy),
    york_within = york_within_leg(x, y, blocks, sx, sy),
    rma_slope(x, y))
  # york_within_leg() fits centred points (intercept 0 by construction). A fixed-effects
  # model has no single intercept; what is stored here is the line through the centroid
  # of the FITTED points (legs that cleared min_pts), for drawing only.
  if (method == "york_within" && is.finite(fit$slope)) {
    big <- as.numeric(names(which(table(blocks[!is.na(blocks) & blocks > 0]) >= 10)))
    f <- !is.na(blocks) & blocks %in% big
    fit$intercept <- mean(y[f]) - fit$slope * mean(x[f])
  }
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
