# plume_tracer.R ---------------------------------------------------------------
# Helpers for the exploratory plume-level tracer analysis (scripts 66-68):
# instrument lags, 1-Hz noise, plume events, and errors-in-variables slopes with
# moving-block bootstrap intervals. Base R. Nothing here feeds the manuscript.
# -----------------------------------------------------------------------------

#' Errors-in-variables slope with CONSTANT per-axis precisions (closed form).
#'
#' With constant sx, sy the York (2004) iteration converges to the Deming slope
#' with lambda = sy^2 / sx^2, so this equals york_slope() in R/ratios.R to
#' numerical precision (checked in script 66) but is closed form, which lets the
#' bootstrap and the power simulation run as matrix operations.
#' Rows of X, Y (matrices) are independent data sets; vectors are one data set.
deming_slope <- function(x, y, sx, sy) {
  lam <- (sy / sx)^2
  if (is.null(dim(x))) { x <- matrix(x, 1); y <- matrix(y, 1) }
  n  <- ncol(x)
  xm <- x - rowMeans(x); ym <- y - rowMeans(y)
  sxx <- rowSums(xm^2); syy <- rowSums(ym^2); sxy <- rowSums(xm * ym)
  d <- syy - lam * sxx
  b <- (d + sqrt(d^2 + 4 * lam * sxy^2)) / (2 * sxy)
  b[!is.finite(b)] <- 0
  b
}

#' Moving-block bootstrap interval for the Deming slope.
#'
#' 1-Hz aircraft data are autocorrelated (the Picarro updates every ~2.5 s), so
#' blocks of `L` consecutive seconds are resampled. Returns slope, 2.5/97.5%.
#' The seed is fixed per call (as york_boot does) so a plume's interval depends
#' only on its own data.
deming_boot <- function(x, y, sx, sy, L = 5, B = 1000, seed = 66) {
  ok <- is.finite(x) & is.finite(y); x <- x[ok]; y <- y[ok]; n <- length(x)
  if (n < 10) return(c(slope = NA, lo = NA, hi = NA, se = NA))
  old <- if (exists(".Random.seed", envir = .GlobalEnv)) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit(if (is.null(old)) suppressWarnings(rm(".Random.seed", envir = .GlobalEnv)) else
            assign(".Random.seed", old, envir = .GlobalEnv), add = TRUE)
  set.seed(seed)
  idx <- block_index(n, L, B)
  bs <- deming_slope(matrix(x[idx], B), matrix(y[idx], B), sx, sy)
  c(slope = deming_slope(x, y, sx, sy),
    lo = stats::quantile(bs, 0.025, names = FALSE, na.rm = TRUE),
    hi = stats::quantile(bs, 0.975, names = FALSE, na.rm = TRUE),
    se = stats::sd(bs, na.rm = TRUE))
}

#' B x n matrix of moving-block bootstrap indices (block length L).
block_index <- function(n, L, B) {
  L <- min(L, n); nb <- ceiling(n / L)
  st <- matrix(sample.int(n - L + 1, B * nb, replace = TRUE), B)
  idx <- st[, rep(seq_len(nb), each = L), drop = FALSE] + rep(rep(0:(L - 1), nb), each = B)
  idx[, seq_len(n), drop = FALSE]
}

#' Series shifted so that out[t] = x[t + k]. k > 0: the tracer is recorded k s
#' AFTER methane, so reading k s ahead aligns it.
shift_series <- function(x, k) {
  n <- length(x); if (k == 0) return(x)
  if (k > 0) c(x[(k + 1):n], rep(NA, k)) else c(rep(NA, -k), x[1:(n + k)])
}

#' High-pass residual: x minus a centred running median of `k` s (gaps filled
#' by linear interpolation for the median only; NA stays NA in the output).
highpass <- function(x, k = 61) {
  ok <- is.finite(x); if (sum(ok) < k) return(rep(NA_real_, length(x)))
  xf <- stats::approx(which(ok), x[ok], seq_along(x), rule = 2)$y
  r <- x - stats::runmed(xf, k, endrule = "median"); r[!ok] <- NA; r
}

#' Effective 1-Hz noise from quiet, level-leg samples: robust sigma (MAD) of the
#' high-passed series and its lag-1 autocorrelation. Includes small-scale
#' atmospheric variability, so it is an upper bound on instrument noise.
noise_stats <- function(hp, quiet) {
  v <- hp; v[!quiet] <- NA
  s <- stats::mad(v, na.rm = TRUE)
  a <- v[-length(v)]; b <- v[-1]; ok <- is.finite(a) & is.finite(b)
  c(sigma = s, rho1 = if (sum(ok) > 50) stats::cor(a[ok], b[ok]) else NA)
}

#' Lag of `y` relative to `x` from strong-variance windows only.
#'
#' The level-leg record is cut into non-overlapping `win`-s windows; only windows
#' whose high-passed x variance is in the top `top` fraction (large, unmistakable
#' co-enhancement structure) are used, and one pooled correlation per trial lag is
#' computed over those windows. Estimating a lag per small plume would fit noise.
#' Returns lag (s), r at that lag, r at lag 0, number of windows, and the peak
#' contrast (r_best minus the mean r of lags +/-3 s away), a sharpness check.
estimate_lag <- function(x, y, inleg, maxlag = 15, win = 60, top = 0.15, min_win = 5) {
  hx <- highpass(x); hy <- highpass(y); n <- length(x)
  wid <- ceiling(seq_len(n) / win)
  okw <- tapply(inleg & is.finite(hx), wid, mean) > 0.9
  vw  <- tapply(hx, wid, stats::var, na.rm = TRUE)
  cand <- as.integer(names(vw))[okw & is.finite(vw)]
  if (length(cand) < min_win) return(c(lag = NA, r = NA, r0 = NA, n_win = length(cand), contrast = NA))
  vc <- vw[as.character(cand)]
  sel <- cand[vc >= stats::quantile(vc, 1 - top)]
  if (length(sel) < min_win) sel <- cand[order(-vc)][seq_len(min(min_win, length(cand)))]
  use <- wid %in% sel
  lags <- -maxlag:maxlag
  rr <- vapply(lags, function(k) {
    ys <- shift_series(hy, k); ok <- use & is.finite(hx) & is.finite(ys)
    if (sum(ok) < 30) NA_real_ else stats::cor(hx[ok], ys[ok])
  }, numeric(1))
  if (all(!is.finite(rr))) return(c(lag = NA, r = NA, r0 = NA, n_win = length(sel), contrast = NA))
  kb <- lags[which.max(rr)]
  far <- rr[abs(lags - kb) >= 3 & abs(lags - kb) <= 6]
  c(lag = kb, r = max(rr, na.rm = TRUE), r0 = rr[lags == 0], n_win = length(sel),
    contrast = max(rr, na.rm = TRUE) - mean(far, na.rm = TRUE))
}

#' Contiguous plume events: enh > thr inside one leg, gaps up to `gap` s bridged,
#' core length >= core_min s. Returns a data.frame of row indices (i0, i1).
find_plume_events <- function(enh, leg, thr = 20, gap = 3, core_min = 10) {
  n <- length(enh); ev <- list(); i <- 1
  e <- ifelse(is.finite(enh), enh, -Inf)
  while (i <= n) {
    if (!(e[i] > thr && leg[i] > 0)) { i <- i + 1; next }
    j <- i; g <- 0; k <- i
    while (k < n && leg[k + 1] == leg[i]) {
      k <- k + 1
      if (e[k] > thr) { j <- k; g <- 0 } else { g <- g + 1; if (g > gap) break }
    }
    if (j - i + 1 >= core_min) ev[[length(ev) + 1]] <- c(i0 = i, i1 = j)
    i <- j + 1
  }
  if (!length(ev)) return(data.frame(i0 = integer(), i1 = integer()))
  as.data.frame(do.call(rbind, ev))
}

#' AR(1) noise with marginal sd `sigma` and lag-1 correlation `rho`.
ar1_noise <- function(n, sigma, rho, nrep = 1) {
  rho <- max(0, min(0.98, rho))
  e <- matrix(stats::rnorm(n * nrep, 0, sigma * sqrt(1 - rho^2)), nrep)
  out <- e; out[, 1] <- stats::rnorm(nrep, 0, sigma)
  if (n > 1) for (t in 2:n) out[, t] <- rho * out[, t - 1] + e[, t]
  out
}

#' Vector-mean wind direction (degrees, direction the wind blows FROM).
wd_mean <- function(wd, ws = 1) {
  u <- -ws * sin(wd * pi / 180); v <- -ws * cos(wd * pi / 180)
  (atan2(-mean(u, na.rm = TRUE), -mean(v, na.rm = TRUE)) * 180 / pi + 360) %% 360
}

hav_km <- function(lat1, lon1, lat2, lon2) {
  p <- pi / 180
  a <- sin((lat2 - lat1) * p / 2)^2 + cos(lat1 * p) * cos(lat2 * p) * sin((lon2 - lon1) * p / 2)^2
  2 * 6371.0088 * asin(pmin(1, sqrt(a)))
}

#' Two-component partition of a tracer inside one plume:
#'   dT = a * dCH4_fossil + b * dCH4_micro + c,
#'   dCH4_fossil = dC2H6 / beta,  dCH4_micro = dCH4 - dC2H6 / beta.
#' If CO co-varies with the ethane-carrying methane rather than with the
#' ethane-free methane in the SAME plume, a > b. Only meaningful when the ethane
#' ratio varies within the plume (collinearity checked by the caller via VIF).
#' OLS with a moving-block bootstrap; ethane noise divided by beta (~1.8 ppb)
#' attenuates `a` toward 0, so the test is conservative for the fossil term.
partition_boot <- function(dch4, dc2h6, dT, beta, L = 5, B = 1000, seed = 67) {
  ok <- is.finite(dch4) & is.finite(dc2h6) & is.finite(dT)
  f <- dc2h6[ok] / beta; m <- dch4[ok] - f; y <- dT[ok]; n <- length(y)
  vif <- if (n > 5) 1 / (1 - stats::cor(f, m)^2) else NA
  if (n < 20) return(c(a = NA, a_lo = NA, a_hi = NA, b = NA, b_lo = NA, b_hi = NA, d_lo = NA, d_hi = NA, vif = vif))
  old <- if (exists(".Random.seed", envir = .GlobalEnv)) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit(if (is.null(old)) suppressWarnings(rm(".Random.seed", envir = .GlobalEnv)) else
            assign(".Random.seed", old, envir = .GlobalEnv), add = TRUE)
  set.seed(seed)
  fit <- function(F, M, Y) {
    F <- F - rowMeans(F); M <- M - rowMeans(M); Y <- Y - rowMeans(Y)
    sff <- rowSums(F^2); smm <- rowSums(M^2); sfm <- rowSums(F * M); sfy <- rowSums(F * Y); smy <- rowSums(M * Y)
    det <- sff * smm - sfm^2
    cbind(a = (smm * sfy - sfm * smy) / det, b = (sff * smy - sfm * sfy) / det)
  }
  base <- fit(matrix(f, 1), matrix(m, 1), matrix(y, 1))
  idx <- block_index(n, L, B)
  bs <- fit(matrix(f[idx], B), matrix(m[idx], B), matrix(y[idx], B))
  q <- function(v, p) stats::quantile(v, p, na.rm = TRUE, names = FALSE)
  d <- bs[, "a"] - bs[, "b"]
  c(a = unname(base[1, "a"]), a_lo = q(bs[, "a"], .025), a_hi = q(bs[, "a"], .975),
    b = unname(base[1, "b"]), b_lo = q(bs[, "b"], .025), b_hi = q(bs[, "b"], .975),
    d_lo = q(d, .025), d_hi = q(d, .975), vif = vif)
}

#' Slope interval that allows for residual autocorrelation analytically.
#' SE from the actual residual scatter (so model misfit counts), inflated by the
#' AR(1) effective sample size n_eff = n (1 - rho) / (1 + rho), rho = lag-1
#' autocorrelation of the residuals (clamped to [0, 0.95]); t quantile on n_eff - 2.
slope_ci_ar1 <- function(x, y, sx, sy, level = 0.95) {
  ok <- is.finite(x) & is.finite(y); x <- x[ok]; y <- y[ok]; n <- length(x)
  if (n < 10) return(c(slope = NA, lo = NA, hi = NA, se = NA, rho = NA, neff = NA))
  b <- deming_slope(x, y, sx, sy); a <- mean(y) - b * mean(x); e <- y - a - b * x
  rho <- min(0.95, max(0, suppressWarnings(stats::cor(e[-1], e[-n]))), na.rm = TRUE)
  neff <- n * (1 - rho) / (1 + rho)
  se <- sqrt(sum(e^2) / (n - 2) / sum((x - mean(x))^2)) * sqrt(n / neff)
  q <- stats::qt(1 - (1 - level) / 2, max(neff - 2, 2))
  c(slope = b, lo = b - q * se, hi = b + q * se, se = se, rho = rho, neff = neff)
}

#' Interval used for every classification in 67/68: the UNION of the 5-s moving-
#' block bootstrap interval and the AR(1)-inflated analytic interval. In script-68
#' null simulations the block bootstrap alone gave one-sided false-positive rates of
#' 4-10% (nominal 2.5%) on 25-80-s plumes because blocks that short cannot hold the
#' Picarro's autocorrelation; the analytic interval alone gave 1.5-5%. The union is
#' at least as conservative as either.
slope_ci <- function(x, y, sx, sy, L = 5, B = 1000, seed = 66) {
  bt <- deming_boot(x, y, sx, sy, L, B, seed); ar <- slope_ci_ar1(x, y, sx, sy)
  c(slope = unname(bt["slope"]), lo = min(bt["lo"], ar["lo"]), hi = max(bt["hi"], ar["hi"]),
    lo_boot = unname(bt["lo"]), lo_ar1 = unname(ar["lo"]), neff = unname(ar["neff"]))
}
