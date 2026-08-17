# lidar_blh.R ----------------------------------------------------------------
# Mixing-layer (boundary-layer) height from the NOAA CSL Doppler-lidar vertical-
# velocity statistics file (velStats_YYYYMM.nc), for the mass-balance flux.
#
# Method: the convective boundary layer is turbulent, so vertical-velocity
# variance (wVar) is large within it and collapses above it. The mixing height
# is taken as the highest gate, contiguous from the surface, where wVar exceeds
# a turbulence threshold (default 0.1 m^2/s^2; Tucker et al. 2009). Requires the
# `ncdf4` package (install.packages("ncdf4")).
#
# COVERAGE NOTE: velStats is monthly. A flight's mixing height only exists if the
# file spans that flight's UTC hours — check with blh_for_window()'s `n` return.
# -----------------------------------------------------------------------------

#' Read velStats_YYYYMM.nc into time (POSIXct UTC), height (m), and wVar matrix.
read_velstats <- function(path) {
  if (!requireNamespace("ncdf4", quietly = TRUE))
    stop("Package 'ncdf4' required: install.packages('ncdf4')")
  nc <- ncdf4::nc_open(path); on.exit(ncdf4::nc_close(nc))
  yday <- as.numeric(ncdf4::ncvar_get(nc, "yDay"))
  year <- as.integer(stats::median(as.numeric(ncdf4::ncvar_get(nc, "year"))))
  height <- as.numeric(ncdf4::ncvar_get(nc, "height"))
  wVar <- ncdf4::ncvar_get(nc, "wVar")            # dims: (height, time) or (time,height)
  # Orient so rows = time, cols = height.
  if (nrow(wVar) == length(height)) wVar <- t(wVar)
  wVar[!is.finite(wVar)] <- NA
  # yDay is 1-based day-of-year; convert to UTC POSIXct.
  t0 <- as.POSIXct(sprintf("%d-01-01", year), tz = "UTC")
  time <- t0 + (yday - 1) * 86400
  list(time = time, height = height, wVar = wVar,
       lat = as.numeric(ncdf4::ncvar_get(nc, "latitude"))[1],
       lon = as.numeric(ncdf4::ncvar_get(nc, "longitude"))[1])
}

#' Mixing height for one wVar profile (vector over height).
#'
#' The mixing height is the top of the layer that is turbulent AND connected to
#' the surface. Gates are sorted by height, and the layer must begin at the
#' lowest usable gate; if that gate is not turbulent there is no surface-connected
#' mixed layer and the retrieval returns NA rather than a spuriously shallow value
#' (an earlier version could return the lowest gate's height in that case). One
#' single-gate dropout is tolerated (max_gap = 1).
mixing_height <- function(wvar, height, thr = 0.1, max_gap = 1L) {
  ord <- order(height); height <- height[ord]; wvar <- wvar[ord]
  valid <- is.finite(wvar) & is.finite(height)
  if (sum(valid) < 3) return(NA_real_)
  turbulent <- valid & wvar >= thr
  if (!any(turbulent)) return(NA_real_)
  first_valid <- which(valid)[1]
  if (!turbulent[first_valid]) return(NA_real_)   # no surface-connected turbulent layer
  top <- first_valid; gap <- 0L
  for (j in seq.int(first_valid + 1L, length(height))) {
    if (isTRUE(turbulent[j])) { top <- j; gap <- 0L }
    else { gap <- gap + 1L; if (gap > max_gap) break }
  }
  height[top]
}

#' Mixing-height time series from a velStats object.
blh_timeseries <- function(vs, thr = 0.1) {
  blh <- vapply(seq_along(vs$time), function(i)
    mixing_height(vs$wVar[i, ], vs$height, thr = thr), numeric(1))
  data.frame(time = vs$time, blh_m = blh)
}

#' Mixing height for a flight window, auto-selecting the monthly velStats file.
#'
#' Picks velStats_YYYYMM.nc from `lidar_dir` matching the flight date, computes
#' the mixing-height series, and returns the median over the flight's UTC window.
#' Monthly series are cached in `.blh_cache` so repeated calls are cheap.
#' @return list(blh_m, n) — n = 0 (blh NA) if no file/coverage.
.blh_cache <- new.env(parent = emptyenv())
blh_flight <- function(date, t_start, t_end, lidar_dir, thr = 0.1) {
  ym <- format(as.Date(date), "%Y%m")
  key <- paste(normalizePath(lidar_dir, mustWork = FALSE), ym, thr, sep = "::")  # dir-specific cache
  if (is.null(.blh_cache[[key]])) {
    f <- file.path(lidar_dir, paste0("velStats_", ym, ".nc"))
    .blh_cache[[key]] <- if (file.exists(f))
      blh_timeseries(read_velstats(f), thr = thr) else NA
  }
  bt <- .blh_cache[[key]]
  if (!is.data.frame(bt)) return(list(blh_m = NA_real_, n = 0))
  blh_for_window(bt, t_start, t_end)
}

#' Median mixing height within a UTC time window (e.g. a flight).
#' @return list(blh_m, n) — n is how many lidar profiles fell in the window
#'   (n = 0 means the file does not cover this flight; BLH is NA).
blh_for_window <- function(blh_ts, t_start, t_end) {
  sel <- blh_ts$time >= t_start & blh_ts$time <= t_end & is.finite(blh_ts$blh_m)
  list(blh_m = if (any(sel)) stats::median(blh_ts$blh_m[sel]) else NA_real_,
       n = sum(sel))
}
