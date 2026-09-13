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
#'
#' TWO FILE LAYOUTS are supported, because NOAA CSL has distributed both and the
#' analysis must not depend on which copy is on disk:
#'   (a) yDay/year + latitude/longitude  — day-of-year time base;
#'   (b) a `time` dimension carrying "Seconds since <date>" + lat/lon.
#' Whichever is present, the returned object is identical in structure and units
#' (POSIXct UTC, metres, m2 s-2), so everything downstream is unchanged. The
#' layout actually used is recorded in the returned `time_base` for provenance.
read_velstats <- function(path) {
  if (!requireNamespace("ncdf4", quietly = TRUE))
    stop("Package 'ncdf4' required: install.packages('ncdf4')")
  nc <- ncdf4::nc_open(path); on.exit(ncdf4::nc_close(nc))
  have <- c(names(nc$var), names(nc$dim))
  .get <- function(nms) {                       # first name that exists, else NULL
    n <- nms[nms %in% have]
    if (!length(n)) return(NULL)
    as.numeric(ncdf4::ncvar_get(nc, n[1]))
  }
  height <- .get(c("height", "Height"))
  if (is.null(height)) stop("velStats file has no height variable: ", basename(path))
  wVar <- ncdf4::ncvar_get(nc, "wVar")            # dims: (height, time) or (time,height)
  # Orient so rows = time, cols = height.
  if (nrow(wVar) == length(height)) wVar <- t(wVar)
  wVar[!is.finite(wVar)] <- NA

  if ("yDay" %in% have) {
    # Layout (a): yDay is 1-based day-of-year; convert to UTC POSIXct.
    yday <- .get("yDay")
    year <- as.integer(stats::median(.get("year")))
    time <- as.POSIXct(sprintf("%d-01-01", year), tz = "UTC") + (yday - 1) * 86400
    time_base <- "yDay"
  } else if ("time" %in% have) {
    # Layout (b): seconds since the start of the file's own year. The units string
    # in the files distributed for this campaign reads "Seconds since 1 Jan 2020
    # 00:00 UTC" on files whose `year` variable is 2024, and the values themselves
    # are < 1 year of seconds, so the year in that string is a template that was
    # never updated. The `year` variable is authoritative and is used here; the
    # units epoch is only a fallback when `year` is absent. A hard check below
    # stops the read if the reconstructed dates do not land in that year.
    tsec <- .get("time")
    yr <- .get("year")
    if (!is.null(yr) && is.finite(stats::median(yr))) {
      year <- as.integer(stats::median(yr))
      epoch <- as.POSIXct(sprintf("%d-01-01", year), tz = "UTC")
    } else {
      un <- ncdf4::ncatt_get(nc, "time")$units
      if (is.null(un) || !grepl("^\\s*seconds since", un, ignore.case = TRUE))
        stop("velStats 'time' units not understood (expected 'Seconds since ...'): ", un)
      ep <- trimws(sub("(?i)\\s*UTC\\s*$", "",
                       sub("(?i)^\\s*seconds since\\s*", "", un, perl = TRUE), perl = TRUE))
      epoch <- NA
      for (f in c("%d %b %Y %H:%M", "%d %b %Y", "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d")) {
        epoch <- as.POSIXct(ep, format = f, tz = "UTC")
        if (!is.na(epoch)) break
      }
      if (is.na(epoch)) stop("could not parse velStats time epoch from units: ", un)
      year <- as.integer(format(epoch, "%Y"))
    }
    time <- epoch + tsec
    if (any(is.na(time)) || !all(format(time, "%Y") == as.character(year)))
      stop("velStats times do not fall in year ", year,
           " after applying the epoch; check the file's time base: ", basename(path))
    time_base <- "seconds-since-year-start"
  } else stop("velStats file has neither yDay nor time: ", basename(path))

  # Gates beyond the lidar's usable range are stored as NaN heights in some files.
  # Drop them here: a NaN height is not a gate, and leaving them in propagates into
  # the height axis of every plot and into the surface-connected layer search.
  hok <- is.finite(height)
  if (!all(hok)) {
    if (!sum(hok)) stop("velStats file has no finite gate heights: ", basename(path))
    height <- height[hok]; wVar <- wVar[, hok, drop = FALSE]
  }

  lat <- .get(c("latitude", "lat")); lon <- .get(c("longitude", "lon"))
  list(time = time, height = height, wVar = wVar,
       lat = if (length(lat)) lat[1] else NA_real_,
       lon = if (length(lon)) lon[1] else NA_real_,
       time_base = time_base)
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
