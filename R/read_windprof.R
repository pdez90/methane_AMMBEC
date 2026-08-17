# read_windprof.R ------------------------------------------------------------
# NOAA CSL radar/Doppler wind-profiler reader (windProf_YYYYMM.nc) and helpers
# to interpolate winds to a given altitude + time, for the mass-balance
# perpendicular-wind term. Requires the `ncdf4` package.
#
# COVERAGE NOTE: monthly file; winds for a flight only exist if the file spans
# that flight's UTC hours (wind_at() returns NA otherwise).
# -----------------------------------------------------------------------------

#' Read windProf_YYYYMM.nc into time (POSIXct UTC), height (m), speed, direction.
read_windprof <- function(path) {
  if (!requireNamespace("ncdf4", quietly = TRUE))
    stop("Package 'ncdf4' required: install.packages('ncdf4')")
  nc <- ncdf4::nc_open(path); on.exit(ncdf4::nc_close(nc))
  yday <- as.numeric(ncdf4::ncvar_get(nc, "yDay"))
  year <- as.integer(stats::median(as.numeric(ncdf4::ncvar_get(nc, "year"))))
  height <- as.numeric(ncdf4::ncvar_get(nc, "height"))
  spd <- ncdf4::ncvar_get(nc, "speed"); dir <- ncdf4::ncvar_get(nc, "direction")
  if (nrow(spd) == length(height)) { spd <- t(spd); dir <- t(dir) }
  spd[!is.finite(spd)] <- NA; dir[!is.finite(dir)] <- NA
  t0 <- as.POSIXct(sprintf("%d-01-01", year), tz = "UTC")
  list(time = t0 + (yday - 1) * 86400, height = height, speed = spd, direction = dir)
}

#' Wind speed + direction interpolated to a target altitude and UTC time.
#' @return list(ws, wd, n_profiles) — NA speed/dir if the file doesn't cover the
#'   time (n_profiles = 0).
wind_at <- function(wp, time, alt_m, max_dt_min = 60) {
  dt <- abs(as.numeric(difftime(wp$time, time, units = "mins")))
  near <- which(dt <= max_dt_min)
  if (!length(near)) return(list(ws = NA_real_, wd = NA_real_, n_profiles = 0))
  # nearest-in-time profile, then linear interpolation over height
  i <- near[which.min(dt[near])]
  sp <- wp$speed[i, ]; dr <- wp$direction[i, ]
  ok <- is.finite(sp) & is.finite(dr) & is.finite(wp$height)
  if (sum(ok) < 2) return(list(ws = NA_real_, wd = NA_real_, n_profiles = length(near)))
  ws <- stats::approx(wp$height[ok], sp[ok], xout = alt_m, rule = 2)$y
  # interpolate direction via unit vectors to avoid the 360/0 wrap
  u <- sin(dr[ok]*pi/180); v <- cos(dr[ok]*pi/180)
  ui <- stats::approx(wp$height[ok], u, xout = alt_m, rule = 2)$y
  vi <- stats::approx(wp$height[ok], v, xout = alt_m, rule = 2)$y
  wd <- (atan2(ui, vi)*180/pi) %% 360
  list(ws = ws, wd = wd, n_profiles = length(near))
}
