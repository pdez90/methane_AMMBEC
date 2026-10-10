# airborne_blh.R ------------------------------------------------------------------
# Boundary-layer height measured along the Twin Otter's own track by the NOAA CSL
# airborne Doppler lidar MD02 (Brewer and Baidar; AMMBEC-AirborneDL-BLH_TwinOtter_
# <date>_R1[_Ln].ict, final R1 release of 8 Oct 2026). Each record gives the BLH in m
# above ground below the aircraft, roughly every 30 s, retrieved from the lidar's fit
# variance, range-corrected intensity gradient, wind-speed gradient and wind shear.
# Flags -9999 (missing), -8888 (below detection) and -7777 (above detection) are NA.
#
# Why: the ground lidar at the DSRC in Boulder sits ~40 km from the urban core and,
# under strong westerlies over the mountains, its vertical-velocity variance picks up
# turbulence aloft that is not connected to the surface (S. Baidar, pers. comm., 9 Oct
# 2026), which inflates its mixing height. The airborne product measures the boundary
# layer over the air the aircraft actually sampled.
#
# flight_mixing_height() is the one entry point for the analysis scripts:
#   source "airborne" (default, config BLH_SOURCE): median airborne BLH over the window
#     (in-box records if `box` is given and at least MIN_N of them exist), falling back
#     to the ground lidar (Dalek 2 at DSRC; PUMAS when Dalek has no data) for the five
#     flights without an airborne file;
#   source "ground": the ground lidar only (the sensitivity case).
# -----------------------------------------------------------------------------------
.abl_cache <- new.env(parent = emptyenv())
ABL_MIN_N <- 3L

read_airborne_blh_file <- function(path) {
  hdr <- readLines(path, n = 7, warn = FALSE)
  nh <- as.integer(strsplit(hdr[1], ",")[[1]][1])
  dd <- as.integer(strsplit(gsub(" ", "", hdr[7]), ",")[[1]][1:3])
  day0 <- as.POSIXct(sprintf("%04d-%02d-%02d", dd[1], dd[2], dd[3]), tz = "UTC")
  x <- utils::read.csv(path, skip = nh - 1, header = TRUE, strip.white = TRUE, check.names = FALSE)
  names(x) <- trimws(names(x))
  for (k in c("ground_height", "flight_altitude", "BLH_agl", "BLH_agl_hi", "BLH_agl_lo"))
    x[[k]][x[[k]] %in% c(-9999, -8888, -7777)] <- NA
  data.frame(time = day0 + x$UTC_start, lat = x$lat, lon = x$lon, ground_m = x$ground_height,
             alt_m = x$flight_altitude, blh_agl = x$BLH_agl, blh_hi = x$BLH_agl_hi, blh_lo = x$BLH_agl_lo)
}

#' The airborne-lidar file for an ARL-Suite flight id ("20240713_R0_L1", "20240709_R0").
airborne_blh_file <- function(flight, dir = AIRBORNE_BLH_DIR) {
  m <- regmatches(flight, regexec("^(\\d{8})_R\\d+(_L\\d)?$", flight))[[1]]
  if (!length(m) || !dir.exists(dir)) return(NA_character_)
  pat <- sprintf("^AMMBEC-AirborneDL-BLH_TwinOtter_%s_R[0-9A-Z]+%s\\.ict$", m[2], if (nzchar(m[3])) m[3] else "")
  f <- sort(list.files(dir, pattern = pat, full.names = TRUE), decreasing = TRUE)   # highest revision first
  if (length(f)) f[1] else NA_character_
}

airborne_blh_data <- function(flight, dir = AIRBORNE_BLH_DIR) {
  key <- paste(dir, flight)
  if (is.null(.abl_cache[[key]])) {
    f <- airborne_blh_file(flight, dir)
    .abl_cache[[key]] <- if (is.na(f)) NA else read_airborne_blh_file(f)
  }
  .abl_cache[[key]]
}

.in_box <- function(lat, lon, box) is.finite(lat) & lat >= box$lat_s & lat <= box$lat_n & lon >= box$lon_w & lon <= box$lon_e

#' Median airborne BLH (m AGL) over [t_start, t_end]; in-box records only when `box`
#' is given and at least ABL_MIN_N exist. Returns list(blh_m, n, in_box).
airborne_blh_window <- function(flight, t_start, t_end, box = NULL, dir = AIRBORNE_BLH_DIR) {
  a <- airborne_blh_data(flight, dir)
  if (!is.data.frame(a)) return(list(blh_m = NA_real_, n = 0L, in_box = FALSE))
  sel <- a$time >= t_start & a$time <= t_end & is.finite(a$blh_agl)
  if (!is.null(box)) {
    sb <- sel & .in_box(a$lat, a$lon, box)
    if (sum(sb) >= ABL_MIN_N) return(list(blh_m = stats::median(a$blh_agl[sb]), n = sum(sb), in_box = TRUE))
  }
  list(blh_m = if (any(sel)) stats::median(a$blh_agl[sel]) else NA_real_, n = sum(sel), in_box = FALSE)
}

#' Airborne BLH (m AGL) at each time: linear interpolation between valid records, NA
#' where the nearest valid record is more than max_gap_s away.
airborne_blh_at <- function(flight, times, max_gap_s = 180, dir = AIRBORNE_BLH_DIR) {
  a <- airborne_blh_data(flight, dir)
  if (!is.data.frame(a)) return(rep(NA_real_, length(times)))
  a <- a[is.finite(a$blh_agl), ]
  if (nrow(a) < 2) return(rep(NA_real_, length(times)))
  tt <- as.numeric(a$time); x <- as.numeric(times)
  v <- stats::approx(tt, a$blh_agl, xout = x, rule = 1)$y
  near <- vapply(x, function(z) if (is.finite(z)) min(abs(tt - z)) else Inf, numeric(1))
  v[near > max_gap_s] <- NA_real_
  v
}

#' Ground-lidar mixing height (Dalek 2 at the DSRC; PUMAS when Dalek has no data).
ground_mixing_height <- function(date, t_start, t_end) {
  if (!exists("blh_flight")) source(file.path(get0("proj", mode = "character", ifnotfound = "."), "R", "lidar_blh.R"))
  b <- tryCatch(blh_flight(as.character(date), t_start, t_end, LIDAR_DIR), error = function(e) NULL)
  if (!is.null(b) && isTRUE(b$n > 0) && is.finite(b$blh_m)) return(list(blh_m = b$blh_m, n = b$n, src = "ground lidar (DSRC)"))
  if (exists("PUMAS_DIR") && dir.exists(PUMAS_DIR)) {
    b <- tryCatch(blh_flight(as.character(date), t_start, t_end, PUMAS_DIR), error = function(e) NULL)
    if (!is.null(b) && isTRUE(b$n > 0) && is.finite(b$blh_m)) return(list(blh_m = b$blh_m, n = b$n, src = "ground lidar (PUMAS)"))
  }
  list(blh_m = NA_real_, n = 0L, src = "none")
}

#' Mixing height for a flight window under the configured source.
flight_mixing_height <- function(flight, date, t_start, t_end, box = NULL,
                          source = get0("BLH_SOURCE", ifnotfound = "airborne")) {
  if (identical(source, "airborne")) {
    a <- airborne_blh_window(flight, t_start, t_end, box)
    if (a$n >= ABL_MIN_N && is.finite(a$blh_m))
      return(list(blh_m = a$blh_m, n = a$n, src = if (a$in_box) "airborne lidar (in box)" else "airborne lidar"))
  }
  ground_mixing_height(date, t_start, t_end)
}
