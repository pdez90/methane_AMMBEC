# read_mobile.R --------------------------------------------------------------
# Base-R reader for the mobile "CAT" methane survey files, e.g.
#   "2023 Q1 .../20230216_HEPTerminal_CAT_Methane.csv"
# Despite the .csv extension these are TAB-delimited with carriage-return
# ('\r') line endings. Columns: Asset, UTC_Time, Methane_ppmv, Latitude,
# Longitude. CH4 only (no ethane); ~1 Hz. GPS is often missing at the start/end
# of a survey. No package dependencies.
# -----------------------------------------------------------------------------

#' Parse survey date, site label and asset (CAT or EMU) out of a survey filename.
parse_mobile_filename <- function(path) {
  stem <- tools::file_path_sans_ext(basename(path))
  m <- regmatches(stem, regexec("^(\\d{8})_(.+?)_(CAT|EMU)_Methane", stem,
                                ignore.case = TRUE))[[1]]
  if (length(m) == 0) return(list(date = NA, site = stem, asset = NA_character_))
  list(date = as.Date(m[2], format = "%Y%m%d"), site = m[3], asset = toupper(m[4]))
}

# --- CDPHE processing constants ----------------------------------------------
# These reproduce the mobile-toxics processing chain (deSouza et al., CDPHE mobile
# air-toxics manuscript, sections 2.1.1 and S1): the same Picarro G2204, the same
# inlet, the same delivered files, so the same two corrections apply here.
#
#   DELAY. A concentration is reported by the instrument after the air that
#   produced it travelled the 3 m inlet and through the analyser, so it must be
#   attributed to the position where that air ENTERED the inlet. CDPHE measured
#   the delay per vehicle: 21 s on the CAT lab, 17 s on the EMU lab. Timestamps
#   are therefore shifted BACK by the asset's delay, and the reading is given the
#   GPS position recorded at that earlier time. At a 25-40 km/h survey speed 21 s is
#   150-230 m of road, so without this a plume is mapped to the wrong block.
#   (Before 2 Oct 2026 only the timestamp was shifted; the position stayed in the
#   same row as the reading, so the map was not actually corrected.)
#
#   NATIVE CADENCE. The Picarro acquires roughly every 5 s, but CDPHE delivers
#   every channel on a common 1-s grid by carrying the most recent reading
#   forward. Those repeats are not independent measurements: averaging each 5-s
#   acquisition block (over the seconds that carry a value, with no gap filling)
#   collapses them, which is what the toxics analysis does for CH4, H2S and HCN.
#   The delivered series is kept alongside as CH4_ppmv_raw, because plume-shape
#   work needs the unaveraged signal.
#
#   GARAGE. Deployments often begin indoors; measurements within 100 m of the
#   ATOPs depot (39.785359 N, -105.104331 W) are not ambient air and are dropped.
#
# Values live in config.R when it has been sourced; the defaults here keep the
# reader usable on its own.
.mob <- function(name, default) {
  v <- get0(name, envir = globalenv(), ifnotfound = NULL)
  if (is.null(v)) default else v
}

#' Great-circle distance in metres (base R, WGS84 mean radius).
.haversine_m <- function(lon1, lat1, lon2, lat2) {
  R <- 6371008.8; d <- pi / 180
  dlat <- (lat2 - lat1) * d; dlon <- (lon2 - lon1) * d
  a <- sin(dlat / 2)^2 + cos(lat1 * d) * cos(lat2 * d) * sin(dlon / 2)^2
  2 * R * asin(pmin(1, sqrt(a)))
}

#' Read one CAT mobile survey.
#'
#' @param path Path to a *_CAT_Methane.csv file.
#' @param drop_no_gps Drop rows lacking a GPS fix (default TRUE).
#' @param correct Apply the CDPHE inlet-delay shift, garage exclusion and
#'   native-cadence averaging (default TRUE; see the constants block above).
#'   Set MOBILE_CORRECT <- FALSE in config.R to read the delivered signal as-is.
#' @return data.frame: Asset, timestamp (POSIXct UTC), CH4_ppmv, Latitude,
#'   Longitude, site, survey_date, and CH4_ppmv_raw (the delivered signal).
read_mobile <- function(path, drop_no_gps = TRUE,
                        correct = .mob("MOBILE_CORRECT", TRUE)) {
  # Tab-delimited with '\r' endings. Skip the file's own header row and supply
  # our own names (avoids the header/col.names length-mismatch warning and is
  # robust to trailing empty GPS fields).
  df <- utils::read.delim(path, header = FALSE, skip = 1, sep = "\t",
                          col.names = c("Asset", "UTC_Time", "CH4_ppmv",
                                        "Latitude", "Longitude"),
                          colClasses = "character", check.names = FALSE,
                          fill = TRUE)

  df$timestamp <- as.POSIXct(df$UTC_Time, tz = "UTC",
                             format = "%Y-%m-%d %H:%M:%S")
  for (c in c("CH4_ppmv", "Latitude", "Longitude"))
    df[[c]] <- suppressWarnings(as.numeric(df[[c]]))

  meta <- parse_mobile_filename(path)
  df$site <- meta$site
  df$survey_date <- meta$date

  # With the delay correction, a reading whose own row has no GPS fix can still be placed:
  # correct_mobile() takes its position from the GPS track at its sampling time and drops
  # it only if that time has no fix. Without the correction, rows without GPS are dropped.
  do_corr <- isTRUE(correct)
  if (drop_no_gps && !do_corr)
    df <- df[!is.na(df$Latitude) & !is.na(df$Longitude), , drop = FALSE]
  rownames(df) <- NULL
  df$CH4_ppmv_raw <- df$CH4_ppmv          # delivered signal, before averaging

  if (do_corr && nrow(df)) df <- correct_mobile(df, asset = meta$asset)

  df[, c("Asset", "timestamp", "CH4_ppmv", "CH4_ppmv_raw", "Latitude", "Longitude",
         "site", "survey_date")]
}

#' Apply the CDPHE inlet-delay shift, garage exclusion and native-cadence average.
#'
#' See the constants block above for why each step exists. Returns the same
#' columns it was given, with `timestamp` shifted, `CH4_ppmv` averaged to the
#' instrument's acquisition cadence, and `CH4_ppmv_raw` left as delivered.
correct_mobile <- function(df, asset = NA_character_) {
  delays <- .mob("MOBILE_DELAY_S", c(CAT = 21, EMU = 17))
  cadence <- .mob("MOBILE_CADENCE_S", 5)
  garage <- .mob("MOBILE_GARAGE", c(lat = 39.785359, lon = -105.104331))
  grad <- .mob("MOBILE_GARAGE_RADIUS_M", 100)
  MAX_GPS_GAP_S <- .mob("MOBILE_MAX_GPS_GAP_S", 5)

  # Asset from the file's own column, falling back to the filename, then CAT.
  a <- toupper(trimws(df$Asset[!is.na(df$Asset) & nzchar(trimws(df$Asset))]))
  a <- if (length(a)) names(sort(table(a), decreasing = TRUE))[1] else toupper(asset)
  if (is.na(a) || !a %in% names(delays)) a <- "CAT"
  # The reading reported at time t was sampled at t - delay, where the vehicle was
  # at the position its GPS logged at t - delay. Shift the time stamp and take the
  # position from the GPS track at the shifted time (linear interpolation between
  # fixes no more than MAX_GPS_GAP_S apart; a reading whose sampling time falls
  # outside the GPS track, or in a longer gap, has no reliable position and is dropped).
  t_rep <- as.numeric(df$timestamp)
  t_smp <- round(t_rep - delays[[a]])
  okp <- is.finite(t_rep) & is.finite(df$Latitude) & is.finite(df$Longitude)
  tg <- t_rep[okp]; o <- order(tg); tg <- tg[o]
  la <- df$Latitude[okp][o]; lo <- df$Longitude[okp][o]
  if (length(tg) >= 2) {
    ut <- unique(tg)
    la <- tapply(la, factor(tg, levels = ut), mean); lo <- tapply(lo, factor(tg, levels = ut), mean); tg <- ut
    k <- findInterval(t_smp, tg)
    gap <- ifelse(k >= 1 & k < length(tg), tg[pmin(k + 1, length(tg))] - tg[pmax(k, 1)], NA)
    exact <- k >= 1 & t_smp == tg[pmax(k, 1)]
    inside <- k >= 1 & (exact | (k < length(tg) & is.finite(gap) & gap <= MAX_GPS_GAP_S))
    df$Latitude  <- ifelse(inside, stats::approx(tg, la, xout = t_smp, rule = 2)$y, NA_real_)
    df$Longitude <- ifelse(inside, stats::approx(tg, lo, xout = t_smp, rule = 2)$y, NA_real_)
  } else {
    df$Latitude <- NA_real_; df$Longitude <- NA_real_
  }
  df$timestamp <- as.POSIXct(t_smp, origin = "1970-01-01", tz = "UTC")
  df <- df[is.finite(df$Latitude) & is.finite(df$Longitude), , drop = FALSE]
  if (!nrow(df)) return(df)

  keep <- .haversine_m(df$Longitude, df$Latitude, garage[["lon"]], garage[["lat"]]) > grad
  df <- df[keep & !is.na(keep), , drop = FALSE]
  if (!nrow(df)) return(df)

  # One value per second (the delivered grid can repeat a second), then the
  # cadence-block mean over the seconds that actually carry a value.
  sec <- as.numeric(df$timestamp)
  if (anyDuplicated(sec)) {
    o <- order(sec)
    df <- df[o, , drop = FALSE]; sec <- sec[o]
    g <- factor(sec, levels = unique(sec))
    df <- data.frame(Asset = tapply(df$Asset, g, function(x) x[1]),
                     timestamp = as.POSIXct(as.numeric(levels(g)),
                                            origin = "1970-01-01", tz = "UTC"),
                     CH4_ppmv = tapply(df$CH4_ppmv, g, mean, na.rm = TRUE),
                     CH4_ppmv_raw = tapply(df$CH4_ppmv_raw, g, mean, na.rm = TRUE),
                     Latitude = tapply(df$Latitude, g, mean, na.rm = TRUE),
                     Longitude = tapply(df$Longitude, g, mean, na.rm = TRUE),
                     site = df$site[1], survey_date = df$survey_date[1],
                     stringsAsFactors = FALSE)
    sec <- as.numeric(df$timestamp)
  }
  if (is.finite(cadence) && cadence > 1) {
    blk <- floor(sec / cadence)
    df$CH4_ppmv <- ave(df$CH4_ppmv, blk, FUN = function(x) mean(x, na.rm = TRUE))
  }
  rownames(df) <- NULL
  df
}
