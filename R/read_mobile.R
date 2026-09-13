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
#   are therefore shifted BACK by the asset's delay. At a 25-40 km/h survey speed
#   21 s is 150-230 m of road, so without this a plume is mapped to the wrong
#   block.
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

  if (drop_no_gps)
    df <- df[!is.na(df$Latitude) & !is.na(df$Longitude), , drop = FALSE]
  rownames(df) <- NULL
  df$CH4_ppmv_raw <- df$CH4_ppmv          # delivered signal, before averaging

  if (isTRUE(correct) && nrow(df)) df <- correct_mobile(df, asset = meta$asset)

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

  # Asset from the file's own column, falling back to the filename, then CAT.
  a <- toupper(trimws(df$Asset[!is.na(df$Asset) & nzchar(trimws(df$Asset))]))
  a <- if (length(a)) names(sort(table(a), decreasing = TRUE))[1] else toupper(asset)
  if (is.na(a) || !a %in% names(delays)) a <- "CAT"
  df$timestamp <- as.POSIXct(round(as.numeric(df$timestamp) - delays[[a]]),
                             origin = "1970-01-01", tz = "UTC")

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
