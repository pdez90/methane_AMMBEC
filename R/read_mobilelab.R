# read_mobilelab.R -----------------------------------------------------------
# Reader for the NOAA CSL Mobile Lab data (AMMBEC MobileLab download), which
# ships as SEPARATE ICARTT files per instrument that share one clock
# (Timewave_UTC, seconds past midnight):
#   *MetNav*   GPS position, ground speed, heading, T/P/RH, sonic wind
#   *Picarro*  CO2, CH4, CO, H2O (raw and _i_ interpolated-to-second)
#   *O3*       ozone
#   *jNO2*     NO2 photolysis frequency
# This merges them on the integer UTC second into one table with position +
# chemistry. Uses read_icartt() (they are ICARTT 1001). Unlike the CAT surveys,
# these carry CH4 AND position AND co-pollutants.
# -----------------------------------------------------------------------------

#' Find the four MobileLab ICARTT files for a date in a directory.
#' @param dir folder containing the *_MobileLab_<date>_*.ict files.
#' @param date_str eight-digit date, e.g. "20240709".
mobilelab_files <- function(dir, date_str) {
  all <- list.files(dir, pattern = paste0("MobileLab_", date_str, ".*\\.ict$"),
                    full.names = TRUE, ignore.case = TRUE)
  pick <- function(key) all[grepl(key, basename(all), ignore.case = TRUE)][1]
  list(metnav = pick("MetNav"), picarro = pick("Picarro"),
       o3 = pick("O3"), jno2 = pick("jNO2"))
}

#' Read + merge the Mobile Lab instruments for one date onto a common second.
#'
#' @param dir,date_str passed to mobilelab_files().
#' @param drop_no_gps drop rows without a GPS fix (default TRUE).
#' @return data.frame: timestamp, Latitude, Longitude, Alt_m, GndSpd, Heading,
#'   AirTemp_C, Pressure_mb, RH_pct, WindDir, WindSpd, CH4_ppb, CO2_ppm, CO_ppb,
#'   H2O_ppmv, O3_ppb, jNO2 (columns present depend on which files exist).
read_mobilelab <- function(dir, date_str, drop_no_gps = TRUE) {
  f <- mobilelab_files(dir, date_str)
  if (is.na(f$metnav)) stop("No MetNav file for ", date_str, " in ", dir)

  # First column of a read_icartt() frame is always the independent time
  # variable in seconds-past-midnight (Timewave_UTC, or Time_Mid for jNO2).
  key <- function(df) round(df[[1]])

  mn <- read_icartt(f$metnav)$data
  base <- data.frame(sec = key(mn),
                     timestamp = mn$timestamp,
                     Latitude = mn$GPS_Lat, Longitude = mn$GPS_Lon,
                     Alt_m = mn$GPS_Alt_m, GndSpd = mn$GPS_GndSpd_m_s,
                     Heading = mn$GPS_Heading_deg,
                     AirTemp_C = mn$AirTemp_C, Pressure_mb = mn$Pressure_mb,
                     RH_pct = mn$RelHumidity_pct,
                     WindDir = mn$WindDir_corr_deg, WindSpd = mn$WindSpd_corr_m_s,
                     stringsAsFactors = FALSE)

  add <- function(base, file, cols) {
    if (is.na(file)) return(base)
    d <- read_icartt(file)$data
    take <- data.frame(sec = key(d))
    for (nm in names(cols)) if (cols[[nm]] %in% names(d)) take[[nm]] <- d[[cols[[nm]]]]
    merge(base, take, by = "sec", all.x = TRUE)
  }
  # Prefer the interpolated-to-second Picarro columns (CH4_i_ppb, ...).
  base <- add(base, f$picarro, list(CH4_ppb = "CH4_i_ppb", CO2_ppm = "CO2_i_ppm",
                                    CO_ppb = "CO_i_ppb", H2O_ppmv = "H2O_i_ppmv"))
  base <- add(base, f$o3,   list(O3_ppb = "O3_ppb"))
  base <- add(base, f$jno2, list(jNO2 = "jNO2"))

  base <- base[order(base$sec), ]
  if (drop_no_gps) base <- base[is.finite(base$Latitude) & is.finite(base$Longitude), ]
  rownames(base) <- NULL
  base$site <- "MobileLab"; base$survey_date <- as.Date(read_icartt(f$metnav)$meta$date)
  base
}
