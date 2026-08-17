# read_mobile.R --------------------------------------------------------------
# Base-R reader for the mobile "CAT" methane survey files, e.g.
#   "2023 Q1 .../20230216_HEPTerminal_CAT_Methane.csv"
# Despite the .csv extension these are TAB-delimited with carriage-return
# ('\r') line endings. Columns: Asset, UTC_Time, Methane_ppmv, Latitude,
# Longitude. CH4 only (no ethane); ~1 Hz. GPS is often missing at the start/end
# of a survey. No package dependencies.
# -----------------------------------------------------------------------------

#' Parse survey date + site label out of a CAT filename.
parse_mobile_filename <- function(path) {
  stem <- tools::file_path_sans_ext(basename(path))
  m <- regmatches(stem, regexec("^(\\d{8})_(.+?)_CAT_Methane", stem,
                                ignore.case = TRUE))[[1]]
  if (length(m) == 0) return(list(date = NA, site = stem))
  list(date = as.Date(m[2], format = "%Y%m%d"), site = m[3])
}

#' Read one CAT mobile survey.
#'
#' @param path Path to a *_CAT_Methane.csv file.
#' @param drop_no_gps Drop rows lacking a GPS fix (default TRUE).
#' @return data.frame: Asset, timestamp (POSIXct UTC), CH4_ppmv, Latitude,
#'   Longitude, site, survey_date.
read_mobile <- function(path, drop_no_gps = TRUE) {
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
  df[, c("Asset", "timestamp", "CH4_ppmv", "Latitude", "Longitude",
         "site", "survey_date")]
}
