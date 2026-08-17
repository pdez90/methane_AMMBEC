# paths.R --------------------------------------------------------------------
# Discover the data files in the MethaneData tree.
#   - Aircraft/ ............ NOAA Twin Otter ICARTT (.ict) flights (July 2024)
#   - "YYYY Qn - ..." ...... ground-based mobile "CAT" surveys (2023-2025)
# Base R.
# -----------------------------------------------------------------------------

#' List all aircraft ICARTT files under <data_dir>/Aircraft.
list_flights <- function(data_dir) {
  ac <- file.path(data_dir, "Aircraft")
  f <- list.files(ac, pattern = "\\.ict$", recursive = TRUE, full.names = TRUE)
  f[order(basename(f))]
}

#' List all mobile CAT survey CSVs across the quarterly folders.
list_mobile <- function(data_dir) {
  f <- list.files(data_dir, pattern = "_CAT_Methane\\.csv$",
                  recursive = TRUE, full.names = TRUE)
  f[order(basename(f))]
}

#' Pull the flight date out of an AMMBEC ICARTT filename
#' (e.g. ..._TwinOtter_20240709_R0_L1.ict -> Date 2024-07-09).
flight_date_from_name <- function(path) {
  m <- regmatches(basename(path), regexpr("_(\\d{8})_", basename(path)))
  if (length(m) == 0) return(as.Date(NA))
  as.Date(gsub("_", "", m), format = "%Y%m%d")
}
