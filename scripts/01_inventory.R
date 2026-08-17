# 01_inventory.R -------------------------------------------------------------
# Walk the whole data tree and build manifests of every aircraft flight and
# mobile survey: date, site, row count, time span, species present, spatial
# bounding box. This is the map you use to decide what to analyse.
#
# Run:  Rscript scripts/01_inventory.R
# Out:  <OUT_DIR>/manifest_flights.csv, <OUT_DIR>/manifest_mobile.csv
# -----------------------------------------------------------------------------
# Robust sourcing whether run from repo root or from scripts/.
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
source(file.path(proj, "R", "read_icartt.R"))
source(file.path(proj, "R", "read_mobile.R"))
source(file.path(proj, "R", "paths.R"))

message("Scanning: ", DATA_DIR)

# --- Aircraft flights --------------------------------------------------------
flights <- list_flights(DATA_DIR)
message("Found ", length(flights), " aircraft ICARTT files.")
frows <- lapply(flights, function(p) {
  out <- tryCatch({
    ic <- read_icartt(p)
    d <- ic$data
    data.frame(
      file = basename(p),
      date = as.character(ic$meta$date),
      n = nrow(d),
      t_start = format(min(d$timestamp, na.rm = TRUE), "%H:%M:%S"),
      t_end   = format(max(d$timestamp, na.rm = TRUE), "%H:%M:%S"),
      has_CH4  = "CH4_ppb"  %in% names(d),
      has_C2H6 = "C2H6_ppb" %in% names(d),
      lat_min = round(min(d$Latitude, na.rm = TRUE), 3),
      lat_max = round(max(d$Latitude, na.rm = TRUE), 3),
      lon_min = round(min(d$Longitude, na.rm = TRUE), 3),
      lon_max = round(max(d$Longitude, na.rm = TRUE), 3),
      stringsAsFactors = FALSE)
  }, error = function(e) {
    message("  ! ", basename(p), ": ", conditionMessage(e)); NULL
  })
  out
})
flights_df <- do.call(rbind, frows)
write.csv(flights_df, file.path(OUT_DIR, "manifest_flights.csv"), row.names = FALSE)

# --- Mobile surveys ----------------------------------------------------------
mob <- list_mobile(DATA_DIR)
message("Found ", length(mob), " mobile survey files.")
mrows <- lapply(mob, function(p) {
  tryCatch({
    d <- read_mobile(p)
    if (nrow(d) == 0) return(NULL)
    data.frame(
      file = basename(p),
      quarter = basename(dirname(p)),
      site = d$site[1],
      date = as.character(d$survey_date[1]),
      n = nrow(d),
      t_start = format(min(d$timestamp, na.rm = TRUE), "%H:%M:%S"),
      t_end   = format(max(d$timestamp, na.rm = TRUE), "%H:%M:%S"),
      ch4_med = round(stats::median(d$CH4_ppmv, na.rm = TRUE), 3),
      ch4_max = round(max(d$CH4_ppmv, na.rm = TRUE), 3),
      lat_min = round(min(d$Latitude, na.rm = TRUE), 3),
      lat_max = round(max(d$Latitude, na.rm = TRUE), 3),
      lon_min = round(min(d$Longitude, na.rm = TRUE), 3),
      lon_max = round(max(d$Longitude, na.rm = TRUE), 3),
      stringsAsFactors = FALSE)
  }, error = function(e) { message("  ! ", basename(p), ": ", conditionMessage(e)); NULL })
})
mobile_df <- do.call(rbind, mrows)
write.csv(mobile_df, file.path(OUT_DIR, "manifest_mobile.csv"), row.names = FALSE)

message("\nWrote:")
message("  ", file.path(OUT_DIR, "manifest_flights.csv"), " (", nrow(flights_df), " flights)")
message("  ", file.path(OUT_DIR, "manifest_mobile.csv"),  " (", nrow(mobile_df),  " surveys)")
