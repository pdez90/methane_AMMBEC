# 01_inventory.R -------------------------------------------------------------
# Walk the whole data tree and build manifests of every aircraft flight and
# mobile survey: date, site, row count, time span, species present, spatial
# bounding box. This is the map you use to decide what to analyze.
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

# Many archived ICARTT files are non-in-situ instruments (AMAX-DOAS, jNO2) with no
# position or time columns. min()/max() on an all-NA column returns +/-Inf AND emits
# a warning, which produced "50 or more warnings" per run and buried real messages.
# These helpers return NA silently instead; the affected rows are inventory-only and
# are never used downstream (only the 22 files carrying CH4 and C2H6 are analysed).
.smin <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
.smax <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
.stime <- function(x, first = TRUE) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  format(if (first) min(x) else max(x), "%H:%M:%S")
}

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
      t_start = .stime(d$timestamp, TRUE),
      t_end   = .stime(d$timestamp, FALSE),
      has_CH4  = "CH4_ppb"  %in% names(d),
      has_C2H6 = "C2H6_ppb" %in% names(d),
      lat_min = round(.smin(d$Latitude), 3),
      lat_max = round(.smax(d$Latitude), 3),
      lon_min = round(.smin(d$Longitude), 3),
      lon_max = round(.smax(d$Longitude), 3),
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
      t_start = .stime(d$timestamp, TRUE),
      t_end   = .stime(d$timestamp, FALSE),
      ch4_med = round(stats::median(d$CH4_ppmv, na.rm = TRUE), 3),
      ch4_max = round(.smax(d$CH4_ppmv), 3),
      lat_min = round(.smin(d$Latitude), 3),
      lat_max = round(.smax(d$Latitude), 3),
      lon_min = round(.smin(d$Longitude), 3),
      lon_max = round(.smax(d$Longitude), 3),
      stringsAsFactors = FALSE)
  }, error = function(e) { message("  ! ", basename(p), ": ", conditionMessage(e)); NULL })
})
mobile_df <- do.call(rbind, mrows)
write.csv(mobile_df, file.path(OUT_DIR, "manifest_mobile.csv"), row.names = FALSE)

message("\nWrote:")
message("  ", file.path(OUT_DIR, "manifest_flights.csv"), " (", nrow(flights_df), " flights)")
message("  ", file.path(OUT_DIR, "manifest_mobile.csv"),  " (", nrow(mobile_df),  " surveys)")
