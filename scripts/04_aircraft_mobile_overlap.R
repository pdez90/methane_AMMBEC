# 04_aircraft_mobile_overlap.R -----------------------------------------------
# Where do the ground mobile surveys and the Twin Otter flights coincide?
# The aircraft flew late-June/July 2024, so temporal overlap can only come from
# mobile surveys on those same dates. For every same-day flight+survey pair this
# reports the closest approach (km) between the aircraft's low-altitude track
# and the mobile drive, flags spatial co-location, and — where they were close —
# compares median CH4. This is the basis for cross-validating the two platforms.
#
# Only aircraft files carrying in-situ CH4 (the "ARL-Suite" in-situ product) are
# used; the AMAX-DOAS / jNO2 column files have no CH4 and are ignored.
#
# Run:  Rscript scripts/04_aircraft_mobile_overlap.R
# Out:  <OUT_DIR>/aircraft_mobile_overlap.csv
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
source(file.path(proj, "R", "read_icartt.R"))
source(file.path(proj, "R", "read_mobile.R"))
source(file.path(proj, "R", "paths.R"))

COLOCATE_KM <- 5        # "co-located" if closest approach is within this
LOW_ALT_M   <- 500      # aircraft points this low (AGL) are ground-comparable

# Great-circle distance (km); vectorised over the second point.
.haversine <- function(lat1, lon1, lat2, lon2) {
  R <- 6371; d2r <- pi / 180
  dlat <- (lat2 - lat1) * d2r; dlon <- (lon2 - lon1) * d2r
  h <- sin(dlat / 2)^2 + cos(lat1 * d2r) * cos(lat2 * d2r) * sin(dlon / 2)^2
  2 * R * asin(pmin(1, sqrt(h)))
}

# --- Index aircraft flights that actually contain CH4, by date ---------------
flights <- list_flights(DATA_DIR)
air <- list()
for (fp in flights) {
  ic <- tryCatch(read_icartt(fp), error = function(e) NULL)
  if (is.null(ic) || !("CH4_ppb" %in% names(ic$data))) next
  d <- ic$data
  d <- d[is.finite(d$Latitude) & is.finite(d$Longitude), ]
  if (!nrow(d)) next
  air[[fp]] <- list(path = fp, date = as.Date(ic$meta$date),
                    low = d[is.finite(d$ALTAGL) & d$ALTAGL < LOW_ALT_M, ], all = d)
}
air_dates <- as.Date(vapply(air, function(a) as.character(a$date), ""))
message("Aircraft flights with in-situ CH4: ", length(air),
        " on dates ", paste(sort(unique(air_dates)), collapse = ", "))

# --- Walk mobile surveys; match same-day flights -----------------------------
mob <- list_mobile(DATA_DIR)
rows <- list()
for (mp in mob) {
  md <- tryCatch(read_mobile(mp), error = function(e) NULL)
  if (is.null(md) || nrow(md) < 20) next
  mdate <- md$survey_date[1]
  hits <- which(air_dates == mdate)
  if (!length(hits)) next

  # thin the mobile track for a cheap closest-approach search
  ms <- md[unique(round(seq(1, nrow(md), length.out = min(300, nrow(md))))), ]
  for (k in hits) {
    a <- air[[names(air)[k]]]
    alow <- if (nrow(a$low)) a$low else a$all   # fall back to full track
    # minimum distance from any aircraft (low) point to any mobile point
    mind <- Inf
    for (i in seq_len(nrow(alow))) {
      dk <- min(.haversine(alow$Latitude[i], alow$Longitude[i],
                           ms$Latitude, ms$Longitude), na.rm = TRUE)
      if (dk < mind) mind <- dk
    }
    coloc <- is.finite(mind) && mind <= COLOCATE_KM
    rows[[length(rows) + 1]] <- data.frame(
      date = as.character(mdate),
      flight = basename(a$path), mobile = basename(mp), site = md$site[1],
      min_approach_km = round(mind, 2),
      colocated = coloc,
      ch4_air_ppmv_med  = if (coloc) round(stats::median(alow$CH4_ppb, na.rm = TRUE)/1000, 3) else NA,
      ch4_mob_ppmv_med  = if (coloc) round(stats::median(md$CH4_ppmv, na.rm = TRUE), 3) else NA,
      stringsAsFactors = FALSE)
  }
}

if (length(rows)) {
  res <- do.call(rbind, rows)
  res <- res[order(res$date, res$min_approach_km), ]
} else {
  res <- data.frame(note = "No mobile survey shares a date with any CH4 flight.")
}
write.csv(res, file.path(OUT_DIR, "aircraft_mobile_overlap.csv"), row.names = FALSE)

message("\nSame-day flight/survey pairs: ", if ("note" %in% names(res)) 0 else nrow(res))
if (!("note" %in% names(res))) {
  message("Co-located (<= ", COLOCATE_KM, " km): ", sum(res$colocated))
  message("Closest same-day approach overall: ",
          round(min(res$min_approach_km, na.rm = TRUE), 1), " km")
}
print(utils::head(res, 30))
