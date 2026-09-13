# 46_mapl_emit_rgee.R ---------------------------------------------------------
# MAPL-EMIT methane plumes over the Denver analysis box, pulled into R with rgee,
# and compared with the Carbon Mapper catalog already in this repository.
#
#   Rscript scripts/46_mapl_emit_rgee.R
#
# Out: <OUT_DIR>/mapl_emit_denver_plumes.csv     one row per plume complex
#      <OUT_DIR>/mapl_vs_carbonmapper.csv        each MAPL plume, nearest CM detection
#      printed summary: counts by confidence, by year, and by site cluster
#
# WHAT THIS DATASET IS. MAPL-EMIT (Google Research / Nature Trace) is a vision
# transformer run over full EMIT radiance granules, Aug 2022 - Jun 2026, 60 m. Each
# plume complex carries a methane column enhancement field (ppm-m), an instance mask,
# a source location and a confidence label. It is the SAME instrument as the "emi"
# rows in results/carbonmapper_denver_plumes.csv, read by a different algorithm — a
# recall and persistence check, not an independent measurement.
#
# IT REPORTS ENHANCEMENTS (ppm-m), NOT EMISSION RATES. Converting to kg/hr needs an
# IME calculation with a wind field and adds its own uncertainty, so nothing here is
# directly comparable to the 4.6-10.7 t/hr campaign estimate or to the inventory
# sectors. Carbon Mapper stays the quantitative source.
#
# CONFIDENCE MATTERS. The producers' own human review puts the false-positive rate at
# 3-5% for 'high' (a plume seen in three or more EMIT observations) and 50-55% for
# 'medium'. This script keeps both but labels them, and the summary is by confidence.
# The model also misses about 16% of expert-annotated NASA EMIT L2B plumes.
#
# SETUP, ONCE:
#   install.packages("rgee"); rgee::ee_install()      # or use an existing Python env
#   rgee::ee_Initialize(project = "ee-priyankadesouza")
# Earth Engine authentication is interactive and browser-based; it is not stored in
# this repository and no credential belongs in this file.
# -----------------------------------------------------------------------------
# Find the repository root (the directory holding config.R). This script is useful as a
# loose copy outside scripts/ — e.g. dropped in the Methane_AMMBEC data folder — so look
# in the usual places rather than assuming the working directory. METHANE_REPO wins.
proj <- local({
  cands <- c(Sys.getenv("METHANE_REPO"), ".", "..", "methane_AMMBEC",
             file.path("..", "methane_AMMBEC"), file.path("..", "..", "methane_AMMBEC"))
  hit <- cands[nzchar(cands) & file.exists(file.path(cands, "config.R"))]
  if (!length(hit))
    stop("config.R not found. Run this from the repository (or its scripts/ folder), ",
         "or set METHANE_REPO to the folder that holds config.R.")
  hit[1]
})
source(file.path(proj, "config.R"))

# Write beside the other outputs whichever layout this copy uses.
if (!nzchar(Sys.getenv("METHANE_OUT_DIR"))) {
  sib <- normalizePath(file.path(proj, "..", "outputs"), mustWork = FALSE)
  if (dir.exists(sib)) OUT_DIR <- sib
}
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

EE_PROJECT <- Sys.getenv("EE_PROJECT", unset = "ee-priyankadesouza")
ASSET      <- "projects/nature-trace/assets/ghg/emit/mapl_emit_plumes_v1_0"
OUT_CSV    <- file.path(OUT_DIR, "mapl_emit_denver_plumes.csv")

for (p in c("rgee", "sf")) if (!requireNamespace(p, quietly = TRUE))
  stop("package '", p, "' is required: install.packages('", p, "')")
suppressMessages({ library(rgee); library(sf) })

message("initialising Earth Engine, project ", EE_PROJECT)
ee_Initialize(project = EE_PROJECT, drive = FALSE)

box <- ee$Geometry$Rectangle(c(URBAN_BOX$lon_w, URBAN_BOX$lat_s,
                               URBAN_BOX$lon_e, URBAN_BOX$lat_n))
ic  <- ee$ImageCollection(ASSET)$filterBounds(box)

n <- tryCatch(ic$size()$getInfo(), error = function(e) {
  stop("could not read ", ASSET, ": ", conditionMessage(e),
       "\nCheck that the Earth Engine project has access to the Nature Trace catalog.")
})
message("plume complexes intersecting the box: ", n)
if (n == 0) { message("nothing to write."); quit(save = "no") }

# One feature per plume: every property the collection carries, plus the centroid of
# its footprint. Properties are taken wholesale rather than named, so a schema change
# upstream shows up as extra columns instead of a silent drop.
to_feature <- function(img) {
  g <- img$geometry()
  c <- g$centroid(100)
  ee$Feature(c, img$toDictionary()
    $set("image_id", img$get("system:index"))
    $set("millis", img$get("system:time_start"))
    $set("centroid_lon", c$coordinates()$get(0))
    $set("centroid_lat", c$coordinates()$get(1))
    $set("footprint_km2", g$area(100)$divide(1e6)))
}
fc <- ee$FeatureCollection(ic$map(ee_utils_pyfunc(to_feature)))

# Small result set (tens of plumes over one city), so pull it straight into R rather
# than round-tripping through Drive. If a wider box ever makes this too big, EE will
# say so and ee_table_to_drive is the fallback.
d <- tryCatch(ee_as_sf(fc, maxFeatures = 10000), error = function(e) {
  stop("could not fetch the features: ", conditionMessage(e),
       "\nFor a larger box use rgee::ee_table_to_drive() instead.")
})
d <- sf::st_drop_geometry(d)

# UTC timestamp from the millis property, if present.
if ("millis" %in% names(d))
  d$datetime_utc <- as.POSIXct(as.numeric(d$millis) / 1000, origin = "1970-01-01", tz = "UTC")
if ("datetime_utc" %in% names(d)) d$date <- as.Date(d$datetime_utc)
d$fetched <- as.character(Sys.Date())
write.csv(d, OUT_CSV, row.names = FALSE)
message("wrote ", OUT_CSV, " (", nrow(d), " plumes, ", ncol(d), " columns)")

# ---- summary ----------------------------------------------------------------
cat("\ncolumns returned:\n  ", paste(names(d), collapse = ", "), "\n", sep = "")
if ("confidence" %in% names(d)) {
  cat("\nby confidence:\n"); print(table(d$confidence, useNA = "ifany"))
}
if ("date" %in% names(d)) {
  cat("\ndate range: ", format(min(d$date, na.rm = TRUE)), " to ",
      format(max(d$date, na.rm = TRUE)), "\n", sep = "")
  cat("by year:\n"); print(table(format(d$date, "%Y")))
}

# Cluster on a ~4 km grid, the same grouping scripts/43 uses, so the site tallies from
# the two catalogs can be read side by side.
if (all(c("centroid_lon", "centroid_lat") %in% names(d))) {
  key <- paste(round(d$centroid_lat / 0.04), round(d$centroid_lon / 0.04))
  cat("\nby site (~4 km clusters):\n")
  for (k in names(sort(table(key), decreasing = TRUE))) {
    i <- key == k
    cat(sprintf("  %.3f, %.3f  n=%d%s\n", mean(d$centroid_lat[i]), mean(d$centroid_lon[i]),
                sum(i),
                if ("confidence" %in% names(d))
                  paste0("  (", paste(names(table(d$confidence[i])), table(d$confidence[i]),
                                      sep = ": ", collapse = ", "), ")") else ""))
  }
}

# ---- compare with the Carbon Mapper catalog ---------------------------------
cm_f <- c(file.path(OUT_DIR, "carbonmapper_denver_plumes.csv"),
          file.path(proj, "results", "carbonmapper_denver_plumes.csv"))
cm_f <- cm_f[file.exists(cm_f)]
if (length(cm_f) && all(c("centroid_lon", "centroid_lat") %in% names(d))) {
  cm <- read.csv(cm_f[1], stringsAsFactors = FALSE)
  cm <- cm[cm$gas == "CH4", , drop = FALSE]
  km <- function(lon1, lat1, lon2, lat2) {         # great-circle, km
    R <- 6371.0088; p <- pi / 180
    a <- sin((lat2 - lat1) * p / 2)^2 +
         cos(lat1 * p) * cos(lat2 * p) * sin((lon2 - lon1) * p / 2)^2
    2 * R * asin(pmin(1, sqrt(a)))
  }
  near <- t(vapply(seq_len(nrow(d)), function(i) {
    dd <- km(d$centroid_lon[i], d$centroid_lat[i], cm$lon, cm$lat)
    j <- which.min(dd)
    c(dist_km = round(dd[j], 2), idx = j)
  }, numeric(2)))
  out <- data.frame(d,
    cm_nearest_plume = cm$plume_id[near[, "idx"]],
    cm_nearest_km    = near[, "dist_km"],
    cm_nearest_date  = cm$date[near[, "idx"]],
    cm_nearest_kg_hr = cm$kg_hr[near[, "idx"]])
  write.csv(out, file.path(OUT_DIR, "mapl_vs_carbonmapper.csv"), row.names = FALSE)
  cat("\nagainst the Carbon Mapper catalog (", nrow(cm), " CH4 plumes):\n", sep = "")
  cat(sprintf("  MAPL plumes within 1 km of a Carbon Mapper detection: %d of %d\n",
              sum(near[, "dist_km"] <= 1), nrow(d)))
  cat(sprintf("  MAPL plumes with NO Carbon Mapper detection within 5 km: %d\n",
              sum(near[, "dist_km"] > 5)))
  cat("  (the second number is what MAPL adds: EMIT plumes the matched filter missed,\n",
      "   subject to the confidence caveat above)\n", sep = "")
  cat("  wrote ", file.path(OUT_DIR, "mapl_vs_carbonmapper.csv"), "\n", sep = "")
} else if (!length(cm_f)) {
  cat("\n(no carbonmapper_denver_plumes.csv found; run scripts/43 for the comparison)\n")
}
