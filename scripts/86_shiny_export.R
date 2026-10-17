# 86_shiny_export.R -----------------------------------------------------------
# Writes the data the Shiny explorer (shiny/app.R) reads, into shiny/data/.
#
# The app recomputes nothing from raw files: it reads the pipeline's summary
# CSVs (copied here from OUT_DIR / results/) plus five small derived tables that
# only this script makes from the raw data:
#   tracks.csv          every flight at 10 s: lat, lon, altitude, CH4 (map tracks)
#   urban_samples.csv   every 1 Hz sample on an urban level leg: raw CH4, C2H6, CO and the
#                       paper's enhancements (joined-leg background, as scripts 15 and 21);
#                       the Background tab recomputes the background from the raw columns
#   basin_samples.csv   the same for basin legs (scripts 31)
#   legs.csv            all level legs with region, geometry and per-leg slope
#   epa_cells.csv       EPA gridded GHGI 0.1-degree cells overlapping the box:
#                       fossil and biogenic methane (t/h) and fossil fraction
#   cdphe_hotspots.csv  CDPHE survey plume samples binned to ~50 m: n, p95, max
#
# Run after the pipeline:  Rscript scripts/86_shiny_export.R
# Needs terra + the GHGI file for epa_cells.csv (skipped with a message otherwise).
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
for (f in c("read_icartt", "paths", "massbalance", "enhancements", "ratios"))
  source(file.path(proj, "R", paste0(f, ".R")))
DEST <- file.path(proj, "shiny", "data"); dir.create(DEST, recursive = TRUE, showWarnings = FALSE)

## ---- 1. summary CSVs the app reads as they are ---------------------------------
COPY <- c("table1.csv", "table1_full.csv", "emission_estimates.csv", "source_attribution.csv",
          "beta_sensitivity.csv", "beta_breakeven.csv", "beta_endmember_ARC.csv",
          "two_endmember_sensitivity.csv", "two_endmember_flights.csv", "two_endmember_breakeven.csv",
          "two_endmember_floor_flights.csv", "perleg_fossil_fraction.csv", "qc_robustness.csv",
          "wind_fossil.csv", "wind_source_exposure_legs.csv", "stilt_exposure_legs.csv",
          "stilt_exposure_summary.csv", "arc_ground_plumes.csv", "arc_ground_ratios.csv",
          "arc_ground_ambient.csv", "arc_ground_summary.csv", "mobile_survey_summary.csv",
          "mobile_trends.csv", "mobile_trend_fits.csv", "basin_ethane_methane.csv", "basin_summary.csv",
          "basin_implied_source_ratio.csv", "fossil_signature_grid.csv", "spatial_match_cells.csv",
          "spatial_match_summary.csv", "inventory_comparison.csv",
          "gra2pes_cells_v2.0beta.csv", "gra2pes_cells_v2.0beta_summary.csv", "co_diurnal_sensitivity.csv", "stilt_exposure_quantities.csv",
          "v2_pointsource_hotspots.csv", "v2_pointsource_encounters.csv", "carbonmapper_denver_sources.csv",
          "carbonmapper_encounters.csv", "background_mode_sensitivity.csv", "background_aloft_sensitivity.csv",
          "background_grid_sensitivity.csv", "background_grid_summary.csv", "enh_threshold_sensitivity.csv",
          "stilt_exposure_receptors.csv", "stilt_particles.csv.gz", "stilt_centroids.csv",
          "lag_sensitivity_summary.csv", "manifest_flights.csv", "paper_values.json")
miss <- character(0)
for (f in COPY) {
  src <- file.path(OUT_DIR, f)
  if (!file.exists(src)) src <- file.path(proj, "results", f)
  if (file.exists(src)) file.copy(src, file.path(DEST, f), overwrite = TRUE) else miss <- c(miss, f)
}
for (f in c("literature_basin_ratios.csv", "biogenic_sources.csv", "denver7_metro_outline.csv", "psco_gas_quality.csv"))
  file.copy(file.path(proj, f), file.path(DEST, f), overwrite = TRUE)
if (length(miss)) message("not found (app will hide what needs them): ", paste(miss, collapse = ", "))

## ---- 1b. particle animations of script 108 (served from shiny/www as downloads) ----
WWW <- file.path(proj, "shiny", "www"); dir.create(WWW, showWarnings = FALSE)
mov <- list.files(file.path(OUT_DIR, "figures"), pattern = "^stilt_particles_.*\\.(gif|mp4)$", full.names = TRUE)
for (f in mov) file.copy(f, file.path(WWW, basename(f)), overwrite = TRUE)
if (length(mov)) message(length(mov), " particle animation file(s) copied to shiny/www/")

## ---- 2. aircraft tracks, leg samples, legs ---------------------------------------
tracks <- list(); us <- list(); bs <- list(); legs <- list()
for (p in list_flights(DATA_DIR)) {
  if (!grepl("ARL-Suite", p)) next
  ic <- tryCatch(read_icartt(p), error = function(e) NULL); if (is.null(ic)) next
  fl <- sub("AMMBEC-ARL-Suite_TwinOtter_", "", sub("\\.ict$", "", basename(p)))
  d <- detect_level_legs(ic$data)
  agl <- if ("ALTAGL" %in% names(d)) d$ALTAGL else NA_real_
  i10 <- seq(1, nrow(d), by = 10)
  tracks[[fl]] <- data.frame(flight = fl, time_utc = format(d$timestamp[i10], "%H:%M:%S"),
                             lat = round(d$Latitude[i10], 4), lon = round(d$Longitude[i10], 4),
                             agl_m = round(agl[i10]), ch4_ppb = round(d$CH4_ppb[i10], 1),
                             leg_id = d$leg_id[i10], stringsAsFactors = FALSE)
  if (!any(d$leg_id > 0)) next
  lm_ <- tag_region(leg_metrics(d)); lm_$flight <- fl
  legs[[fl]] <- lm_
  bg_track <- rolling_baseline(d$CH4_ppb)            # the along-track CH4 background of script 85 (whole record)
  for (reg in c("urban", "basin")) {
    ids <- lm_$leg_id[lm_$region == reg]; if (!length(ids)) next
    z <- d[d$leg_id %in% ids, ]
    if (nrow(z) < 50) next
    for (v in intersect(c("CH4_ppb", "C2H6_ppb", "CO_ppb"), names(z))) z <- add_enhancements(z, v)
    out <- data.frame(flight = fl, leg_id = z$leg_id, time_utc = format(z$timestamp, "%H:%M:%S"),
                      lat = round(z$Latitude, 4), lon = round(z$Longitude, 4),
                      agl_m = round(if ("ALTAGL" %in% names(z)) z$ALTAGL else NA_real_),
                      ch4_ppb = round(z$CH4_ppb, 1), c2h6_ppb = round(z$C2H6_ppb, 2),
                      co_ppb = if ("CO_ppb" %in% names(z)) round(z$CO_ppb, 1) else NA_real_,
                      bg_ch4_track = round(bg_track[d$leg_id %in% ids], 1),
                      dch4_ppb = round(z$CH4_ppb_enh, 1), dc2h6_ppb = round(z$C2H6_ppb_enh, 2),
                      dco_ppb = if ("CO_ppb_enh" %in% names(z)) round(z$CO_ppb_enh, 1) else NA_real_,
                      wd_deg = round(z$WD), ws_ms = round(z$WS, 1), stringsAsFactors = FALSE)
    out$gated <- is.finite(out$dch4_ppb) & out$dch4_ppb > 20
    if (reg == "urban") us[[fl]] <- out else bs[[fl]] <- out
  }
}
TR <- do.call(rbind, tracks); write.csv(TR, file.path(DEST, "tracks.csv"), row.names = FALSE)
US <- do.call(rbind, us); write.csv(US, file.path(DEST, "urban_samples.csv"), row.names = FALSE)
BS <- do.call(rbind, bs); write.csv(BS, file.path(DEST, "basin_samples.csv"), row.names = FALSE)
LG <- do.call(rbind, legs)
pl <- read.csv(file.path(DEST, "perleg_fossil_fraction.csv"), stringsAsFactors = FALSE)
LG <- merge(LG, pl[, c("flight", "leg_id", "n_gated", "leg_slope", "leg_r")], by = c("flight", "leg_id"), all.x = TRUE)
write.csv(LG, file.path(DEST, "legs.csv"), row.names = FALSE)
message(sprintf("tracks %d rows, urban samples %d (%d gated), basin samples %d, legs %d",
                nrow(TR), nrow(US), sum(US$gated), nrow(BS), nrow(LG)))

## ---- 3. EPA gridded GHGI cells over the box --------------------------------------
if (requireNamespace("terra", quietly = TRUE) && exists("GHGI_FILE") && file.exists(GHGI_FILE)) {
  MW_CH4 <- 16.04; NA_ <- 6.02214e23
  r <- terra::rast(GHGI_FILE); nm <- names(r)
  fi <- grepl("Natural_Gas|Petroleum|PostMeter|Coal|Abandoned_Oil", nm)
  bi <- grepl("Landfill|Wastewater|Enteric|Manure|Composting|Rice|Field_Burning", nm)
  area <- r[["grid_cell_area"]]
  to_thr <- function(x) x * area * MW_CH4 / NA_ * 3600 / 1e6
  fos <- to_thr(sum(r[[which(fi)]])); bio <- to_thr(sum(r[[which(bi)]]))
  b <- URBAN_BOX; ex <- terra::ext(b$lon_w - 0.1, b$lon_e + 0.1, b$lat_s - 0.1, b$lat_n + 0.1)
  fc <- terra::crop(fos, ex, snap = "out"); bc <- terra::crop(bio, ex, snap = "out")
  xy <- terra::xyFromCell(fc, seq_len(terra::ncell(fc)))
  E <- data.frame(lon = xy[, 1], lat = xy[, 2], fossil_t_hr = signif(terra::values(fc)[, 1], 4),
                  biogenic_t_hr = signif(terra::values(bc)[, 1], 4))
  E$fossil_frac <- round(E$fossil_t_hr / (E$fossil_t_hr + E$biogenic_t_hr), 3)
  write.csv(E, file.path(DEST, "epa_cells.csv"), row.names = FALSE)
  message(sprintf("epa_cells.csv: %d cells", nrow(E)))
} else message("terra or the GHGI file is missing: epa_cells.csv not written (the app hides the EPA map)")

## ---- 4. CDPHE survey plume samples, binned ---------------------------------------
hp <- file.path(OUT_DIR, "mobile_hotspots.csv")
if (file.exists(hp)) {
  H <- read.csv(hp, stringsAsFactors = FALSE)
  H$lat_b <- round(H$Latitude / 0.0005) * 0.0005; H$lon_b <- round(H$Longitude / 0.0005) * 0.0005
  key <- paste(H$site, H$lat_b, H$lon_b)
  agg <- data.frame(site = tapply(H$site, key, `[`, 1), lat = tapply(H$lat_b, key, `[`, 1),
                    lon = tapply(H$lon_b, key, `[`, 1), n = as.integer(tapply(H$CH4_ppmv_enh, key, length)),
                    n_surveys = as.integer(tapply(substr(H$timestamp, 1, 10), key, function(v) length(unique(v)))),
                    enh_p95_ppmv = round(tapply(H$CH4_ppmv_enh, key, stats::quantile, 0.95, na.rm = TRUE), 3),
                    enh_max_ppmv = round(tapply(H$CH4_ppmv_enh, key, max, na.rm = TRUE), 3))
  write.csv(agg, file.path(DEST, "cdphe_hotspots.csv"), row.names = FALSE)
  message(sprintf("cdphe_hotspots.csv: %d bins from %d plume samples", nrow(agg), nrow(H)))
} else message("mobile_hotspots.csv not found (script 03): cdphe_hotspots.csv not written")

sz <- sum(file.size(list.files(DEST, full.names = TRUE))) / 1e6
message(sprintf("shiny/data: %d files, %.1f MB", length(list.files(DEST)), sz))
