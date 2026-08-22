# 05_massbalance_flux.R ------------------------------------------------------
# EXPLORATORY / NOT USED IN THE MANUSCRIPT. Computes a per-flight downwind-curtain
# "flux", but the AMMBEC legs are not curated, validated downwind curtains and no
# flight forms a valid closed loop (script 13). Its numerical fluxes are therefore
# NOT reported and nothing downstream reads them; the manuscript's urban emission
# rate comes from the enhancement-ratio method (scripts 15/21). Excluded from
# run_all.R and retained only as an exploratory diagnostic.
#
# Airborne mass-balance CH4 flux per flight (AMMBEC memo section 1).
# Detects level legs, computes per-leg enhancement + perpendicular wind + line
# integral, and integrates a downwind "curtain" up to a boundary-layer height.
#
# TWO-PASS WORKFLOW (read R/massbalance.R header first):
#   PASS 1 - discovery. Run with no config present. It analyzes ALL legs with a
#            default BLH, writes massbalance_legs.csv (inspect these!), and drops
#            a blank template `curtain_config.csv` next to config.R.
#   PASS 2 - curated. Open curtain_config.csv and, per flight, fill in:
#              leg_ids        the downwind-screen legs, e.g. "4;5;6;8"
#              blh_m          boundary-layer height (m AGL) from lidar/sounding
#              background_ppb upwind background (blank = auto 5th percentile)
#            Re-run. Flux is now computed from only those legs, that BLH, and
#            that background. Blank cells fall back to the automatic behavior.
#
# Run:  Rscript scripts/05_massbalance_flux.R [optional: one .ict] [optional: default BLH_m]
# Out:  <OUT_DIR>/massbalance_flux.csv, <OUT_DIR>/massbalance_legs.csv,
#       <OUT_DIR>/figures/<flight>_legs_altitude.png, and curtain_config.csv (template)
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
source(file.path(proj, "R", "read_icartt.R"))
source(file.path(proj, "R", "paths.R"))
source(file.path(proj, "R", "massbalance.R"))
# Optional: use wind-profiler winds in the curtain if the file is present.
wp <- NULL
if (exists("WINDPROF_FILE") && file.exists(WINDPROF_FILE)) {
  source(file.path(proj, "R", "read_windprof.R"))
  wp <- tryCatch(read_windprof(WINDPROF_FILE), error = function(e) NULL)
  if (!is.null(wp)) message("Wind profiler loaded (", WINDPROF_FILE,
                            ") - will be used where it covers a flight.")
}
# Optional: measured per-flight mixing height from the monthly velStats lidar.
have_lidar <- requireNamespace("ncdf4", quietly = TRUE) && exists("LIDAR_DIR") &&
  dir.exists(LIDAR_DIR) && length(list.files(LIDAR_DIR, pattern = "velStats_.*\\.nc"))
if (have_lidar) source(file.path(proj, "R", "lidar_blh.R"))

args <- commandArgs(trailingOnly = TRUE)
default_blh <- 2000                                 # fallback BLH (m AGL)
if (length(args)) {
  maybe <- suppressWarnings(as.numeric(args[length(args)]))
  if (!is.na(maybe)) { default_blh <- maybe; args <- args[-length(args)] }
}
args <- args[file.exists(args)]                     # ignore stray args (e.g. pasted comments)
flights <- if (length(args)) args else list_flights(DATA_DIR)

cfg <- read_curtain_config(CURTAIN_CONFIG)
if (is.null(cfg)) {
  message("No curtain_config.csv yet - PASS 1 (all legs, BLH=", default_blh,
          " m). A template will be written for you.")
} else {
  message("Using curtain_config.csv - PASS 2 (curated legs / BLH / background).")
}

flux_rows <- list(); leg_rows <- list(); info <- list()
for (p in flights) {
  ic <- tryCatch(read_icartt(p), error = function(e) NULL)
  if (is.null(ic) || !all(c("CH4_ppb","WS","WD","TrackAngleTrue","ALTGPS") %in% names(ic$data))) next
  fb <- basename(p)
  info[[p]] <- data.frame(flight = fb, date = as.character(ic$meta$date), stringsAsFactors = FALSE)

  d <- detect_level_legs(ic$data)
  if (!any(d$leg_id > 0)) { message("no level legs: ", fb); next }

  # per-flight config row (if any)
  row <- if (!is.null(cfg) && fb %in% names(cfg)) cfg[[fb]][1, ] else NULL
  man_bg <- if (!is.null(row)) suppressWarnings(as.numeric(row$background_ppb)) else NA
  sel    <- if (!is.null(row)) parse_leg_ids(row$leg_ids) else NULL
  # BLH priority: curated config value > measured lidar > default fallback.
  if (!is.null(row) && !is.na(suppressWarnings(as.numeric(row$blh_m)))) {
    blh <- as.numeric(row$blh_m); blh_src <- "config"
  } else {
    blh <- default_blh; blh_src <- "default"
    if (have_lidar) {
      tr <- range(d$timestamp, na.rm = TRUE)
      b <- blh_flight(ic$meta$date, tr[1], tr[2], LIDAR_DIR)
      if (isTRUE(b$n > 0) && is.finite(b$blh_m)) { blh <- round(b$blh_m); blh_src <- "lidar" }
    }
  }

  legs <- leg_metrics(d, background = if (is.na(man_bg)) NULL else man_bg, windprof = wp)
  if (is.null(legs) || !nrow(legs)) next
  wind_used <- if ("wind_src" %in% names(legs))
    names(sort(table(legs$wind_src), decreasing = TRUE))[1] else "aircraft"
  bg <- attr(legs, "background")
  bg_src <- if (is.na(man_bg)) "auto_p05" else "config"

  used <- legs
  if (!is.null(sel)) used <- legs[legs$leg_id %in% sel, ]
  if (!nrow(used)) { message("selected legs not found for ", fb, " - using all"); used <- legs }

  fx <- curtain_flux(used, blh_m = blh)
  tag <- tools::file_path_sans_ext(fb)
  legs$flight <- fb; legs$selected <- legs$leg_id %in% (if (is.null(sel)) legs$leg_id else sel)
  leg_rows[[p]] <- legs

  flux_rows[[p]] <- data.frame(
    flight = fb, date = as.character(ic$meta$date),
    n_legs_total = nrow(legs), n_legs_used = nrow(used),
    leg_ids_used = paste(used$leg_id, collapse = ";"),
    background_ppb = round(bg, 1), background_src = bg_src,
    blh_m = blh, blh_src = blh_src, wind_src = wind_used,
    flux_kg_hr = round(fx$flux_kg_hr, 1), flux_t_hr = round(fx$flux_t_hr, 3),
    stringsAsFactors = FALSE)

  # Diagnostic figure: selected legs highlighted.
  png(file.path(OUT_DIR, "figures", paste0(tag, "_legs_altitude.png")), 660, 560, res = 110)
  plot(legs$L_mol_per_m_s, legs$agl_m, pch = 21, cex = 1.3,
       bg = ifelse(legs$selected, "firebrick", "white"), col = "steelblue",
       xlab = "leg line integral  L  (mol / m / s)", ylab = "leg altitude (m AGL)",
       main = paste0(tag, "\nused ", nrow(used), "/", nrow(legs), " legs  flux~",
                     round(fx$flux_t_hr,2), " t/hr  BLH=", blh, "m (", blh_src, ")"))
  text(legs$L_mol_per_m_s, legs$agl_m, legs$leg_id, pos = 4, cex = 0.7)
  abline(h = blh, lty = 2, col = "gray50")
  dev.off()
  message("done: ", tag, "  used ", nrow(used), "/", nrow(legs),
          " legs  flux~", round(fx$flux_t_hr,2), " t/hr  (BLH ", blh, "m ", blh_src, ")")
}

flux <- do.call(rbind, flux_rows)
write.csv(flux, file.path(OUT_DIR, "massbalance_flux.csv"), row.names = FALSE)
write.csv(do.call(rbind, leg_rows), file.path(OUT_DIR, "massbalance_legs.csv"), row.names = FALSE)

# Write a blank config template if none exists yet (PASS 1 only).
if (is.null(cfg) && length(info)) {
  wrote <- write_curtain_template(CURTAIN_CONFIG, do.call(rbind, info))
  if (wrote) message("\nTemplate written: ", CURTAIN_CONFIG,
                     "\n  -> fill leg_ids / blh_m / background_ppb per flight, then re-run for PASS 2.")
}
message("\nWrote massbalance_flux.csv (", if (is.null(flux)) 0 else nrow(flux), " flights).")
