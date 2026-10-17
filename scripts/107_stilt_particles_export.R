# 107_stilt_particles_export.R ------------------------------------------------
# Exports the HRRR-STILT particle clouds of script 64 for the Shiny explorer's
# particle animation (STILT tab) and for the GIF of script 108.
#
# For every receptor in <OUT_DIR>/stilt_exposure_receptors.csv (the 112 of the
# paper) the particle file stilt_runs/<id>/particles.rds (500 particles, 1-min
# steps, 10 h back) is thinned to
#   * N_PART particles (the same random subset per receptor, seed 1) at every
#     STEP_MIN minutes back, with position, height above ground, footprint
#     sensitivity and mixed-layer height, and
#   * the centroid of ALL 500 particles at every STEP_MIN minutes (mean position,
#     mean height, share of particles in the lower half of the mixed layer, i.e.
#     those that contribute to the surface footprint).
# Nothing is recomputed: the particles are those the paper convolved with the
# inventories (script 64 applied the plume-dilution correction before saving).
#
# Out: <OUT_DIR>/stilt_particles.csv.gz, <OUT_DIR>/stilt_centroids.csv
#      (script 86 copies both to shiny/data/)
# Run: Rscript scripts/107_stilt_particles_export.R      (base R, < 1 min)
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
N_PART <- as.integer(Sys.getenv("METHANE_STILT_EXPORT_NPART", "25")); STEP_MIN <- 15; VEGHT <- 0.5
RUN_DIR <- file.path(OUT_DIR, "stilt_runs")
R <- read.csv(file.path(OUT_DIR, "stilt_exposure_receptors.csv"), stringsAsFactors = FALSE)
if (!nrow(R)) stop("no receptors in stilt_exposure_receptors.csv")
steps <- c(1, seq(STEP_MIN, 600, by = STEP_MIN))            # minutes back: 1 (release) then every 15 min
parts <- list(); cents <- list(); missing <- character(0)
for (i in seq_len(nrow(R))) {
  r <- R[i, ]; f <- file.path(RUN_DIR, r$id, "particles.rds")
  if (!file.exists(f)) { missing <- c(missing, r$id); next }
  p <- readRDS(f); p <- p[is.finite(p$long) & is.finite(p$lati), ]
  p$min_back <- -p$time
  q <- p[p$min_back %in% steps, ]
  q$in_foot <- is.finite(q$mlht) & q$zagl < VEGHT * q$mlht
  set.seed(1); keep <- sort(sample(sort(unique(q$indx)), min(N_PART, length(unique(q$indx)))))
  s <- q[q$indx %in% keep, ]
  parts[[r$id]] <- data.frame(flight = r$flight, leg_id = r$leg_id, rec = r$rec, indx = s$indx, min_back = s$min_back,
    lon = round(s$long, 3), lat = round(s$lati, 3), zagl_m = round(s$zagl), mlht_m = round(s$mlht),
    foot = signif(s$foot, 2), in_foot = s$in_foot, stringsAsFactors = FALSE)
  ag <- aggregate(cbind(lon = q$long, lat = q$lati, zagl_m = q$zagl, in_foot = as.numeric(q$in_foot), foot = q$foot),
                  by = list(min_back = q$min_back), FUN = mean)
  cents[[r$id]] <- data.frame(flight = r$flight, leg_id = r$leg_id, rec = r$rec, min_back = ag$min_back,
    lon = round(ag$lon, 4), lat = round(ag$lat, 4), zagl_m = round(ag$zagl_m), frac_in_foot = round(ag$in_foot, 2),
    foot_mean = signif(ag$foot, 3), stringsAsFactors = FALSE)
}
if (length(missing)) message("no particle file for ", length(missing), " receptor(s): ", paste(head(missing, 5), collapse = ", "), if (length(missing) > 5) " ...")
P <- do.call(rbind, parts); C <- do.call(rbind, cents)
zz <- gzfile(file.path(OUT_DIR, "stilt_particles.csv.gz"), "w"); write.csv(P, zz, row.names = FALSE); close(zz)   # gzipped: read.csv() reads it directly
write.csv(C, file.path(OUT_DIR, "stilt_centroids.csv"), row.names = FALSE)
message(sprintf("stilt_particles.csv.gz: %s rows (%d receptors x %d particles x %d steps); stilt_centroids.csv: %s rows",
                format(nrow(P), big.mark = ","), length(parts), N_PART, length(steps), format(nrow(C), big.mark = ",")))
