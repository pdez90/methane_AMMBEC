# 54_carbonmapper_encounters.R -------------------------------------------------
# Carbon Mapper gives independently located, sector-typed methane point sources
# inside the analysis box (two landfills, three oil-and-gas sites; script 43/44).
# The aircraft gives winds and an ethane:methane ratio.  Put together, every
# aircraft sample can be asked: was it DOWNWIND of one of those sources when it
# saw methane?  A sample is linked to a source when
#     distance <= D_MAX_KM, the sample lies within ALIGN_DEG of the source's
#     downwind line (aircraft wind at the sample), and the sample is in the
#     boundary layer (ALTAGL < AGL_MAX_M).
# Linked, gated (dCH4 > 20 ppb) samples are pooled per flight x leg x source and
# fitted with the York slope wherever >= MIN_PTS remain.  Landfill-linked
# encounters should be ethane-poor; oil-and-gas-linked ones ethane-enriched.
# Samples linked to sources of BOTH classes at once are flagged "mixed".
# It REPORTS; nothing published changes.
#
#   Rscript scripts/54_carbonmapper_encounters.R
#   Out: <OUT_DIR>/carbonmapper_encounters.csv, carbonmapper_encounter_summary.csv
#        <OUT_DIR>/figures/FigS12_carbonmapper_encounters.png
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
source(file.path(proj, "R", "read_icartt.R")); source(file.path(proj, "R", "paths.R"))
source(file.path(proj, "R", "massbalance.R")); source(file.path(proj, "R", "enhancements.R"))
source(file.path(proj, "R", "ratios.R")); source(file.path(proj, "R", "qc.R"))

MIN_ENH <- 20; MIN_PTS <- 10; AGL_MAX_M <- 1500
# strict linking is the pre-specified test; relaxed is reported as a sensitivity
CRITERIA <- list(strict = c(d_max_km = 12, align_deg = 25), relaxed = c(d_max_km = 20, align_deg = 35))
FIG <- file.path(OUT_DIR, "figures"); dir.create(FIG, showWarnings = FALSE, recursive = TRUE)

hav_km <- function(lat1, lon1, lat2, lon2) {
  p <- pi / 180; R <- 6371.0088
  a <- sin((lat2 - lat1) * p / 2)^2 + cos(lat1 * p) * cos(lat2 * p) * sin((lon2 - lon1) * p / 2)^2
  2 * R * asin(pmin(1, sqrt(a)))
}
bearing <- function(lat1, lon1, lat2, lon2) {
  p <- pi / 180; dlon <- (lon2 - lon1) * p
  y <- sin(dlon) * cos(lat2 * p); x <- cos(lat1 * p) * sin(lat2 * p) - sin(lat1 * p) * cos(lat2 * p) * cos(dlon)
  (atan2(y, x) / p + 360) %% 360
}
ang_diff <- function(a, b) abs((a - b + 180) %% 360 - 180)

## ---- Carbon Mapper sources in the box ------------------------------------------
srcf <- c(file.path(OUT_DIR, "carbonmapper_denver_sources.csv"), file.path(proj, "results", "carbonmapper_denver_sources.csv"))
srcf <- srcf[file.exists(srcf)][1]
if (is.na(srcf)) stop("carbonmapper_denver_sources.csv not found; run scripts/43_carbonmapper_plumes.sh")
src <- read.csv(srcf, stringsAsFactors = FALSE)
src <- src[src$gas == "CH4", ]
src$class <- ifelse(grepl("^6", src$sector), "landfill", ifelse(grepl("^1B2", src$sector), "oil_and_gas", src$sector))
src$name <- sprintf("%s %.3fN %.3fW", src$class, src$geom_lat, -src$geom_lon)
# orientation labels from the landmarks used by scripts 44/49 (approximate; confirm on a map)
lm <- data.frame(name = c("Tower Rd landfill", "DADS landfill"), lat = c(39.852, 39.662), lon = c(-104.758, -104.690))
for (i in seq_len(nrow(src))) { d <- hav_km(src$geom_lat[i], src$geom_lon[i], lm$lat, lm$lon)
  if (min(d) < 2) src$name[i] <- lm$name[which.min(d)] }
cat("Carbon Mapper CH4 sources:\n"); print(src[, c("name", "sector", "plume_count", "emission_auto", "persistence")], row.names = FALSE)

## ---- aircraft samples on urban legs ---------------------------------------------
.flights <- list_flights(DATA_DIR)
if (!length(.flights)) stop("no flight files under METHANE_DATA_DIR = ", DATA_DIR)
flights <- list()
for (p in .flights) {
  if (!grepl("ARL-Suite", p)) next
  ic <- tryCatch(read_icartt(p), error = function(e) NULL)
  if (is.null(ic) || !all(c("CH4_ppb", "C2H6_ppb") %in% names(ic$data))) next
  d <- detect_level_legs(ic$data); if (!any(d$leg_id > 0)) next
  lg <- tag_region(leg_metrics(d)); urb <- lg$leg_id[lg$region == "urban"]
  if (length(urb) < 1) next
  du <- d[d$leg_id %in% urb, ]
  du <- add_enhancements(du, "CH4_ppb"); du <- add_enhancements(du, "C2H6_ppb")
  fl <- sub("AMMBEC-ARL-Suite_TwinOtter_", "", sub("\\.ict$", "", basename(p)))
  ok <- is.finite(du$WD) & is.finite(du$WS) & is.finite(du$ALTAGL) & du$ALTAGL < AGL_MAX_M &
        is.finite(du$CH4_ppb_enh) & is.finite(du$C2H6_ppb_enh)
  du <- du[ok, ]; if (!nrow(du)) next
  flights[[fl]] <- du
}
if (!length(flights)) stop("no urban-leg samples with winds")

enc <- list(); ctrl <- list()
for (crit in names(CRITERIA)) for (fl in names(flights)) {
  du <- flights[[fl]]; D_MAX_KM <- CRITERIA[[crit]]["d_max_km"]; ALIGN_DEG <- CRITERIA[[crit]]["align_deg"]
  # link every sample to every source
  link <- matrix(FALSE, nrow(du), nrow(src)); dist <- link * 0; mis <- dist
  for (j in seq_len(nrow(src))) {
    dist[, j] <- hav_km(du$Latitude, du$Longitude, src$geom_lat[j], src$geom_lon[j])
    b <- bearing(src$geom_lat[j], src$geom_lon[j], du$Latitude, du$Longitude)   # source -> sample
    mis[, j] <- ang_diff(b, (du$WD + 180) %% 360)                               # vs the downwind line
    link[, j] <- dist[, j] <= D_MAX_KM & mis[, j] <= ALIGN_DEG
  }
  cls <- src$class
  n_lf <- rowSums(link[, cls == "landfill", drop = FALSE]); n_og <- rowSums(link[, cls == "oil_and_gas", drop = FALSE])
  mixed <- n_lf > 0 & n_og > 0
  gated <- du$CH4_ppb_enh > MIN_ENH
  for (j in seq_len(nrow(src))) for (g in unique(du$leg_id[link[, j] & gated])) {
    k <- link[, j] & gated & du$leg_id == g
    if (sum(k) < MIN_PTS) next
    x <- du$CH4_ppb_enh[k]; y <- du$C2H6_ppb_enh[k]
    yk <- york_slope(x, y, 1, 0.2)
    enc[[length(enc) + 1]] <- data.frame(criterion = crit, flight = fl, leg_id = g, source = src$name[j], class = cls[j],
      n = sum(k), n_mixed = sum(mixed[k]), dist_km = round(mean(dist[k, j]), 1), mismatch_deg = round(mean(mis[k, j])),
      transport_min = round(mean(dist[k, j] * 1000 / pmax(0.5, du$WS[k])) / 60),
      agl_m = round(mean(du$ALTAGL[k])), wd_deg = round(mean(du$WD[k])), dch4_mean_ppb = round(mean(x), 1),
      dch4_max_ppb = round(max(x), 1), slope = round(yk$slope, 4), r = round(suppressWarnings(cor(x, y)), 2),
      fossil_pct = round(pmin(100, 100 * yk$slope / SOURCE_C2H6_CH4)), stringsAsFactors = FALSE)
  }
  # control: gated samples in the boundary layer linked to NO Carbon Mapper source
  k0 <- gated & rowSums(link) == 0
  if (sum(k0) >= MIN_PTS) {
    yk <- york_slope(du$CH4_ppb_enh[k0], du$C2H6_ppb_enh[k0], 1, 0.2)
    ctrl[[length(ctrl) + 1]] <- data.frame(criterion = crit, flight = fl, n = sum(k0), slope = round(yk$slope, 4),
      r = round(suppressWarnings(cor(du$CH4_ppb_enh[k0], du$C2H6_ppb_enh[k0])), 2), stringsAsFactors = FALSE)
  }
}
E <- if (length(enc)) do.call(rbind, enc) else NULL
C <- if (length(ctrl)) do.call(rbind, ctrl) else NULL
if (is.null(E)) stop("no aircraft encounters linked to any Carbon Mapper source")
E <- E[order(E$criterion, E$class, E$source, E$flight, E$leg_id), ]
E$ambiguous <- E$n_mixed / E$n > 0.5
write.csv(E, file.path(OUT_DIR, "carbonmapper_encounters.csv"), row.names = FALSE)

summ <- do.call(rbind, lapply(split(E[!E$ambiguous, ], list(E$criterion[!E$ambiguous], E$class[!E$ambiguous]), drop = TRUE), function(s) data.frame(
  criterion = s$criterion[1], class = s$class[1], n_encounters = nrow(s), n_flights = length(unique(s$flight)), n_samples = sum(s$n),
  median_slope = round(median(s$slope), 4), min_slope = round(min(s$slope), 4), max_slope = round(max(s$slope), 4),
  median_r = round(median(s$r), 2), median_fossil_pct = round(median(s$fossil_pct)),
  n_ethane_poor = sum(s$slope < 0.01), n_ethane_rich = sum(s$slope > 0.03))))
if (!is.null(C)) for (crit in unique(C$criterion)) { Cc <- C[C$criterion == crit, ]
  summ <- rbind(summ, data.frame(criterion = crit, class = "no linked source (control)", n_encounters = nrow(Cc),
  n_flights = nrow(Cc), n_samples = sum(Cc$n), median_slope = round(median(Cc$slope), 4), min_slope = round(min(Cc$slope), 4),
  max_slope = round(max(Cc$slope), 4), median_r = round(median(Cc$r), 2), median_fossil_pct = round(median(pmin(100, 100 * Cc$slope / SOURCE_C2H6_CH4))),
  n_ethane_poor = sum(Cc$slope < 0.01), n_ethane_rich = sum(Cc$slope > 0.03))) }
write.csv(summ, file.path(OUT_DIR, "carbonmapper_encounter_summary.csv"), row.names = FALSE)

for (crit in names(CRITERIA)) {
  cat(sprintf("\n[%s] encounters: boundary layer, downwind within %g deg and %g km of the source\n", crit,
              CRITERIA[[crit]]["align_deg"], CRITERIA[[crit]]["d_max_km"]))
  print(E[E$criterion == crit, c("flight", "leg_id", "source", "n", "n_mixed", "dist_km", "mismatch_deg", "transport_min", "agl_m", "wd_deg",
            "dch4_mean_ppb", "dch4_max_ppb", "slope", "r", "fossil_pct")], row.names = FALSE)
}
cat("\nby criterion and source class (encounters linked to both classes excluded):\n"); print(summ, row.names = FALSE)
if (!is.null(C)) { cat("\ncontrol (gated boundary-layer samples linked to no source):\n"); print(C, row.names = FALSE) }

## ---- figure ----------------------------------------------------------------------
png(file.path(FIG, "FigS12_carbonmapper_encounters.png"), 1200, 720, res = 150)
par(mar = c(4.4, 4.6, 2, 1))
set.seed(42)
Es <- E; cl <- factor(Es$class, levels = c("landfill", "oil_and_gas"))
xs <- as.numeric(cl) + ifelse(Es$criterion == "strict", -0.12, 0.12) + runif(nrow(Es), -0.06, 0.06)
plot(xs, Es$slope, xlim = c(0.5, 2.5), pch = ifelse(Es$ambiguous, 4, ifelse(Es$criterion == "strict", 21, 22)),
     bg = ifelse(cl == "landfill", "#1a9850", "#b2182b"),
     cex = 0.7 + 1.5 * sqrt(Es$n / max(Es$n)), xaxt = "n", xlab = "", ylab = "encounter ethane:methane slope (mol/mol)", las = 1)
axis(1, at = 1:2, labels = c("downwind of Carbon Mapper landfill", "downwind of Carbon Mapper oil-and-gas site"))
abline(h = 0, col = "gray70"); abline(h = SOURCE_C2H6_CH4, lty = 3, col = "gray40"); abline(h = 0.0813, lty = 3, col = "gray40")
text(2.5, SOURCE_C2H6_CH4, "0.102", pos = 2, cex = 0.7, col = "gray40"); text(2.5, 0.0813, "0.0813", pos = 2, cex = 0.7, col = "gray40")
if (!is.null(C)) { cm <- median(C$slope[C$criterion == "strict"]); abline(h = cm, lty = 2, col = "gray30"); text(0.5, cm, "control median", pos = 4, cex = 0.7, col = "gray30") }
legend("topleft", bty = "n", cex = 0.75, pch = c(21, 22, 4), legend = c("strict linking (12 km, 25 deg)", "relaxed linking (20 km, 35 deg)", "linked to both classes (ambiguous)"))
title("Aircraft ethane:methane slope on samples downwind of Carbon Mapper sources", cex.main = 0.85)
dev.off()
cat("\nwrote carbonmapper_encounters.csv, carbonmapper_encounter_summary.csv, FigS12_carbonmapper_encounters.png\n")
