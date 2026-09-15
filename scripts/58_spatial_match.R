# 58_spatial_match.R ----------------------------------------------------------
# HOW WELL DO THE INVENTORIES MATCH THE AIRCRAFT SPATIALLY?
#
# Fig 4B compares the aircraft-sampled fossil-vs-biogenic character of Denver's
# methane (0.02 deg cells, script 25) with the EPA gridded GHGI at 0.1 deg by eye.
# This script makes that comparison cell by cell and adds GRA2PES (4 km), which is
# both finer and, in v2.0beta, carries the waste and post-meter sectors the paper's
# source allocation turns on. For every aircraft cell with a local ethane:methane
# slope it samples each inventory's fossil fraction at that location and reports
#   * rank agreement (Spearman rho) between the aircraft and inventory fractions,
#   * class agreement (biogenic < 1/3, mixed, fossil > 2/3),
#   * the inventory's emission-weighted fossil fraction over the sampled cells vs
#     over the whole box (did the aircraft sample the fossil-rich or the
#     biogenic-rich part of the inventory?), and
#   * how much of each inventory's box methane lies inside the sampled cells.
# It also draws a 4-km source-context map: GRA2PES v2 methane density, the named
# facilities, Carbon Mapper point sources, the aircraft cells, and the direction of
# the DJB oil-and-gas centroid.
#
# Inputs (all under OUT_DIR unless noted):
#   fossil_signature_grid.csv            script 25 (aircraft cells; York slope, fossil_frac at the adopted endmember)
#   gra2pes_cells_<version>.csv          script 57 (one or more versions; skipped if absent)
#   carbonmapper_denver_sources.csv      script 43/44 (optional)
#   urban_legs.csv                       script 11 (leg centroids, for the map)
#   METHANE_GHGI                         EPA gridded GHGI NetCDF (terra); skipped if absent
#   ../biogenic_sources.csv              the three named facilities
# Out: <OUT_DIR>/spatial_match_cells.csv, spatial_match_summary.csv,
#      <OUT_DIR>/figures/FigS13_spatial_match.png
# Run: Rscript scripts/58_spatial_match.R
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
FIG <- file.path(OUT_DIR, "figures"); dir.create(FIG, showWarnings = FALSE, recursive = TRUE)
b <- URBAN_BOX
BETA0 <- SOURCE_C2H6_CH4                                   # adopted endmember (0.102)
BETA_LO <- 0.061; BETA_HI <- if (exists("SOURCE_C2H6_CH4_DENVER_DELIVERED")) SOURCE_C2H6_CH4_DENVER_DELIVERED else 0.110
DJB_CENTROID <- c(lat = 40.451, lon = -104.590)             # GHGI-weighted O&G centroid (script 53)
MW_CH4 <- 16.04; NA_ <- 6.02214e23
hav_km <- function(lo1, la1, lo2, la2) { R <- 6371.0088; p <- pi / 180
  a <- sin((la2 - la1) * p / 2)^2 + cos(la1 * p) * cos(la2 * p) * sin((lo2 - lo1) * p / 2)^2
  2 * R * asin(pmin(1, sqrt(a))) }
cls <- function(ff) ifelse(is.na(ff), NA, ifelse(ff < 1/3, "biogenic", ifelse(ff > 2/3, "fossil", "mixed")))

## ---- aircraft cells -----------------------------------------------------------
A <- read.csv(file.path(OUT_DIR, "fossil_signature_grid.csv"), stringsAsFactors = FALSE)
A$cell_id <- seq_len(nrow(A))
A$ff_adopted <- A$fossil_frac
A$ff_lo <- ifelse(is.finite(A$slope), pmax(0, pmin(1, A$slope / BETA_LO)), NA)   # basin lower case -> higher fossil
A$ff_hi <- ifelse(is.finite(A$slope), pmax(0, pmin(1, A$slope / BETA_HI)), NA)   # delivered gas -> lower fossil
D <- A[is.finite(A$ff_adopted), ]                          # cells with a local slope
message(sprintf("aircraft cells: %d sampled, %d with a local slope (median fossil fraction %.2f at %.3f)",
                nrow(A), nrow(D), median(D$ff_adopted), BETA0))

## ---- EPA gridded GHGI at 0.1 deg ---------------------------------------------
epa <- NULL
if (requireNamespace("terra", quietly = TRUE) && exists("GHGI_FILE") && file.exists(GHGI_FILE)) {
  r <- terra::rast(GHGI_FILE); nm <- names(r)
  fi <- grepl("Natural_Gas|Petroleum|PostMeter|Coal|Abandoned_Oil", nm)
  bi <- grepl("Landfill|Wastewater|Enteric|Manure|Composting|Rice|Field_Burning", nm)
  area <- r[["grid_cell_area"]]                                  # m2
  to_thr <- function(x) x * area * 1e4 * MW_CH4 / NA_ * 3600 / 1e6  # molec cm-2 s-1 -> t/hr per cell
  fos <- to_thr(sum(r[[which(fi)]])); bio <- to_thr(sum(r[[which(bi)]]))
  ffr <- fos / (fos + bio)
  ex  <- terra::ext(b$lon_w, b$lon_e, b$lat_s, b$lat_n)
  epa <- list(ff = terra::crop(ffr, ex, snap = "out"), fos = terra::crop(fos, ex, snap = "out"),
              bio = terra::crop(bio, ex, snap = "out"))
  pts <- terra::vect(D[, c("lon", "lat")], geom = c("lon", "lat"), crs = "EPSG:4326")
  D$epa_ff  <- terra::extract(ffr, pts)[, 2]
  D$epa_fos <- terra::extract(fos, pts)[, 2]; D$epa_bio <- terra::extract(bio, pts)[, 2]
  # box-wide and sampled-cell emission-weighted fractions (cells weighted by their in-box area share)
  epa$box_ff <- {                                                # area-weighted over the box (0.1-deg cells straddle its edges)
    m <- terra::crop(fos, ex, snap = "out"); n <- terra::crop(bio, ex, snap = "out")
    cov <- terra::rasterize(terra::as.polygons(ex, crs = "EPSG:4326"), m, cover = TRUE)
    cov[is.na(cov)] <- 0
    sum(terra::values(m * cov), na.rm = TRUE) / (sum(terra::values(m * cov), na.rm = TRUE) + sum(terra::values(n * cov), na.rm = TRUE)) }
  message(sprintf("EPA GHGI: box fossil fraction (fossil/[fossil+biogenic]) %.2f", epa$box_ff))
} else message("EPA GHGI file or terra not available; EPA comparison skipped.")

## ---- GRA2PES cells (script 57) -------------------------------------------------
gfiles <- list.files(OUT_DIR, pattern = "^gra2pes_cells_[^_]+\\.csv$", full.names = TRUE)
GR <- list()
for (f in gfiles) {
  g <- read.csv(f, stringsAsFactors = FALSE); v <- g$inventory_version[1]
  # nearest 4-km cell centre for every aircraft cell (aircraft cells are 0.02 deg, so
  # each lies inside exactly one 4-km cell; nearest centre <= 2.9 km)
  nn <- sapply(seq_len(nrow(D)), function(k) { d <- hav_km(D$lon[k], D$lat[k], g$lon, g$lat); which.min(d) })
  dist <- sapply(seq_len(nrow(D)), function(k) hav_km(D$lon[k], D$lat[k], g$lon[nn[k]], g$lat[nn[k]]))
  ok <- dist <= 3
  tag <- gsub("[^A-Za-z0-9]", "", v)
  D[[paste0("gra_", tag, "_ff")]]  <- ifelse(ok, g$fossil_frac[nn], NA)
  D[[paste0("gra_", tag, "_ch4")]] <- ifelse(ok, g$ch4_total_t_hr[nn], NA)
  D[[paste0("gra_", tag, "_waste")]] <- ifelse(ok, g$waste_t_hr[nn], NA)
  # share of the inventory's box methane inside the aircraft-sampled cells: assign each
  # 4-km cell to "sampled" if any aircraft cell (of the 106) falls in it
  nnA <- sapply(seq_len(nrow(A)), function(k) which.min(hav_km(A$lon[k], A$lat[k], g$lon, g$lat)))
  sampled <- seq_len(nrow(g)) %in% unique(nnA)
  GR[[v]] <- list(g = g, sampled = sampled, tag = tag,
                  box_ff = sum(g$fossil_t_hr) / (sum(g$fossil_t_hr) + sum(g$biogenic_t_hr)),
                  sampled_ff = sum(g$fossil_t_hr[sampled]) / (sum(g$fossil_t_hr[sampled]) + sum(g$biogenic_t_hr[sampled])),
                  share_sampled = sum(g$ch4_total_t_hr[sampled]) / sum(g$ch4_total_t_hr),
                  waste_share_sampled = if (sum(g$waste_t_hr) > 0) sum(g$waste_t_hr[sampled]) / sum(g$waste_t_hr) else NA)
  message(sprintf("GRA2PES %s: %d cells, box CH4 %.2f t/hr, box fossil fraction %.2f; sampled 4-km cells hold %.0f%% of box CH4",
                  v, nrow(g), sum(g$ch4_total_t_hr), GR[[v]]$box_ff, 100 * GR[[v]]$share_sampled))
}

## ---- cell-matched comparison --------------------------------------------------
score <- function(inv_ff, label, box_ff = NA, sampled_ff = NA, share = NA) {
  ok <- is.finite(inv_ff) & is.finite(D$ff_adopted)
  if (sum(ok) < 4) return(NULL)
  ct <- suppressWarnings(cor.test(D$ff_adopted[ok], inv_ff[ok], method = "spearman"))
  agree <- mean(cls(D$ff_adopted[ok]) == cls(inv_ff[ok]))
  data.frame(inventory = label, n_cells = sum(ok),
             aircraft_median_ff = round(median(D$ff_adopted[ok]), 3),
             aircraft_median_ff_lo_hi = sprintf("%.2f to %.2f", median(D$ff_hi[ok]), median(D$ff_lo[ok])),
             inventory_median_ff_at_cells = round(median(inv_ff[ok]), 3),
             inventory_ff_sampled_weighted = round(sampled_ff, 3),
             inventory_ff_box_weighted = round(box_ff, 3),
             share_of_inventory_ch4_in_sampled_cells = round(share, 3),
             spearman_rho = round(unname(ct$estimate), 2), p_value = signif(ct$p.value, 2),
             class_agreement = round(agree, 2),
             mean_abs_diff = round(mean(abs(D$ff_adopted[ok] - inv_ff[ok])), 3),
             cells_inventory_more_fossil = sum(inv_ff[ok] > D$ff_adopted[ok]),
             stringsAsFactors = FALSE)
}
S <- list()
if (!is.null(epa)) {
  # EPA share inside sampled cells: 0.1-deg cells containing any of the 106 aircraft cells
  cid <- terra::cellFromXY(epa$fos, as.matrix(A[, c("lon", "lat")]))
  tot <- terra::values(epa$fos) + terra::values(epa$bio); tot[is.na(tot)] <- 0
  inside <- seq_along(tot) %in% unique(cid)
  fv <- terra::values(epa$fos); bv <- terra::values(epa$bio)
  S[["EPA"]] <- score(D$epa_ff, "EPA gridded GHGI 2020 (0.1 deg)", epa$box_ff,
                      sum(fv[inside], na.rm = TRUE) / sum(fv[inside] + bv[inside], na.rm = TRUE),
                      sum(tot[inside]) / sum(tot))
}
for (v in names(GR)) S[[v]] <- score(D[[paste0("gra_", GR[[v]]$tag, "_ff")]], paste0("GRA2PES ", v, " (4 km)"),
                                     GR[[v]]$box_ff, GR[[v]]$sampled_ff, GR[[v]]$share_sampled)
SUM <- do.call(rbind, S)
write.csv(D, file.path(OUT_DIR, "spatial_match_cells.csv"), row.names = FALSE)
write.csv(SUM, file.path(OUT_DIR, "spatial_match_summary.csv"), row.names = FALSE)
cat("\n"); print(SUM, row.names = FALSE); cat("\n")

## ---- figure ---------------------------------------------------------------------
pal <- colorRampPalette(c("#1a9850", "#fee08b", "#d73027"))(100)
ffcol <- function(ff) ifelse(is.na(ff), NA, pal[pmax(1, pmin(100, ceiling(ff * 100)))])
LM <- tryCatch(read.csv(file.path(proj, "biogenic_sources.csv"), stringsAsFactors = FALSE), error = function(e) NULL)
CM <- tryCatch(read.csv(file.path(OUT_DIR, "carbonmapper_denver_sources.csv"), stringsAsFactors = FALSE), error = function(e) NULL)
LEGS <- tryCatch(read.csv(file.path(OUT_DIR, "urban_legs.csv"), stringsAsFactors = FALSE), error = function(e) NULL)
half <- 0.01
box_outline <- function() rect(b$lon_w, b$lat_s, b$lon_e, b$lat_n, border = "black", lwd = 1.5)
draw_cells <- function(col) rect(D$lon - half, D$lat - half, D$lon + half, D$lat + half, col = col, border = "grey20", lwd = 0.4)
overlay <- function() {
  if (!is.null(LEGS)) { u <- LEGS[LEGS$region == "urban", ]; points(u$lon, u$lat, pch = 4, cex = 0.5, col = "grey35") }
  if (!is.null(CM)) { cm <- CM[CM$gas == "CH4", ]
    points(cm$geom_lon, cm$geom_lat, pch = ifelse(cm$sector == "6A", 24, 25),
           bg = ifelse(cm$sector == "6A", "#1a9850", "#d73027"), col = "black",
           cex = 0.9 + 0.12 * sqrt(as.numeric(cm$plume_count))) }
  if (!is.null(LM)) points(LM$lon, LM$lat, pch = 3, cex = 1.6, lwd = 2, col = "black")
}
png(file.path(FIG, "FigS13_spatial_match.png"), width = 2200, height = 2000, res = 200)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 0, 0))
asp <- 1 / cos(mean(c(b$lat_s, b$lat_n)) * pi / 180)
xl <- c(b$lon_w - 0.02, b$lon_e + 0.02); yl <- c(b$lat_s - 0.02, b$lat_n + 0.02)
# A: source context at 4 km (v2 if present, else the first GRA2PES version)
vA <- if ("v2.0beta" %in% names(GR)) "v2.0beta" else names(GR)[1]
plot(NA, xlim = xl, ylim = yl, asp = asp, xlab = "Longitude", ylab = "Latitude",
     main = if (!is.na(vA)) paste0("A) Source context: GRA2PES ", vA, " methane (4 km)") else "A) Source context")
if (!is.na(vA)) { g <- GR[[vA]]$g; dens <- g$ch4_total_t_hr / 16 * 1000        # kg/hr/km2
  gp <- grey(1 - 0.85 * pmin(1, sqrt(dens / max(dens, na.rm = TRUE))))
  rect(g$lon - 0.0235, g$lat - 0.018, g$lon + 0.0235, g$lat + 0.018, col = gp, border = NA)
  wc <- g$waste_t_hr > 0.05 * max(g$waste_t_hr, na.rm = TRUE) & g$waste_t_hr > 0
  rect(g$lon[wc] - 0.0235, g$lat[wc] - 0.018, g$lon[wc] + 0.0235, g$lat[wc] + 0.018, border = "#1a9850", lwd = 1.2) }
box_outline(); draw_cells(ffcol(D$ff_adopted)); overlay()
arrows(b$lon_e - 0.12, b$lat_n - 0.03, b$lon_e - 0.12 + 0.09 * sign(DJB_CENTROID[["lon"]] - (b$lon_e - 0.12)),
       b$lat_n - 0.03 + 0.06, lwd = 2, length = 0.1); text(b$lon_e - 0.12, b$lat_n - 0.045, "to DJB O&G centroid", cex = 0.7)
legend("bottomleft", bty = "n", cex = 0.72,
       legend = c("aircraft cell (fossil fraction, green->red)", "GRA2PES waste cell", "Carbon Mapper landfill / O&G source",
                  "named facility", "urban leg centroid"),
       pch = c(22, 22, 24, 3, 4), pt.bg = c("#fee08b", NA, "#1a9850", NA, NA), col = c("grey20", "#1a9850", "black", "black", "grey35"),
       pt.cex = c(1.4, 1.4, 1, 1.4, 0.7), pt.lwd = c(0.5, 1.5, 1, 2, 1))
# B: EPA fossil fraction
plot(NA, xlim = xl, ylim = yl, asp = asp, xlab = "Longitude", ylab = "Latitude", main = "B) EPA gridded GHGI fossil fraction (0.1 deg)")
if (!is.null(epa)) { m <- epa$ff; xy <- terra::xyFromCell(m, seq_len(terra::ncell(m))); v <- terra::values(m)[, 1]
  rect(xy[, 1] - 0.05, xy[, 2] - 0.05, xy[, 1] + 0.05, xy[, 2] + 0.05, col = ffcol(v), border = "white") }
box_outline(); draw_cells(ffcol(D$ff_adopted)); overlay()
# C: GRA2PES fossil fraction
plot(NA, xlim = xl, ylim = yl, asp = asp, xlab = "Longitude", ylab = "Latitude",
     main = if (!is.na(vA)) paste0("C) GRA2PES ", vA, " fossil fraction (4 km)") else "C) GRA2PES fossil fraction")
if (!is.na(vA)) { g <- GR[[vA]]$g
  rect(g$lon - 0.0235, g$lat - 0.018, g$lon + 0.0235, g$lat + 0.018, col = ffcol(g$fossil_frac), border = "white") }
box_outline(); draw_cells(ffcol(D$ff_adopted)); overlay()
# D: cell-matched scatter
plot(NA, xlim = c(0, 1), ylim = c(0, 1), xlab = sprintf("aircraft fossil fraction in cell (endmember %.3f)", BETA0),
     ylab = "inventory fossil fraction at the same location", main = "D) Cell-by-cell comparison")
abline(0, 1, lty = 2, col = "grey50"); abline(h = 0.5, v = 0.5, lty = 3, col = "grey80")
syms <- c(21, 24, 22, 23); k <- 0; lab <- character(0)
if (!is.null(epa)) { k <- k + 1; points(D$ff_adopted, D$epa_ff, pch = syms[k], bg = "#4575b4", cex = 1.1); lab <- c(lab, "EPA GHGI 0.1 deg") }
for (v in names(GR)) { k <- k + 1; points(D$ff_adopted, D[[paste0("gra_", GR[[v]]$tag, "_ff")]], pch = syms[k],
                                          bg = c("#fdae61", "#abd9e9", "#999999")[k - !is.null(epa)], cex = 1.1); lab <- c(lab, paste("GRA2PES", v)) }
legend("topleft", bty = "n", pch = syms[seq_len(k)], pt.bg = c("#4575b4", "#fdae61", "#abd9e9", "#999999")[seq_len(k)], legend = lab, cex = 0.8)
if (!is.null(SUM)) legend("bottomright", bty = "n", cex = 0.72,
                          legend = sprintf("%s: rho %.2f, class agreement %.0f%%", sub(" \\(.*", "", SUM$inventory), SUM$spearman_rho, 100 * SUM$class_agreement))
dev.off()
message("wrote spatial_match_cells.csv, spatial_match_summary.csv, figures/FigS13_spatial_match.png")
