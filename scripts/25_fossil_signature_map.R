# 25_fossil_signature_map.R ---------------------------------------------------
# A qualitative SPATIAL map of the observed fossil-vs-biogenic character of the
# Denver methane, straight from the aircraft. This is the spatial companion to
# the aggregate budget split in script 23. Script 23 answers "what fraction of
# the whole urban budget is fossil"; this script answers "WHERE does the air
# read fossil, and where does it read biogenic".
#
# Method (all observed, no transport model):
#   * pool every in-plume aircraft sample from all flights (CH4 enhancement above
#     a rolling background, ethane likewise);
#   * lay a coarse grid over the metro box;
#   * in each cell with enough plume points, estimate the local ethane:methane
#     slope (robust median of C2H6_enh / CH4_enh), and convert it to a fossil
#     fraction with the same endmember used everywhere else (SOURCE_C2H6_CH4);
#   * colour each cell from biogenic (low ethane) to fossil (high ethane).
# Cells are only drawn where the aircraft actually sampled, so this is a map of
# the observed signature along the flight paths, NOT a gridded emission
# inversion, which this coverage cannot support. Read it qualitatively.
#
# If terra and the EPA gridded GHGI file are present, a second panel maps the
# bottom-up fossil fraction per grid cell for the same domain, so the observed
# pattern can be compared with where the inventory places fossil vs biogenic
# methane. That panel is skipped cleanly when terra or the file is absent.
#
# Out: <OUT_DIR>/figures/Fig6_fossil_signature_map.png
#      <OUT_DIR>/fossil_signature_grid.csv
# Run: Rscript scripts/25_fossil_signature_map.R
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
source(file.path(proj, "R", "read_icartt.R")); source(file.path(proj, "R", "paths.R"))
source(file.path(proj, "R", "ratios.R")); source(file.path(proj, "R", "enhancements.R"))

## grid + threshold parameters
CELL_DEG    <- 0.02     # ~1.7 x 2.2 km cells over Denver
ENH_MIN_PPB <- 20       # CH4 enhancement to count a sample as in-plume
NCELL_MIN   <- 12       # minimum plume points in a cell to colour it
AGL_MAX_M   <- 1500     # keep boundary-layer samples

# Point sources to annotate. Read from biogenic_sources.csv (name,lat,lon,type,source)
# so the overlay is auditable and extendable: add a row per verified facility.
# Marker by type: landfill = square, wastewater = down-triangle, refinery = up-triangle.
PCH_BY_TYPE <- c(landfill = 22, wastewater = 25, refinery = 24, other = 21)
BG_BY_TYPE  <- c(landfill = "#000000", wastewater = "#2980b9", refinery = "#c0392b", other = "white")
srcfile <- file.path(proj, "biogenic_sources.csv")
if (file.exists(srcfile)) {
  LANDMARKS <- read.csv(srcfile, stringsAsFactors = FALSE)
  ty <- ifelse(LANDMARKS$type %in% names(PCH_BY_TYPE), LANDMARKS$type, "other")
  LANDMARKS$pch <- unname(PCH_BY_TYPE[ty]); LANDMARKS$bg <- unname(BG_BY_TYPE[ty])
} else {
  LANDMARKS <- data.frame(name = "Suncor refinery", lat = 39.802, lon = -104.939,
                          type = "refinery", pch = 24, bg = "#c0392b", stringsAsFactors = FALSE)
}

## ---- pool in-plume samples from every flight -------------------------------
pts <- list()
for (p in list_flights(DATA_DIR)) {
  if (!grepl("ARL-Suite", p)) next
  ic <- tryCatch(read_icartt(p), error = function(e) NULL); if (is.null(ic)) next
  d <- ic$data
  if (!all(c("Latitude","Longitude","CH4_ppb","C2H6_ppb") %in% names(d))) next
  d <- add_enhancements(d, "CH4_ppb"); d <- add_enhancements(d, "C2H6_ppb")
  agl <- if ("ALTAGL" %in% names(d)) d$ALTAGL else rep(NA_real_, nrow(d))
  k <- is.finite(d$CH4_ppb_enh) & d$CH4_ppb_enh > ENH_MIN_PPB &
       is.finite(d$C2H6_ppb_enh) & is.finite(d$Latitude) & is.finite(d$Longitude) &
       (is.na(agl) | agl < AGL_MAX_M)
  if (any(k)) pts[[p]] <- data.frame(lat = d$Latitude[k], lon = d$Longitude[k],
                                     xh = d$CH4_ppb_enh[k], xe = d$C2H6_ppb_enh[k])
}
P <- do.call(rbind, pts)
if (is.null(P) || nrow(P) < NCELL_MIN) { message("Too few in-plume samples for a map."); quit(save = "no") }

## ---- bin to a grid and estimate a local fossil fraction per cell -----------
b <- if (exists("URBAN_BOX")) URBAN_BOX else
     list(lat_s = min(P$lat), lat_n = max(P$lat), lon_w = min(P$lon), lon_e = max(P$lon))
P <- P[P$lat >= b$lat_s & P$lat <= b$lat_n & P$lon >= b$lon_w & P$lon <= b$lon_e, ]
P$ci <- floor((P$lon - b$lon_w) / CELL_DEG); P$ri <- floor((P$lat - b$lat_s) / CELL_DEG)
key <- paste(P$ri, P$ci)
cells <- lapply(split(P, key), function(g) {
  if (nrow(g) < NCELL_MIN) return(NULL)
  slope <- stats::median(g$xe / g$xh, na.rm = TRUE)        # robust local C2H6:CH4
  ff <- max(0, min(1, fossil_fraction(slope, SOURCE_C2H6_CH4)))
  data.frame(lon = b$lon_w + (g$ci[1] + 0.5) * CELL_DEG,
             lat = b$lat_s + (g$ri[1] + 0.5) * CELL_DEG,
             n = nrow(g), slope = round(slope, 3), fossil_frac = round(ff, 2))
})
grid <- do.call(rbind, cells)
if (is.null(grid)) { message("No cell reached the minimum sample count."); quit(save = "no") }
write.csv(grid, file.path(OUT_DIR, "fossil_signature_grid.csv"), row.names = FALSE)

## ---- optional EPA gridded GHGI fossil-fraction panel -----------------------
have_inv <- requireNamespace("terra", quietly = TRUE) && exists("GHGI_FILE") && file.exists(GHGI_FILE)

## optional Vulcan fossil-CO2 grayscale basemap for the observed (left) panel.
## The offline emissions raster traces the urban road network and built-up area,
## so it gives geographic context with no map tiles. Falls back to a plain
## background if terra or the Vulcan file is unavailable.
have_base <- requireNamespace("terra", quietly = TRUE) && exists("VULCAN_FILE") && file.exists(VULCAN_FILE)
vbase <- if (!have_base) NULL else tryCatch({
  r  <- terra::rast(VULCAN_FILE)
  ep <- terra::project(terra::as.polygons(terra::ext(b$lon_w, b$lon_e, b$lat_s, b$lat_n),
                                          crs = "EPSG:4326"), terra::crs(r))
  r  <- terra::crop(r, ep); r <- terra::project(r, "EPSG:4326")
  rl <- log10(r); rl[rl < log10(20)] <- NA; rl        # log stretch; hide near-zero
}, error = function(e) NULL)

## ---- plot ------------------------------------------------------------------
pal <- colorRampPalette(c("#1a9850", "#f7f7b0", "#b2182b"))(100)  # green->fossil red
col_of <- function(f) pal[pmax(1, pmin(100, round(f * 99) + 1))]
FIG <- file.path(OUT_DIR, "figures"); dir.create(FIG, showWarnings = FALSE, recursive = TRUE)
png(file.path(FIG, "Fig6_fossil_signature_map.png"),
    width = if (have_inv) 1700 else 1000, height = 900, res = 150)
if (have_inv) par(mfrow = c(1, 2), mar = c(4, 4, 3, 1)) else par(mar = c(4, 4, 3, 1))

if (!is.null(vbase)) {
  terra::plot(vbase, col = rev(grey.colors(64, start = 0.15, end = 0.97)), legend = FALSE,
              axes = TRUE, xlim = c(b$lon_w, b$lon_e), ylim = c(b$lat_s, b$lat_n),
              mar = c(4, 4, 3, 1), main = "Observed methane character (aircraft)")
} else {
  plot(grid$lon, grid$lat, type = "n", xlab = "Longitude", ylab = "Latitude",
       main = "Observed methane character (aircraft)", asp = 1/cos(39.8*pi/180),
       xlim = c(b$lon_w, b$lon_e), ylim = c(b$lat_s, b$lat_n))
}
# cells are semi-transparent over a basemap so the streets show through, opaque otherwise
cellcol <- if (!is.null(vbase)) adjustcolor(col_of(grid$fossil_frac), alpha.f = 0.72) else col_of(grid$fossil_frac)
rect(grid$lon - CELL_DEG/2, grid$lat - CELL_DEG/2, grid$lon + CELL_DEG/2, grid$lat + CELL_DEG/2,
     col = cellcol, border = NA)
points(LANDMARKS$lon, LANDMARKS$lat, pch = LANDMARKS$pch, bg = LANDMARKS$bg, col = "black", cex = 1.5, lwd = 1.5)
# label to the LEFT of any facility in the right 40% of the panel, else to the
# right, so long names do not run off the panel edge.
lab_pos <- ifelse(LANDMARKS$lon > (b$lon_w + 0.60 * (b$lon_e - b$lon_w)), 2, 4)
text(LANDMARKS$lon, LANDMARKS$lat, LANDMARKS$name, pos = lab_pos, cex = 0.55, font = 2)
legend("bottomleft", bty = "n", cex = 0.72,
       legend = c("biogenic (low ethane)", "mixed", "fossil (high ethane)"),
       fill = pal[c(5, 50, 95)])
legend("topright", bty = "n", cex = 0.72, pt.cex = 1.3, title = "facilities",
       legend = c("landfill", "wastewater plant", "refinery"),
       pch = c(22, 25, 24), pt.bg = c("#000000", "#2980b9", "#c0392b"))
# colour bar
xr <- b$lon_w + (b$lon_e - b$lon_w) * c(0.72, 0.97); yr <- b$lat_s + (b$lat_n - b$lat_s) * 0.06
for (i in 1:100) rect(xr[1] + (xr[2]-xr[1])*(i-1)/100, yr, xr[1] + (xr[2]-xr[1])*i/100, yr + (b$lat_n-b$lat_s)*0.02,
                      col = pal[i], border = NA)
text(mean(xr), yr + (b$lat_n-b$lat_s)*0.035, "fossil fraction  0 -> 1", cex = 0.6)

if (have_inv) {
  r <- terra::rast(GHGI_FILE)
  nm <- names(r)
  pick <- function(pat) which(grepl(pat, nm, ignore.case = TRUE))
  fi <- pick("1B2|natural_gas|petroleum|1B1|fossil"); bi <- pick("5A|5D|landfill|wastewater|3A|manure|enteric")
  fr <- if (length(fi)) sum(r[[fi]]) else NULL; br <- if (length(bi)) sum(r[[bi]]) else NULL
  if (!is.null(fr) && !is.null(br)) {
    ff <- fr / (fr + br)
    ff <- terra::crop(ff, terra::ext(b$lon_w, b$lon_e, b$lat_s, b$lat_n))
    terra::plot(ff, col = pal, range = c(0, 1),
                main = "EPA gridded GHGI fossil fraction (bottom-up)",
                xlab = "Longitude", ylab = "Latitude")
    points(LANDMARKS$lon, LANDMARKS$lat, pch = LANDMARKS$pch, bg = LANDMARKS$bg, col = "black", cex = 1.5, lwd = 1.5)
  } else { plot.new(); title("EPA GHGI panel: expected sector names not found") }
}
dev.off()
message(sprintf("Wrote Fig6_fossil_signature_map.png (%d coloured cells) and fossil_signature_grid.csv%s",
                nrow(grid), if (have_inv) " with EPA panel" else " (EPA panel skipped: no terra/GHGI)"))
message("Most-fossil cells (top of the ethane signature):")
print(utils::head(grid[order(-grid$fossil_frac), ], 6), row.names = FALSE)
