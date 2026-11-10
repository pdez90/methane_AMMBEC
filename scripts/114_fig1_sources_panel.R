# 114_fig1_sources_panel.R ------------------------------------------------------
# Figure 1, panel (b): the Denver-metro analysis box enlarged, with the main point
# and area methane sources named in the paper, so a reader can relate the flight
# transects of panel (a) to the sources discussed in sections 2.5, 4.1 and 4.3.
#
# Shown: the two landfills and three oil-and-gas sites of the Carbon Mapper
# catalogue (carbonmapper_denver_sources.csv, script 43), the Cherokee gas-fired
# power station (the catalogue's CO2 source), the Suncor refinery and the Metro
# Water Recovery (Robert W. Hite) wastewater plant, the GRA2PES v2.0beta waste
# hotspot cells (v2_pointsource_hotspots.csv, script 59), all flight tracks, and
# the urban transect segments coloured as in panel (a).
#
# Out: <OUT_DIR>/figures/Fig1b_box_sources.png      panel (b) alone
#      <OUT_DIR>/figures/Fig1_study_map_sources.png (a) script 20's map | (b)
# Run: Rscript scripts/114_fig1_sources_panel.R     (after scripts 11, 20, 43, 59)
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
for (f in c("read_icartt", "paths")) source(file.path(proj, "R", paste0(f, ".R")))
FIGD <- file.path(OUT_DIR, "figures"); b <- URBAN_BOX
ext <- c(b$lon_w - 0.07, b$lon_e + 0.07, b$lat_s - 0.05, b$lat_n + 0.07)

# named sources. Landfill, oil-and-gas and power-station positions are the Carbon
# Mapper source positions; the wastewater plant is the EPA Risk Management Plan
# facility position (6450 York St) and the refinery an approximate centre (orientation only).
SRC <- data.frame(
  name = c("Tower Road landfill", "DADS landfill", "Metro Water Recovery\n(wastewater)", "Suncor refinery", "Cherokee power station"),
  type = c("landfill", "landfill", "wastewater", "refinery", "power"),
  lat  = c(39.8517, 39.6622, 39.8111, 39.802, 39.8074),
  lon  = c(-104.7575, -104.6904, -104.9536, -104.939, -104.9639),
  lab_lat = c(39.872, 39.682, 39.772, 39.792, 39.842),      # label positions, clear of the markers
  lab_lon = c(-104.7575, -104.6904, -105.03, -104.86, -105.03), stringsAsFactors = FALSE)
cm <- file.path(OUT_DIR, "carbonmapper_denver_sources.csv")
OG <- if (file.exists(cm)) { x <- read.csv(cm, stringsAsFactors = FALSE); x[x$gas == "CH4" & x$sector == "1B2", c("geom_lat", "geom_lon")] } else NULL
hs <- file.path(OUT_DIR, "v2_pointsource_hotspots.csv")
HS <- if (file.exists(hs)) read.csv(hs, stringsAsFactors = FALSE) else NULL
PCH <- c(landfill = 22, wastewater = 25, refinery = 24, power = 23)
BG  <- c(landfill = "#111111", wastewater = "#1f78b4", refinery = "#d7301f", power = "#7b3294")

halo <- function(x, y, lab, col = "gray10", cex = 0.62, pos = 4, font = 1, r = 0.0016) {
  for (dx in c(-r, 0, r)) for (dy in c(-r, 0, r)) if (dx != 0 || dy != 0) text(x + dx, y + dy, lab, col = "white", cex = cex, pos = pos, font = font)
  text(x, y, lab, col = col, cex = cex, pos = pos, font = font)
}
pal <- function(v, vmax) { cr <- colorRamp(c("#0d0887", "#7e03a8", "#cc4778", "#f89540", "#f0f921")); z <- pmin(pmax(v, 0), vmax) / vmax; z[!is.finite(z)] <- 0; rgb(cr(z), maxColorValue = 255) }

# basemap: the street map already stored for the explorer (script 109, OpenStreetMap,
# lightened), which shows where the sources sit in the city; the Vulcan raster of
# panel (a) is the fallback when that image is absent.
bx <- file.path(proj, "shiny", "data", "basemap_extents.csv"); bp <- file.path(proj, "shiny", "data", "basemap_metro.png"); CREDIT <- ""
png(file.path(FIGD, "Fig1b_box_sources.png"), width = 1280, height = 1560, res = 200)
if (file.exists(bx) && file.exists(bp) && requireNamespace("png", quietly = TRUE)) {
  be <- read.csv(bx, stringsAsFactors = FALSE); be <- be[be$name == "metro", ]; CREDIT <- "\u00a9 OpenStreetMap contributors"
  ext <- c(max(ext[1], be$xmin), min(ext[2], be$xmax), max(ext[3], be$ymin), min(ext[4], be$ymax))
  par(mar = c(2.4, 2.4, 9.2, 2.6) + c(1.0, 0.4, 0, 0))
  plot(NA, xlim = ext[1:2], ylim = ext[3:4], xaxs = "i", yaxs = "i", asp = 1 / cos(mean(ext[3:4]) * pi / 180), xlab = "", ylab = "", cex.axis = 0.62, mgp = c(2, 0.5, 0), tcl = -0.3)
  rasterImage(png::readPNG(bp), be$xmin, be$ymin, be$xmax, be$ymax); box()
} else {
  suppressMessages(library(terra)); r <- rast(VULCAN_FILE)
  e <- project(as.polygons(ext(ext[1], ext[2], ext[3], ext[4]), crs = "EPSG:4326"), crs(r))
  rl <- log10(project(crop(r, e), "EPSG:4326")); rl[rl < log10(20)] <- NA
  plot(rl, col = rev(grey.colors(64, start = 0.30, end = 0.985)), legend = FALSE, axes = TRUE, xlim = ext[1:2], ylim = ext[3:4], mar = c(2.4, 2.4, 9.2, 2.6))
}
for (p in list_flights(DATA_DIR)) {
  if (!grepl("ARL-Suite", p)) next
  x <- tryCatch(read_icartt(p)$data, error = function(e) NULL); if (is.null(x)) next
  lines(x$Longitude, x$Latitude, col = "#B2182B40", lwd = 0.5)
}
legs <- read.csv(file.path(OUT_DIR, "urban_legs.csv")); vmax <- stats::quantile(legs$ch4_enh_mean_ppb, 0.95, na.rm = TRUE)
lu <- legs[legs$region == "urban", ]
points(lu$lon, lu$lat, pch = 21, bg = pal(lu$ch4_enh_mean_ppb, vmax), col = "gray25", lwd = 0.4, cex = 0.8)
rect(b$lon_w, b$lat_s, b$lon_e, b$lat_n, border = "#0a7d0a", lwd = 2)
if (!is.null(HS)) rect(HS$lon - 0.0235, HS$lat - 0.018, HS$lon + 0.0235, HS$lat + 0.018, border = "#1b9e3c", lwd = 1.6, lty = 1, col = "#1b9e3c22")
if (!is.null(OG) && nrow(OG)) points(OG$geom_lon, OG$geom_lat, pch = 4, col = "#e66101", lwd = 2, cex = 1.1)
points(SRC$lon, SRC$lat, pch = PCH[SRC$type], bg = BG[SRC$type], col = "white", lwd = 0.8, cex = 1.5)
segments(SRC$lon, SRC$lat, SRC$lab_lon, SRC$lab_lat, col = BG[SRC$type], lwd = 0.8)
points(SRC$lon, SRC$lat, pch = PCH[SRC$type], bg = BG[SRC$type], col = "white", lwd = 0.8, cex = 1.5)
for (i in seq_len(nrow(SRC))) halo(SRC$lon[i] * 0 + SRC$lab_lon[i], SRC$lab_lat[i], SRC$name[i], col = BG[SRC$type[i]], cex = 0.6, pos = NULL, font = 2)
CITY <- data.frame(name = c("Denver", "Aurora", "DIA", "Golden"), lat = c(39.7392, 39.729, 39.856, 39.755), lon = c(-104.9903, -104.832, -104.674, -105.221))
points(CITY$lon, CITY$lat, pch = 15, col = "gray35", cex = 0.6); halo(CITY$lon, CITY$lat, CITY$name, col = "gray25", cex = 0.6, pos = 1)
op <- par(no.readonly = TRUE)
par(fig = c(0, 1, 0.80, 1), new = TRUE, mar = c(0, 0, 0, 0))
plot(NA, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "", xaxs = "i", yaxs = "i")
text(0.5, 0.86, "Main methane sources in the analysis box", cex = 0.95, font = 2)
lx <- c(0.06, 0.06, 0.06, 0.52, 0.52, 0.52); ly <- c(0.60, 0.40, 0.20, 0.60, 0.40, 0.20)
lab <- c("landfill (Carbon Mapper source)", "wastewater treatment plant", "refinery", "gas-fired power station", "oil-and-gas site (Carbon Mapper)", "GRA2PES v2.0beta waste hotspot cell")
points(lx[1:4], ly[1:4], pch = PCH[c("landfill", "wastewater", "refinery", "power")], bg = BG[c("landfill", "wastewater", "refinery", "power")], col = "white", cex = 1.3)
points(lx[5], ly[5], pch = 4, col = "#e66101", lwd = 2, cex = 1.0)
rect(lx[6] - 0.014, ly[6] - 0.05, lx[6] + 0.014, ly[6] + 0.05, border = "#1b9e3c", col = "#1b9e3c22", lwd = 1.4)
text(lx + 0.03, ly, lab, pos = 4, cex = 0.6, offset = 0)
text(0.5, 0.04, paste0("circles: urban transect segments, coloured by CH4 enhancement as in (a); red lines: flight tracks", if (nzchar(CREDIT)) paste0("; map ", CREDIT) else ""), cex = 0.5, col = "gray25")
par(op); dev.off()

# (a) | (b)
fa <- file.path(FIGD, "Fig1_study_map.png")
if (file.exists(fa) && requireNamespace("png", quietly = TRUE)) {
  A <- png::readPNG(fa); B <- png::readPNG(file.path(FIGD, "Fig1b_box_sources.png"))
  png(file.path(FIGD, "Fig1_study_map_sources.png"), width = dim(A)[2] + dim(B)[2], height = max(dim(A)[1], dim(B)[1]), res = 200)
  par(mar = c(0, 0, 0, 0)); plot(NA, xlim = c(0, 2), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "", xaxs = "i", yaxs = "i")
  rasterImage(A, 0, 0, 1, 1); rasterImage(B, 1, 0, 2, 1)
  text(0.02, 0.975, "(a)", font = 2, cex = 1.1, pos = 4, offset = 0); text(1.02, 0.975, "(b)", font = 2, cex = 1.1, pos = 4, offset = 0)
  dev.off(); message("Wrote Fig1b_box_sources.png and Fig1_study_map_sources.png")
} else message("Wrote Fig1b_box_sources.png (Fig1_study_map.png or the png package not found: panels not combined)")
