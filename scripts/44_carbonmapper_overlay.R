# 44_carbonmapper_overlay.R ---------------------------------------------------
# Carbon Mapper point-source plume detections drawn on the gridded ethane
# fossil-signature map (Figure 4B / script 25), so the two independent views of
# the same box can be read together:
#
#   background cells  = delta-C2H6 : delta-CH4 slope per 0.02 deg cell, expressed as a
#                       fossil fraction (fossil_signature_grid.csv, script 25)
#   markers           = individual plumes detected by GAO / EMIT / Tanager-1
#                       (carbonmapper_denver_plumes.csv, scripts/43), sized by
#                       emission rate, shaped by the sector Carbon Mapper assigns
#
# WHAT IT ACTUALLY SHOWS (re-run 12 Sep 2026, after script 25 gained a leverage gate):
# the two views DO NOT OVERLAP AT ALL. No Carbon Mapper plume falls inside a coloured
# cell. Both landfill clusters sit east of the flight tracks, and the four oil-and-gas
# plumes that previously appeared to land in fossil-leaning cells landed in cells that
# should never have been coloured: 80 of 106 grid cells hold too narrow a range of
# methane enhancement to constrain a slope at all, and 16 carried a York slope outside
# the physical range [0, beta], clamping to a saturated 0% or 100%.
#
# An earlier version of this header reported "four plumes in cells with median fossil
# fraction 0.67 against 0.39 overall - a small but independent consistency check".
# That check was an artefact of those unconstrained cells and IS WITHDRAWN. There is no
# spatial agreement to report here, in either direction.
#
# The honest reading of the figure is coverage, not agreement: the landfills Carbon
# Mapper sees repeatedly are outside anything the ethane map can speak to.
#
# Run:  Rscript scripts/44_carbonmapper_overlay.R
# Out:  <OUT_DIR>/figures/FigX_carbonmapper_overlay.png
#       <OUT_DIR>/carbonmapper_vs_signature.csv   (each plume with the fossil
#                                                  fraction of the cell it falls in)
# Base R only. Needs scripts 25 and 43 to have run.
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))

# Run it without setting any environment variable: look for each input in OUT_DIR,
# then in the sibling outputs/ folder of the reorganised layout, then in the copies
# committed under results/. Outputs go wherever the inputs were found.
.find <- function(name, what) {
  cands <- c(file.path(OUT_DIR, name),
             file.path(proj, "..", "outputs", name),
             file.path(proj, "results", name))
  hit <- cands[file.exists(cands)]
  if (!length(hit)) stop(name, " not found in any of:\n  ", paste(cands, collapse = "\n  "),
                         "\n", what)
  hit[1]
}
grid_f  <- .find("fossil_signature_grid.csv", "Run scripts/25 (or bash run_local.sh) first.")
plume_f <- .find("carbonmapper_denver_plumes.csv", "Run bash scripts/43_carbonmapper_plumes.sh first.")
OUT_DIR <- dirname(normalizePath(if (basename(dirname(plume_f)) == "results")
                                 grid_f else plume_f))
if (basename(OUT_DIR) == "results") OUT_DIR <- normalizePath(file.path(proj, "..", "outputs"),
                                                             mustWork = FALSE)
dir.create(file.path(OUT_DIR, "figures"), showWarnings = FALSE, recursive = TRUE)
message("reading: ", grid_f, "\n         ", plume_f, "\nwriting to: ", OUT_DIR)
g <- read.csv(grid_f, stringsAsFactors = FALSE)
p <- read.csv(plume_f, stringsAsFactors = FALSE)
p <- p[p$gas == "CH4", , drop = FALSE]
message("grid cells: ", nrow(g), " | CH4 plumes: ", nrow(p),
        " (", sum(!is.na(p$kg_hr)), " quantified)")

CELL_DEG <- 0.02
# Fossil fraction of the cell each plume falls in, for the CSV and for the caption.
cell_ff <- function(lon, lat) {
  d <- abs(g$lon - lon) <= CELL_DEG / 2 & abs(g$lat - lat) <= CELL_DEG / 2
  if (!any(d)) NA_real_ else g$fossil_frac[which(d)[1]]
}
p$cell_fossil_frac <- mapply(cell_ff, p$lon, p$lat)
write.csv(p, file.path(OUT_DIR, "carbonmapper_vs_signature.csv"), row.names = FALSE)

# ---- figure -----------------------------------------------------------------
pal <- colorRampPalette(c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"))(64)
ff  <- pmin(1, pmax(0, g$fossil_frac))
col <- pal[pmax(1, ceiling(ff * 64))]

png(file.path(OUT_DIR, "figures", "FigX_carbonmapper_overlay.png"),
    width = 1700, height = 1300, res = 200)
op <- par(mar = c(4.2, 4.2, 3.2, 6.5))
plot(NA, xlim = c(URBAN_BOX$lon_w, URBAN_BOX$lon_e), ylim = c(URBAN_BOX$lat_s, URBAN_BOX$lat_n),
     xlab = "longitude", ylab = "latitude", asp = 1 / cos(39.7 * pi / 180),
     main = "Ethane fossil signature and Carbon Mapper plume detections")
rect(g$lon - CELL_DEG/2, g$lat - CELL_DEG/2, g$lon + CELL_DEG/2, g$lat + CELL_DEG/2,
     col = col, border = NA)
rect(URBAN_BOX$lon_w, URBAN_BOX$lat_s, URBAN_BOX$lon_e, URBAN_BOX$lat_n, border = "grey30")

# Marker area scales with emission rate; unquantified detections drawn small and hollow.
SEC_PCH <- c("6A" = 21, "1B2" = 24, "4D" = 25)          # waste / oil-gas / wastewater
SEC_COL <- c("6A" = "#111111", "1B2" = "#7b3294", "4D" = "#1f78b4")
pch <- unname(ifelse(p$sector %in% names(SEC_PCH), SEC_PCH[p$sector], 22))
brd <- unname(ifelse(p$sector %in% names(SEC_COL), SEC_COL[p$sector], "grey20"))
cexq <- ifelse(is.na(p$kg_hr), 0.7, 0.8 + 2.2 * sqrt(p$kg_hr / 1300))
points(p$lon, p$lat, pch = pch, cex = cexq, lwd = 1.6, col = brd,
       bg = ifelse(is.na(p$kg_hr), NA, adjustcolor("white", alpha.f = 0.75)))

# Facilities the analysis already tracks, for orientation.
srcfile <- file.path(proj, "biogenic_sources.csv")
if (file.exists(srcfile)) {
  L <- read.csv(srcfile, stringsAsFactors = FALSE)
  points(L$lon, L$lat, pch = 4, cex = 1.1, lwd = 2, col = "grey15")
  text(L$lon, L$lat, L$name, pos = 4, cex = 0.45, col = "grey15", offset = 0.4)
}

legend("topleft", bg = "white", box.col = "grey70", cex = 0.6, pt.cex = c(1.1, 1.1, 0.9),
       pch = c(21, 24, 4), col = c("#111111", "#7b3294", "grey15"), inset = c(0.01, 0.01),
       legend = c("plume, solid waste (6A)", "plume, oil & gas (1B2)", "tracked facility"))
legend("bottomright", bg = "white", box.col = "grey70", cex = 0.6, pt.lwd = 1.6,
       col = "#111111", pch = 21, inset = c(0.01, 0.01), y.intersp = 1.5,
       pt.cex = 0.8 + 2.2 * sqrt(c(100, 500, 1300) / 1300),
       legend = c("100 kg/hr", "500 kg/hr", "1300 kg/hr"))

# colour key for the background
par(fig = c(0.84, 0.885, 0.25, 0.78), new = TRUE, mar = c(0, 0, 0, 0))
plot(NA, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
rect(0, (0:63)/64, 1, (1:64)/64, col = pal, border = NA)
axis(4, at = c(0, .25, .5, .75, 1), labels = c("0", "25", "50", "75", "100%"),
     las = 1, cex.axis = 0.55, tck = -0.2)
mtext("cell fossil fraction", side = 3, line = 0.2, cex = 0.55)
par(op); dev.off()
cat("wrote", file.path(OUT_DIR, "figures", "FigX_carbonmapper_overlay.png"), "\n")

# ---- what the overlay says, in numbers --------------------------------------
ok <- !is.na(p$cell_fossil_frac)
if (any(ok)) {
  cat(sprintf("\nPlumes falling in a coloured cell: %d of %d\n", sum(ok), nrow(p)))
  cat(sprintf("  median cell fossil fraction at plume locations: %.2f\n",
              median(p$cell_fossil_frac[ok])))
  cat(sprintf("  median over all coloured cells:                 %.2f\n", median(g$fossil_frac)))
  for (s in unique(p$sector[ok])) {
    v <- p$cell_fossil_frac[ok & p$sector == s]
    cat(sprintf("  sector %-4s n=%2d  median cell fossil fraction %.2f\n", s, length(v), median(v)))
  }
} else cat("\nNo plume falls inside a coloured cell (the flights did not sample those cells).\n")
