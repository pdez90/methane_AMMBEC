# 26_mobile_plume_figure.R ----------------------------------------------------
# Uses the CDPHE mobile-survey data more directly, by SHOWING the plumes rather
# than only reporting a multi-year trend. Two panels:
#   Left  : the distribution of per-survey peak methane enhancement at each of
#           the three surveyed facilities, over all 2023 to 2025 surveys. This
#           shows how large and how repeatable the facility plumes are.
#   Right : every plume detection from the Suncor P66 surveys, mapped and
#           coloured by enhancement, over the Suncor refinery and the adjacent
#           Metro Water Recovery (Robert W. Hite) wastewater plant. This shows
#           the spatial footprint of the plumes at the co-located complex.
#
# Reads the outputs of script 03 (mobile_survey_summary.csv, mobile_hotspots.csv).
# These ground surveys are not coincident with the flights and are used only to
# characterise the facility source types, never to constrain the airborne budget.
#
# Out: <OUT_DIR>/figures/FigS4_mobile_plumes.png
# Run: Rscript scripts/26_mobile_plume_figure.R   (after script 03)
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))

sumf <- file.path(OUT_DIR, "mobile_survey_summary.csv")
hotf <- file.path(OUT_DIR, "mobile_hotspots.csv")
if (!file.exists(sumf)) { message("No mobile_survey_summary.csv; run script 03 first."); quit(save = "no") }
S <- read.csv(sumf, stringsAsFactors = FALSE)
# Use the per-survey 95th-percentile enhancement (stable) rather than the maximum
# (a 1 s spike, sensitive to survey duration). Fall back to max only if p95 absent.
S$metric <- if ("ch4_enh_p95" %in% names(S)) S$ch4_enh_p95 else S$ch4_enh_max
metric_lbl <- if ("ch4_enh_p95" %in% names(S)) "95th-percentile" else "peak"
S <- S[is.finite(S$metric), ]
if (!nrow(S)) { message("No usable mobile surveys."); quit(save = "no") }

# tidy site labels
lab <- c(SuncorP66 = "Suncor refinery", HEPTerminal = "HEP terminal",
         CollinsAerospace = "Collins Aerospace")
S$sitelab <- ifelse(S$site %in% names(lab), lab[S$site], S$site)
sites <- names(sort(tapply(S$metric, S$sitelab, stats::median), decreasing = TRUE))

# facility coordinates for the map overlay (from biogenic_sources.csv if present)
srcfile <- file.path(proj, "biogenic_sources.csv")
FAC <- if (file.exists(srcfile)) read.csv(srcfile, stringsAsFactors = FALSE) else
  data.frame(name = c("Suncor refinery","Metro Water Recovery (Robert W. Hite)"),
             lat = c(39.802, 39.792), lon = c(-104.939, -104.938),
             type = c("refinery","wastewater"), stringsAsFactors = FALSE)
pch_fac <- c(refinery = 24, wastewater = 25, landfill = 22, other = 21)
bg_fac  <- c(refinery = "#c0392b", wastewater = "#2980b9", landfill = "#000000", other = "white")

FIG <- file.path(OUT_DIR, "figures"); dir.create(FIG, showWarnings = FALSE, recursive = TRUE)
png(file.path(FIG, "FigS4_mobile_plumes.png"), width = 1700, height = 780, res = 150)
par(mfrow = c(1, 2), mar = c(7, 4.5, 3, 1))

## ---- Left: per-survey enhancement percentile by facility --------------------
grp <- lapply(sites, function(s) S$metric[S$sitelab == s])
boxplot(grp, names = sites, las = 2, outline = TRUE, col = "#dfeaf5",
        ylab = sprintf("Per-survey %s CH4 enhancement (ppmv)", metric_lbl),
        main = "CDPHE facility plumes, 2023 to 2025")
ns <- vapply(grp, length, 1L)
mtext(sprintf("n = %d surveys", sum(ns)), side = 3, line = 0.2, cex = 0.8, col = "#555555")
text(seq_along(sites), par("usr")[3] - 0.04*diff(par("usr")[3:4]),
     labels = sprintf("(%d)", ns), xpd = TRUE, cex = 0.75, col = "#555555")

## ---- Right: Suncor plume detections mapped over a Denver basemap ------------
par(mar = c(4, 4.5, 3, 1))
# Vulcan fossil-CO2 raster as an offline basemap (same source as Figs 1 and S1);
# traces the urban/industrial footprint behind the plume points. Falls back to a
# plain background if terra or the raster is unavailable, so the script never fails.
.local_basemap <- function(xlim, ylim) {
  if (!requireNamespace("terra", quietly = TRUE) ||
      !exists("VULCAN_FILE") || !file.exists(VULCAN_FILE)) return(NULL)
  tryCatch({
    r <- terra::rast(VULCAN_FILE)
    e <- terra::project(terra::as.polygons(terra::ext(xlim[1], xlim[2], ylim[1], ylim[2]),
                                           crs = "EPSG:4326"), terra::crs(r))
    r <- terra::project(terra::crop(r, e), "EPSG:4326")
    rl <- log10(r); rl[rl < log10(20)] <- NA
    rl
  }, error = function(e) NULL)
}
if (file.exists(hotf)) {
  H <- read.csv(hotf, stringsAsFactors = FALSE)
  H <- H[H$site == "SuncorP66" & is.finite(H$Latitude) & is.finite(H$Longitude) &
         is.finite(H$CH4_ppmv_enh), ]
  if (nrow(H)) {
    # focus on the complex; clip extreme coordinates
    cx <- stats::median(FAC$lon); cy <- stats::median(FAC$lat)
    H <- H[abs(H$Longitude - cx) < 0.06 & abs(H$Latitude - cy) < 0.06, ]
    ord <- order(H$CH4_ppmv_enh)                    # draw strongest on top
    H <- H[ord, ]
    ramp <- colorRamp(c("#2166AC", "#F7F7F7", "#B2182B"))
    z <- H$CH4_ppmv_enh; zc <- pmin(z, stats::quantile(z, 0.98, na.rm = TRUE))
    zc <- (zc - min(zc)) / (diff(range(zc)) + 1e-9)
    col <- grDevices::rgb(ramp(zc), maxColorValue = 255)
    # plot extent: plumes + both facilities, with a small margin
    xlim <- range(c(H$Longitude, FAC$lon)); ylim <- range(c(H$Latitude, FAC$lat))
    xlim <- xlim + c(-1, 1) * (0.12 * diff(xlim) + 0.004)
    ylim <- ylim + c(-1, 1) * (0.12 * diff(ylim) + 0.004)
    bm <- .local_basemap(xlim, ylim)
    if (!is.null(bm)) {
      terra::plot(bm, col = rev(grey.colors(64, start = 0.30, end = 0.97)), legend = FALSE,
                  xlim = xlim, ylim = ylim, xlab = "Longitude", ylab = "Latitude",
                  main = "Suncor P66 survey plume detections", cex.main = 0.95)
    } else {
      plot(NA, xlim = xlim, ylim = ylim, xlab = "Longitude", ylab = "Latitude",
           asp = 1/cos(39.8*pi/180), main = "Suncor P66 survey plume detections", cex.main = 0.95)
    }
    points(H$Longitude, H$Latitude, pch = 20, cex = 0.5, col = col)
    ty <- ifelse(FAC$type %in% names(pch_fac), FAC$type, "other")
    points(FAC$lon, FAC$lat, pch = unname(pch_fac[ty]), bg = unname(bg_fac[ty]),
           col = "black", cex = 1.6, lwd = 1.5)
    text(FAC$lon, FAC$lat, FAC$name, pos = 4, cex = 0.6, font = 2)
    legend("topright", bty = "n", cex = 0.65, pt.cex = 1.2, bg = "white",
           legend = c("refinery","wastewater"), pch = c(24,25),
           pt.bg = c("#c0392b","#2980b9"))
    legend("bottomleft", bty = "n", cex = 0.7, title = "plume ΔCH4", bg = "white",
           legend = c("lower","higher"), fill = c("#2166AC","#B2182B"))
  } else { plot.new(); title("No Suncor plume detections found") }
} else { plot.new(); title("mobile_hotspots.csv not found") }
dev.off()
message("Wrote figures/FigS4_mobile_plumes.png")
message(sprintf("Facilities surveyed: %s", paste(sites, collapse = ", ")))
