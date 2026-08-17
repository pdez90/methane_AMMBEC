# 07_mobilelab.R -------------------------------------------------------------
# NOAA CSL Mobile Lab analysis (merged MetNav + Picarro + O3 + jNO2).
# Unlike the CAT surveys these carry CH4 AND position AND co-pollutants, so we
# can look at the drive track, methane enhancements, and the CH4:CO / CH4:CO2
# relationships that separate combustion from leak/biogenic sources.
#
# Run:  Rscript scripts/07_mobilelab.R [YYYYMMDD]   (default 20240709)
# Out:  <OUT_DIR>/mobilelab_<date>.csv, mobilelab_<date>_hotspots.csv,
#       <OUT_DIR>/figures/mobilelab_<date>_overview.png
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
source(file.path(proj, "R", "read_icartt.R"))
source(file.path(proj, "R", "read_mobilelab.R"))
source(file.path(proj, "R", "enhancements.R"))

args <- commandArgs(trailingOnly = TRUE)
date_str <- if (length(args)) args[1] else "20240709"
if (!dir.exists(MOBILELAB_DIR)) stop("MOBILELAB_DIR not found: ", MOBILELAB_DIR,
                                     "  (set METHANE_MOBILELAB_DIR)")

d <- read_mobilelab(MOBILELAB_DIR, date_str)
d <- add_enhancements(d, "CH4_ppb", window = 120, q = 0.05, n_sigma = 3)
write.csv(d, file.path(OUT_DIR, paste0("mobilelab_", date_str, ".csv")), row.names = FALSE)
hp <- d[d$plume, c("timestamp","Latitude","Longitude","CH4_ppb","CH4_ppb_enh","CO2_ppm","CO_ppb","O3_ppb")]
write.csv(hp, file.path(OUT_DIR, paste0("mobilelab_", date_str, "_hotspots.csv")), row.names = FALSE)

.heat <- function(v) {
  rng <- range(v, na.rm = TRUE); s <- (v - rng[1]) / (diff(rng) + 1e-9)
  cols <- rep("#BBBBBB", length(v)); ok <- is.finite(s)
  cols[ok] <- grDevices::rgb(colorRamp(c("#2166AC","#F7F7F7","#B2182B"))(s[ok])/255); cols
}

png(file.path(OUT_DIR, "figures", paste0("mobilelab_", date_str, "_overview.png")),
    1300, 1000, res = 120)
op <- par(mfrow = c(2,2), mar = c(4,4,3,1))

# (a) drive track coloured by CH4 enhancement (clipped for contrast)
clip <- stats::quantile(d$CH4_ppb_enh, 0.98, na.rm = TRUE)
plot(d$Longitude, d$Latitude, col = .heat(pmin(d$CH4_ppb_enh, clip)), pch = 20,
     cex = 0.7, asp = 1, xlab = "Longitude", ylab = "Latitude",
     main = paste0("Mobile Lab drive track — CH4 enhancement (", date_str, ")"))
pts <- d[d$plume, ]
if (nrow(pts)) points(pts$Longitude, pts$Latitude, col = "black", cex = 1.2, lwd = .6)

# (b) CH4 + O3 time series (dual axis)
plot(d$timestamp, d$CH4_ppb, type = "l", col = "firebrick", xlab = "UTC",
     ylab = "CH4 (ppb)", main = "CH4 and O3 time series")
par(new = TRUE)
plot(d$timestamp, d$O3_ppb, type = "l", col = "steelblue", axes = FALSE, xlab="", ylab="")
axis(4); mtext("O3 (ppb)", side = 4, line = -1.3, col = "steelblue")
legend("topleft", c("CH4","O3"), col = c("firebrick","steelblue"), lty = 1, bty = "n")

# (c) CH4 vs CO2 (combustion vs leak signature)
plot(d$CO2_ppm, d$CH4_ppb, pch = 20, cex = .4, col = "#00000055",
     xlab = "CO2 (ppm)", ylab = "CH4 (ppb)", main = "CH4 vs CO2")

# (d) CH4 enhancement histogram
hist(d$CH4_ppb_enh[d$CH4_ppb_enh > 0], breaks = 60, col = "grey70", border = NA,
     xlab = "CH4 enhancement (ppb)", main = "CH4 enhancement distribution")
par(op); dev.off()

message("Mobile Lab ", date_str, ": ", nrow(d), " GPS pts, ",
        sum(d$plume, na.rm=TRUE), " plume pts, CH4 max enh ",
        round(max(d$CH4_ppb_enh, na.rm=TRUE)), " ppb.")
message("Wrote mobilelab_", date_str, ".csv, hotspots, and overview figure.")
