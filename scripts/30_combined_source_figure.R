# 30_combined_source_figure.R -------------------------------------------------
# Assemble the two source-mix panels into ONE main-text figure, so the manuscript
# stays within its four-item figure/table limit:
#   A) the aggregate source-attribution bar  (Fig5_attribution.png, script 23)
#   B) the spatial fossil-signature maps      (Fig6_fossil_signature_map.png, script 25)
# Both source panels are produced by R (scripts 23 and 25); this step only stacks
# them and adds the A)/B) panel labels, so the combined figure stays fully
# reproducible. Run scripts 23 and 25 first.
#
# Needs the 'png' package to read the PNGs (install.packages("png") once).
#
# Out: <OUT_DIR>/figures/Fig4_source_combined.png
# Run: Rscript scripts/30_combined_source_figure.R
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
if (!requireNamespace("png", quietly = TRUE))
  stop("This step needs the 'png' package to read the panel PNGs. install.packages('png').")

FIG <- file.path(OUT_DIR, "figures")
fa  <- file.path(FIG, "Fig5_attribution.png")          # panel A (bar)
fb  <- file.path(FIG, "Fig6_fossil_signature_map.png") # panel B (maps)
if (!file.exists(fa)) stop("Missing ", fa, " -- run scripts/23_source_attribution.R first.")
if (!file.exists(fb)) stop("Missing ", fb, " -- run scripts/25_fossil_signature_map.R first.")

A <- png::readPNG(fa); B <- png::readPNG(fb)
aw <- dim(A)[2]; ah <- dim(A)[1]                       # width, height in pixels
bw <- dim(B)[2]; bh <- dim(B)[1]
W  <- max(aw, bw)                                       # canvas width; A centerd at native size
gap <- round(0.015 * W)
H  <- ah + gap + bh
ax <- (W - aw) / 2                                      # left edge of the (narrower) A panel

png(file.path(FIG, "Fig4_source_combined.png"), width = W, height = H, res = 200)
op <- par(mar = c(0, 0, 0, 0)); on.exit(par(op))
plot(NA, xlim = c(0, W), ylim = c(0, H), xaxs = "i", yaxs = "i",
     axes = FALSE, xlab = "", ylab = "")
# rasterImage places row 1 of the array at ytop, so pass (xleft, ybottom, xright, ytop)
rasterImage(B, 0, 0, W, bh)                             # panel B along the bottom
rasterImage(A, ax, bh + gap, ax + aw, H)               # panel A on top, centerd
lab <- function(x, y, s) text(x, y, s, adj = c(0, 1), cex = 2.2, font = 2, xpd = NA)
lab(ax + 0.008 * W, H - 0.006 * H, "A)")
lab(0.008 * W,      bh - 0.004 * H, "B)")
dev.off()
message("Wrote ", file.path(FIG, "Fig4_source_combined.png"),
        sprintf("  (%d x %d px: A on top, B below)", W, H))
