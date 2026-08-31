# 42_toc_graphic.R -----------------------------------------------------------
# ES&T Table of Contents graphic, 3.25 x 1.75 inches at 300 dpi, drawn entirely
# from pipeline outputs so the numbers on it match the manuscript.
#
# Inputs (all produced by the pipeline):
#   <OUT_DIR>/ratio_method_flux.csv   <- script 15 (flight dates)
#   <OUT_DIR>/beta_endmember_ARC.csv  <- script 27 (per-flight fossil % at the ARC endmember)
#   <OUT_DIR>/beta_sensitivity.csv    <- script 27 (median fossil % at the 2024
#                                        best-estimate endmembers, noted rows)
#   <OUT_DIR>/paper_values.json       <- script 21 (fossil_median, gra_lo, gra_hi)
#
# Run:  Rscript scripts/42_toc_graphic.R
# Out:  <OUT_DIR>/figures/TOC_graphic.png   (975 x 525 px, 300 dpi)
#       <OUT_DIR>/toc_graphic_values.csv    (the numbers drawn, for the record)
# Base R only.
# ----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
OUT <- OUT_DIR
dir.create(file.path(OUT, "figures"), showWarnings = FALSE, recursive = TRUE)
need <- function(p) { if (!file.exists(p)) stop("missing input: ", p); p }

## ---- per-flight fossil fractions at the 2024 best-estimate endmember ---------
# Script 27 writes the per-flight fraction under the ARC ground ratio (0.0813),
# the upper end of the 2024 best estimate the abstract quotes (30 to 39% fossil),
# so the bars and the headline share one calibration. Dates come from script 15.
rmf <- read.csv(need(file.path(OUT, "ratio_method_flux.csv")), stringsAsFactors = FALSE)
arc <- read.csv(need(file.path(OUT, "beta_endmember_ARC.csv")), stringsAsFactors = FALSE)
stopifnot(all(c("flight", "fossil_pct_ARC") %in% names(arc)))
fl  <- merge(arc, rmf[, c("flight", "date")], by = "flight")
fl$date <- as.Date(fl$date); fl$fossil <- fl$fossil_pct_ARC / 100
fl  <- fl[is.finite(fl$fossil), ]
fl  <- fl[order(fl$date, fl$flight), ]
fl$lab <- format(fl$date, "%e Jul")
dup <- duplicated(fl$lab) | duplicated(fl$lab, fromLast = TRUE)
fl$lab[dup] <- paste0(fl$lab[dup], ifelse(grepl("_L2$", fl$flight[dup]), " (2)", " (1)"))
fossil_med_arc <- stats::median(fl$fossil_pct_ARC)

## ---- headline numbers ----------------------------------------------------------
raw <- paste(readLines(need(file.path(OUT, "paper_values.json")), warn = FALSE), collapse = " ")
jnum <- function(key) { m <- regmatches(raw, regexpr(sprintf('"%s"\\s*:\\s*-?[0-9.]+', key), raw))
  if (!length(m)) NA_real_ else as.numeric(sub('.*:\\s*', '', m)) }
fossil_med_adopted <- jnum("fossil_median")          # 24 at 0.102 (recorded only)
gra_lo <- jnum("gra_lo"); gra_hi <- jnum("gra_hi")   # 4.61 to 10.71 t/hr
# 2024 best-estimate endmember range: the noted rows of the beta sweep (implied
# source ratio from the basin legs, and the ARC ground ratio). Median fossil % there.
bs <- read.csv(need(file.path(OUT, "beta_sensitivity.csv")), stringsAsFactors = FALSE)
best <- bs[grepl("implied|ARC", bs$note), ]
if (!nrow(best)) stop("beta_sensitivity.csv has no noted best-estimate rows; run script 27 after 31")
fossil_best_lo <- min(best$median_fossil_pct); fossil_best_hi <- max(best$median_fossil_pct)
bio_lo <- 100 - fossil_best_hi; bio_hi <- 100 - fossil_best_lo   # 61 to 70/76
stopifnot(is.finite(c(fossil_med_adopted, gra_lo, gra_hi, bio_lo, bio_hi)))

write.csv(data.frame(fossil_median_adopted = fossil_med_adopted, fossil_median_ARC = fossil_med_arc,
                     fossil_median_best_lo = fossil_best_lo, fossil_median_best_hi = fossil_best_hi,
                     biogenic_lo = bio_lo, biogenic_hi = bio_hi,
                     gra_lo = gra_lo, gra_hi = gra_hi, n_flights = nrow(fl)),
          file.path(OUT, "toc_graphic_values.csv"), row.names = FALSE)

## ---- draw -------------------------------------------------------------------------
bioC <- "#2a9d8f"; fosC <- "#e07a3f"; ink <- "#1c2321"; muted <- "#6b7671"; bg <- "white"
W_IN <- 3.25; H_IN <- 1.75; DPI <- 300
png(file.path(OUT, "figures", "TOC_graphic.png"), width = W_IN, height = H_IN, units = "in",
    res = DPI, bg = bg, type = if (capabilities("cairo")) "cairo" else NULL)
par(family = "sans", xpd = NA)
layout(matrix(c(1, 2), nrow = 1), widths = c(1.45, 1.0))

# left: one bar per flight, biogenic share to the left, fossil share to the right
par(mar = c(1.7, 3.0, 1.35, 0.4))
n <- nrow(fl)
plot(NA, xlim = c(0, 100), ylim = c(0.4, n + 0.6), axes = FALSE, xlab = "", ylab = "", yaxs = "i")
for (i in seq_len(n)) {
  y <- n - i + 1; f <- 100 * fl$fossil[i]
  rect(0, y - 0.36, 100 - f, y + 0.36, col = bioC, border = NA)
  rect(100 - f, y - 0.36, 100, y + 0.36, col = fosC, border = NA)
}
text(-2, n:1, fl$lab, adj = 1, cex = 0.42, col = ink)
med_line <- 100 - fossil_med_arc
segments(med_line, 0.55, med_line, n + 0.45, col = ink, lwd = 1.1, lty = 2)
text(med_line, n + 0.72, "median", cex = 0.36, col = ink)
segments(0, 0.5, 100, 0.5, col = muted, lwd = 0.6)
segments(c(0, 50, 100), 0.5, c(0, 50, 100), 0.38, col = muted, lwd = 0.6)
text(50, n + 1.15, "Denver urban methane by flight, July 2024", cex = 0.48, font = 2, col = ink)
text(0, 0.22, "biogenic", adj = c(0, 1), cex = 0.42, font = 2, col = bioC)
text(100, 0.22, expression(bold("fossil, from " * C[2]*H[6]*":"*CH[4])), adj = c(1, 1), cex = 0.42, col = fosC)
text(50, -0.55, "share of each flight's urban methane", adj = c(0.5, 1), cex = 0.36, col = muted)

# right: the headline
par(mar = c(0.4, 0.2, 0.4, 0.3))
plot(NA, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
text(0.5, 0.86, "airborne ethane tracer", cex = 0.46, col = muted)
text(0.5, 0.64, sprintf("%d to %d%%", round(bio_lo), round(bio_hi)), cex = 1.55, font = 2, col = bioC)
text(0.5, 0.44, "of Denver's urban methane\nis biogenic", cex = 0.5, col = ink)
text(0.5, 0.20, bquote(.(sprintf("%.1f to %.1f t", gra_lo, gra_hi)) ~ CH[4] ~ "per hour,"), cex = 0.42, col = ink)
text(0.5, 0.10, "above two bottom-up inventories", cex = 0.42, col = ink)
dev.off()
cat(sprintf("wrote %s  (biogenic %d to %d%%, flux %.1f to %.1f t/hr, %d flights)\n",
            file.path(OUT, "figures", "TOC_graphic.png"), round(bio_lo), round(bio_hi), gra_lo, gra_hi, n))
