# 12_urban_figures.R ---------------------------------------------------------
# Main-text SOURCE-MIX figures (this script no longer draws the study map or any
# closed-loop figure):
#   Fig 2  per-flight fossil fraction of urban methane, WITH 95% leg-block
#          bootstrap confidence intervals (the evidence for the biogenic-dominated
#          claim: point estimate and whether the interval clears 50%).
#   Fig 3  fossil-vs-biogenic ethane:methane contrast (two example flights).
#
# The study map (Fig 1) and SI per-flight basemaps are script 20. The closed-loop
# mass balance is NOT applicable to this basin-optimized geometry, so there is no
# closed-loop figure here; the loop geometry is documented qualitatively in
# scripts 13 and 28 (Figure S6).
#
# Run:  Rscript scripts/12_urban_figures.R   (needs the flight ICARTT in DATA_DIR)
# Out:  <OUT_DIR>/figures/{Fig2_source_mix,Fig3_ethane_contrast}.png
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
source(file.path(proj, "R", "read_icartt.R")); source(file.path(proj, "R", "paths.R"))
source(file.path(proj, "R", "massbalance.R")); source(file.path(proj, "R", "enhancements.R"))
source(file.path(proj, "R", "ratios.R"))

BIO_FLIGHT    <- "20240710_R0_L1"     # biogenic-dominated example (Fig 3/left)
FOSSIL_FLIGHT <- "20240713_R0_L2"     # fossil-influenced example (Fig 3/right)
FIG <- file.path(OUT_DIR, "figures"); dir.create(FIG, showWarnings = FALSE, recursive = TRUE)
find_flight <- function(key) { f <- list_flights(DATA_DIR); f[grepl("ARL-Suite", f) & grepl(key, f)][1] }

# ---- Fig 2: per-flight fossil fraction with bootstrap CIs (same estimator as
# Table 1: instrument-weighted York + leg-block bootstrap) ----
ff <- function(s) 100 * fossil_fraction(s, SOURCE_C2H6_CH4)
rows <- list()
for (p in list_flights(DATA_DIR)) {
  if (!grepl("ARL-Suite", p)) next
  ic <- tryCatch(read_icartt(p), error = function(e) NULL); if (is.null(ic)) next
  if (!all(c("CH4_ppb","C2H6_ppb") %in% names(ic$data))) next
  d <- detect_level_legs(ic$data); if (!any(d$leg_id > 0)) next
  lg <- tag_region(leg_metrics(d)); urb <- lg$leg_id[lg$region == "urban"]
  du <- d[d$leg_id %in% urb, ]; if (length(unique(urb)) < 3 || nrow(du) < 50) next
  du <- add_enhancements(du, "CH4_ppb"); du <- add_enhancements(du, "C2H6_ppb")
  x <- du$CH4_ppb_enh; y <- du$C2H6_ppb_enh; k <- is.finite(x) & is.finite(y) & x > 20
  if (sum(k) < 10) next
  # Same estimator and bootstrap as Table 1 (script 21), via config.R FOSSIL_ESTIMATOR.
  fit <- fossil_slope_fit(x[k], y[k], du$leg_id[k], 1, 0.2)
  bo  <- tryCatch(york_boot(x[k], y[k], 1, 0.2, blocks = du$leg_id[k], within = fossil_method() == "york_within"),
                  error = function(e) list(lo = NA, hi = NA))
  fl <- sub("AMMBEC-ARL-Suite_TwinOtter_", "", sub(".ict", "", basename(p)))
  rows[[fl]] <- data.frame(flight = sub("_R0", "", fl),
    pct = ff(fit$slope), lo = ff(bo$lo), hi = ff(bo$hi), stringsAsFactors = FALSE)
}
S <- do.call(rbind, rows)
S$pct <- pmax(0, pmin(100, S$pct)); S$lo <- pmax(0, pmin(100, S$lo)); S$hi <- pmax(0, pmin(100, S$hi))
S <- S[order(S$pct), ]; n <- nrow(S)
dcol <- grDevices::rgb(colorRamp(c("#2C7FB8", "#D95F0E"))(S$pct/100)/255)
png(file.path(FIG, "Fig2_source_mix.png"), 1200, 720, res = 130); par(mar = c(7, 4.8, 3.5, 1))
plot(NA, xlim = c(.5, n+.5), ylim = c(0, 100), xaxt = "n", xlab = "",
     ylab = "Fossil fraction of urban methane (%)",
     main = "Day-to-day fossil vs biogenic source mix over the Denver metro")
abline(h = c(0,25,75,100), col = "gray92"); abline(h = 50, lty = 2, lwd = 2, col = "#c0392b")
text(0.55, 52, "majority fossil", col = "#c0392b", cex = .7, pos = 4)  # left end: the right end collides with the top flight's label
arrows(1:n, S$lo, 1:n, S$hi, angle = 90, code = 3, length = 0.04, col = dcol, lwd = 2)
points(1:n, S$pct, pch = 19, col = dcol, cex = 2.0)
text(1:n, S$pct, paste0(round(S$pct), "%"), pos = 3, cex = .7, offset = .8)
axis(1, at = 1:n, labels = S$flight, las = 2)
mtext("point estimate with 95% leg-block bootstrap interval", side = 3, line = 0.1, cex = .75, col = "grey40")
dev.off()

# ---- Fig 3: ethane:methane contrast for two example flights ----
em_urban <- function(f) { d <- detect_level_legs(read_icartt(f)$data); lg <- tag_region(leg_metrics(d))
  du <- d[d$leg_id %in% lg$leg_id[lg$region == "urban"], ]; du <- add_enhancements(du, "CH4_ppb"); du <- add_enhancements(du, "C2H6_ppb")
  k <- is.finite(du$CH4_ppb_enh) & is.finite(du$C2H6_ppb_enh) & du$CH4_ppb_enh > 20
  list(x = du$CH4_ppb_enh[k], y = du$C2H6_ppb_enh[k], fit = ethane_methane_ratio(du, "CH4_ppb", "C2H6_ppb", 20, method = fossil_method())) }
bio <- em_urban(find_flight(BIO_FLIGHT)); fos <- em_urban(find_flight(FOSSIL_FLIGHT))
png(file.path(FIG, "Fig3_ethane_contrast.png"), 1150, 620, res = 130); par(mfrow = c(1,2), mar = c(4.2,4.2,3,1))
plot(bio$x, bio$y, pch = 20, col = "#2C7FB8AA", cex = .6, xlab = "dCH4 (ppb)", ylab = "dC2H6 (ppb)",
     main = sprintf("%s: biogenic-dominated\nslope=%.3f", BIO_FLIGHT, bio$fit$slope))
if (is.finite(bio$fit$slope)) abline(bio$fit$intercept, bio$fit$slope, col = "#2C7FB8", lwd = 2)
plot(fos$x, fos$y, pch = 20, col = "#D95F0EAA", cex = .6, xlab = "dCH4 (ppb)", ylab = "dC2H6 (ppb)",
     main = sprintf("%s: fossil-influenced\nslope=%.3f", FOSSIL_FLIGHT, fos$fit$slope))
if (is.finite(fos$fit$slope)) abline(fos$fit$intercept, fos$fit$slope, col = "#D95F0E", lwd = 2)
dev.off()

message("Wrote Fig2_source_mix.png (with bootstrap CIs) and Fig3_ethane_contrast.png to ", FIG, ".")
message(sprintf("Fig 2: %d flights; intervals clearing 50%% (biogenic-dominant): %d of %d.",
                n, sum(round(S$hi) < 50, na.rm = TRUE), n))
# Counted on the ROUNDED upper bound, as Table 1 prints it and as paper_values.json's
# ci_below50 counts it (script 21): 0703_R0_L1's bound is 49.6, prints as 50, and a
# sentence saying it lies below 50% would contradict the table.
