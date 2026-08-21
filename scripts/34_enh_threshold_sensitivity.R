# 34_enh_threshold_sensitivity.R ----------------------------------------------
# Does the fossil fraction depend on the dCH4 > 20 ppb plume gate?
#
# WHY THIS EXISTS. Every ethane:methane fit in this analysis keeps only points
# whose methane enhancement exceeds a threshold (min_enh, 20 ppb by default in
# R/ratios.R), so the slope reflects plumes rather than baseline scatter. That
# threshold is the one analysis parameter with no sensitivity test behind it: the
# endmember has script 27 and the data-selection filters have script 22, but the
# gate was simply asserted to be reasonable. This script tests it, in two ways.
#
#   (1) IS 20 ppb ACTUALLY ABOVE THE NOISE? The gate is meant to sit well above
#       both the instrument precision (1 ppb, the ICARTT value the York fit uses
#       as sx) and the scatter of the rolling background. The second of those was
#       never quantified. Here it is estimated per flight as the median absolute
#       deviation of the methane enhancement over the urban legs, scaled to a
#       standard deviation. MAD is used rather than sd because the enhancement
#       distribution has a long positive plume tail that would inflate an sd; the
#       MAD of that distribution is dominated by the near-background points and so
#       estimates baseline scatter even with plumes present.
#
#   (2) DOES THE ANSWER MOVE? Every urban flight's fossil fraction is recomputed
#       across a range of gates. If the campaign median is flat over that range,
#       the conclusion does not rest on the particular value chosen; if it is not,
#       that is a result the manuscript has to report.
#
# The gate is shared with the DJB basin ratio (script 31), so the basin median is
# swept alongside the urban one. A gate defensible for the city but not the basin
# would be a problem for section S8, where the two are compared.
#
# SELF-CHECK. At 20 ppb this reproduces the per-flight fossil fractions in
# table1.csv (script 21) exactly, since it uses the same leg selection, the same
# background, and the same York precisions. The check runs automatically when
# table1.csv is present and any mismatch is printed loudly, so the sweep can
# never quietly diverge from the pipeline it is supposed to be testing.
#
# RUNTIME NOTE. Bootstrap intervals are computed only at the gates in BOOT_AT,
# with the same B as the rest of the analysis. Bootstrapping every gate would
# multiply the cost of script 21 by the length of THRESH for intervals that are
# only used to show a trend.
#
# Needs: base R.
# Out:   <OUT_DIR>/enh_threshold_sensitivity.csv   per flight and gate (urban)
#        <OUT_DIR>/enh_threshold_summary.csv       per gate, campaign-level
#        <OUT_DIR>/enh_threshold_basin.csv         per gate, DJB basin legs
#        <OUT_DIR>/background_scatter.csv          per flight baseline noise
#        <OUT_DIR>/figures/FigS10_enh_threshold.png
# Run:   Rscript scripts/34_enh_threshold_sensitivity.R
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
source(file.path(proj, "R", "read_icartt.R")); source(file.path(proj, "R", "paths.R"))
source(file.path(proj, "R", "massbalance.R")); source(file.path(proj, "R", "enhancements.R"))
source(file.path(proj, "R", "ratios.R"))

THRESH   <- c(5, 10, 15, 20, 25, 30, 40, 50, 75, 100)   # ppb dCH4 gates to sweep
ADOPTED  <- 20                                          # the value the paper uses
BOOT_AT  <- c(10, 20, 50)                               # gates that get full intervals
B_BOOT   <- 2000                                        # same B as scripts 21 and 31
MIN_PTS  <- 10                                          # york_slope needs at least this
SX <- 1.0; SY <- 0.2                                    # ICARTT CH4 / C2H6 precisions

urban_rows <- list(); basin_rows <- list(); bg_rows <- list()

for (p in list_flights(DATA_DIR)) {
  if (!grepl("ARL-Suite", p)) next
  ic <- tryCatch(read_icartt(p), error = function(e) NULL)
  if (is.null(ic) || !all(c("CH4_ppb", "C2H6_ppb") %in% names(ic$data))) next
  fl <- sub("\\.ict$", "", sub("AMMBEC-ARL-Suite_TwinOtter_", "", basename(p)))

  d <- detect_level_legs(ic$data); if (!any(d$leg_id > 0)) next
  legs <- tag_region(leg_metrics(d))

  # Identical selection to script 21, so the 20 ppb column is comparable.
  for (reg in c("urban", "basin")) {
    ids <- legs$leg_id[legs$region == reg]
    dr  <- d[d$leg_id %in% ids, ]
    if (nrow(dr) < 50) next
    dr <- add_enhancements(dr, "CH4_ppb"); dr <- add_enhancements(dr, "C2H6_ppb")
    x <- dr$CH4_ppb_enh; y <- dr$C2H6_ppb_enh; g <- dr$leg_id
    fin <- is.finite(x) & is.finite(y)

    if (reg == "urban") {
      # (1) baseline scatter of the enhancement, robust to the plume tail
      sig <- stats::mad(x[fin], na.rm = TRUE)
      bg_rows[[fl]] <- data.frame(
        flight = fl, date = as.character(ic$meta$date), n_points = sum(fin),
        bg_scatter_ppb = round(sig, 2),
        gate_over_scatter = if (is.finite(sig) && sig > 0) round(ADOPTED / sig, 1) else NA_real_,
        gate_over_precision = ADOPTED / SX, stringsAsFactors = FALSE)
    }

    for (thr in THRESH) {
      k <- fin & x > thr
      if (sum(k) < MIN_PTS) {
        row <- data.frame(flight = fl, date = as.character(ic$meta$date), region = reg,
          gate_ppb = thr, n_points = sum(k), n_legs = length(unique(g[k])),
          slope = NA_real_, fossil_pct = NA_real_, lo_pct = NA_real_, hi_pct = NA_real_,
          stringsAsFactors = FALSE)
      } else {
        sl <- york_slope(x[k], y[k], SX, SY)$slope
        lo <- hi <- NA_real_
        if (thr %in% BOOT_AT) {
          bo <- york_boot(x[k], y[k], SX, SY, blocks = g[k], B = B_BOOT)
          lo <- 100 * fossil_fraction(bo$lo, SOURCE_C2H6_CH4)
          hi <- 100 * fossil_fraction(bo$hi, SOURCE_C2H6_CH4)
        }
        row <- data.frame(flight = fl, date = as.character(ic$meta$date), region = reg,
          gate_ppb = thr, n_points = sum(k), n_legs = length(unique(g[k])),
          slope = round(sl, 5), fossil_pct = round(100 * fossil_fraction(sl, SOURCE_C2H6_CH4), 1),
          lo_pct = round(lo, 1), hi_pct = round(hi, 1), stringsAsFactors = FALSE)
      }
      if (reg == "urban") urban_rows[[length(urban_rows) + 1L]] <- row
      else                basin_rows[[length(basin_rows) + 1L]] <- row
    }
  }
}

U  <- if (length(urban_rows)) do.call(rbind, urban_rows) else NULL
BA <- if (length(basin_rows)) do.call(rbind, basin_rows) else NULL
BG <- if (length(bg_rows))    do.call(rbind, bg_rows)    else NULL
if (is.null(U)) { message("No urban flights with enough data; nothing to sweep."); quit(save = "no") }

# ---- campaign summary per gate ----------------------------------------------
summ <- do.call(rbind, lapply(THRESH, function(thr) {
  s <- U[U$gate_ppb == thr & is.finite(U$fossil_pct), ]
  data.frame(gate_ppb = thr, n_flights = nrow(s),
    median_fossil_pct = if (nrow(s)) round(stats::median(s$fossil_pct)) else NA_real_,
    min_fossil_pct = if (nrow(s)) round(min(s$fossil_pct)) else NA_real_,
    max_fossil_pct = if (nrow(s)) round(max(s$fossil_pct)) else NA_real_,
    median_n_points = if (nrow(s)) round(stats::median(s$n_points)) else NA_real_,
    n_majority_fossil = sum(s$fossil_pct > 50), stringsAsFactors = FALSE)
}))

basin_summ <- if (!is.null(BA)) do.call(rbind, lapply(THRESH, function(thr) {
  s <- BA[BA$gate_ppb == thr & is.finite(BA$slope) & BA$slope > 0, ]
  data.frame(gate_ppb = thr, n_flights = nrow(s),
    median_ratio = if (nrow(s)) round(stats::median(s$slope), 4) else NA_real_,
    median_n_points = if (nrow(s)) round(stats::median(s$n_points)) else NA_real_,
    stringsAsFactors = FALSE)
})) else NULL

write.csv(U, file.path(OUT_DIR, "enh_threshold_sensitivity.csv"), row.names = FALSE)
write.csv(summ, file.path(OUT_DIR, "enh_threshold_summary.csv"), row.names = FALSE)
if (!is.null(basin_summ)) write.csv(basin_summ, file.path(OUT_DIR, "enh_threshold_basin.csv"), row.names = FALSE)
if (!is.null(BG)) write.csv(BG, file.path(OUT_DIR, "background_scatter.csv"), row.names = FALSE)

# ---- self-check against table1.csv ------------------------------------------
t1p <- file.path(OUT_DIR, "table1.csv")
if (file.exists(t1p)) {
  t1 <- read.csv(t1p, stringsAsFactors = FALSE)
  ours <- U[U$gate_ppb == ADOPTED, c("flight", "fossil_pct")]
  theirs <- data.frame(flight = t1$Flight,
                       t1_pct = suppressWarnings(as.numeric(sub(" .*", "", t1$Fossil_pct_CI))))
  m <- merge(ours, theirs, by = "flight", all = FALSE)
  m$diff <- round(m$fossil_pct) - m$t1_pct
  bad <- m[is.finite(m$diff) & abs(m$diff) > 1, ]
  if (nrow(bad)) {
    message("\n*** SELF-CHECK FAILED: the ", ADOPTED, " ppb column does not match table1.csv ***")
    print(bad)
  } else {
    message(sprintf("Self-check OK: the %d ppb column matches table1.csv on %d flights.",
                    ADOPTED, sum(is.finite(m$diff))))
  }
} else message("table1.csv not found; skipping the self-check (run script 21 first).")

# ---- figure -----------------------------------------------------------------
FIG <- file.path(OUT_DIR, "figures"); dir.create(FIG, showWarnings = FALSE, recursive = TRUE)
png(file.path(FIG, "FigS10_enh_threshold.png"), width = 1500, height = 760, res = 165)
par(mfrow = c(1, 2), mar = c(4.4, 4.4, 2.8, 1.0))

fls <- unique(U$flight)
plot(range(THRESH), c(0, 100), type = "n", xlab = expression(Delta*"CH"[4]*" gate (ppb)"),
     ylab = "Fossil fraction (%)", main = "Fossil fraction vs plume gate", cex.main = 0.95)
abline(h = 50, col = "grey70", lty = 3)
for (f in fls) {
  s <- U[U$flight == f & is.finite(U$fossil_pct), ]
  if (nrow(s) > 1) lines(s$gate_ppb, s$fossil_pct, col = "#9aa6b2", lwd = 1)
}
lines(summ$gate_ppb, summ$median_fossil_pct, col = "#b5179e", lwd = 3)
points(summ$gate_ppb, summ$median_fossil_pct, col = "#b5179e", pch = 19, cex = 0.8)
abline(v = ADOPTED, col = "#0a7d0a", lwd = 1.6, lty = 2)
text(ADOPTED, 96, "adopted", col = "#0a7d0a", cex = 0.6, pos = 4)
legend("topright", c("campaign median", "individual flights"),
       col = c("#b5179e", "#9aa6b2"), lwd = c(3, 1), bty = "n", cex = 0.62)

plot(summ$gate_ppb, summ$median_n_points, type = "b", pch = 19, col = "#2C7FB8", log = "y",
     xlab = expression(Delta*"CH"[4]*" gate (ppb)"), ylab = "Median points per flight",
     main = "Sample size vs gate", cex.main = 0.95)
abline(v = ADOPTED, col = "#0a7d0a", lwd = 1.6, lty = 2)
abline(h = MIN_PTS, col = "grey60", lty = 3)
text(max(THRESH), MIN_PTS, "fit minimum", col = "grey40", cex = 0.55, pos = 3)
dev.off()

# ---- report -----------------------------------------------------------------
cat("\n=== Plume-gate sensitivity ===\n")
if (!is.null(BG)) {
  cat(sprintf("Baseline scatter of dCH4 over urban legs: median %.1f ppb across %d flights (range %.1f to %.1f).\n",
              stats::median(BG$bg_scatter_ppb, na.rm = TRUE), nrow(BG),
              min(BG$bg_scatter_ppb, na.rm = TRUE), max(BG$bg_scatter_ppb, na.rm = TRUE)))
  cat(sprintf("The %d ppb gate is therefore %.0fx the instrument precision (%.1f ppb) and about %.0fx the baseline scatter.\n\n",
              ADOPTED, ADOPTED / SX, SX,
              ADOPTED / stats::median(BG$bg_scatter_ppb, na.rm = TRUE)))
}
print(summ, row.names = FALSE)
ok <- summ[is.finite(summ$median_fossil_pct), ]
base <- summ$median_fossil_pct[summ$gate_ppb == ADOPTED]
if (!nrow(ok) || !length(base) || !is.finite(base)) {
  cat("\nNo gate produced an evaluable campaign median; nothing to summarise.\n")
} else {
  cat(sprintf("\nCampaign median at the adopted %d ppb gate: %d%%.\n", ADOPTED, base))
  cat(sprintf("Across gates %d to %d ppb the median spans %d to %d%%, a maximum shift of %d points.\n",
              min(ok$gate_ppb), max(ok$gate_ppb), min(ok$median_fossil_pct), max(ok$median_fossil_pct),
              max(abs(ok$median_fossil_pct - base))))
  cat(sprintf("Flights majority fossil: %s across the sweep (%d at the adopted gate).\n",
              paste(range(ok$n_majority_fossil), collapse = " to "),
              summ$n_majority_fossil[summ$gate_ppb == ADOPTED]))
}
if (!is.null(basin_summ)) {
  cat("\nDJB basin ratio (script 31 uses the same gate):\n")
  print(basin_summ, row.names = FALSE)
}
cat("\nWrote enh_threshold_sensitivity.csv, enh_threshold_summary.csv, enh_threshold_basin.csv,\n")
cat("background_scatter.csv and figures/FigS10_enh_threshold.png\n")
