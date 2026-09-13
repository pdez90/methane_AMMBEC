# 48_perleg_fossil_fraction.R --------------------------------------------------
# Three ways to get one campaign fossil fraction, side by side, because the choice
# currently changes the headline number by a factor of ~1.75.
#
#   Rscript scripts/48_perleg_fossil_fraction.R
#
# Out: <OUT_DIR>/perleg_fossil_fraction.csv     one row per urban LEG
#      <OUT_DIR>/fossil_estimator_compare.csv   one row per FLIGHT, three estimators
#      printed: the campaign median under each, and the altitude/ethane structure
#
# WHY. Script 21 fits ONE York regression to all gated urban points of a flight and
# converts the slope to a fossil fraction, clamped to [0,1]. On two of the seven
# flights that pooled slope is NEGATIVE (20240708_R0_L1: -0.0285, r = -0.47, n = 121;
# 20240710_R0_L1: -0.0129, r = -0.34, n = 185), so both are recorded as 0% fossil and
# they set the campaign median: 24% over seven flights, 42% over the five with an
# admissible slope.
#
# A negative source ratio is not physically interpretable -- it would mean adding
# methane removes ethane. And script 35 shows the pooled negative is an AGGREGATION
# artefact: on 20240710_R0_L1 every individual leg has a POSITIVE slope (+0.0308,
# +0.0005, +0.0221) while the pooled fit is -0.0129, and the pooled |r| (0.34) exceeds
# every within-leg |r|. That is the signature of between-leg contrast, not a source
# mixing line: legs at different altitudes sit in air with different ethane
# backgrounds, and pooling measures that contrast instead of the ratio.
#
# Script 35's own note says an ethane offset "lowers r but does not bias the slope".
# True WITHIN a leg. Across legs it is false: an offset correlated with the leg's mean
# methane drags the pooled slope, which is what this script quantifies.
#
# THE THREE ESTIMATORS
#   pooled      - one York fit to all gated urban points            (what script 21 does)
#   leg_median  - median of the per-leg York slopes
#   leg_wmean   - per-leg slopes averaged, weighted by gated points per leg
# The leg is already the unit of resampling for the leg-block bootstrap, so making it
# the unit of fitting is a small change of estimator, not of method.
#
# This script REPORTS. It does not alter config.R, paper_values.json or any published
# number; which estimator the paper adopts is a scientific judgement, not a default.
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
source(file.path(proj, "R", "read_icartt.R")); source(file.path(proj, "R", "paths.R"))
source(file.path(proj, "R", "massbalance.R")); source(file.path(proj, "R", "enhancements.R"))
source(file.path(proj, "R", "ratios.R")); source(file.path(proj, "R", "qc.R"))

MIN_ENH      <- 20     # ppb dCH4 plume gate, as scripts 21/35
MIN_PTS_LEG  <- 10     # minimum gated points to fit a leg
MIN_PTS_FLT  <- 50     # as script 21
BETA         <- SOURCE_C2H6_CH4

ff_unclamped <- function(slope) 100 * slope / BETA          # may be < 0 or > 100
ff_clamped   <- function(slope) round(100 * fossil_fraction(slope, BETA))

leg_rows <- list(); flt_rows <- list()
# Count why flights drop out. A bare "no flights found" is useless for diagnosis, and the
# first version of this script hid a missing source() behind a silent tryCatch.
skipped <- c(not_arl = 0, unreadable = 0, no_species = 0, no_legs = 0,
             too_few_points = 0, no_gated = 0, no_fittable_leg = 0)

.flights <- list_flights(DATA_DIR)
if (!length(.flights))
  stop("no flight files under METHANE_DATA_DIR = ", DATA_DIR,
       "\n  Set the environment first:  eval \"$(bash run_local.sh --print-env)\"")
for (p in .flights) {
  if (!grepl("ARL-Suite", p)) { skipped["not_arl"] <- skipped["not_arl"] + 1; next }
  ic <- tryCatch(read_icartt(p), error = function(e) NULL)
  if (is.null(ic)) { skipped["unreadable"] <- skipped["unreadable"] + 1; next }
  if (!all(c("CH4_ppb", "C2H6_ppb") %in% names(ic$data))) {
    skipped["no_species"] <- skipped["no_species"] + 1; next }
  # NOT wrapped in tryCatch: an error here is a bug to see, not a flight to skip.
  d <- detect_level_legs(ic$data)
  if (!any(d$leg_id > 0)) { skipped["no_legs"] <- skipped["no_legs"] + 1; next }

  lg  <- tag_region(leg_metrics(d))
  urb <- lg$leg_id[lg$region == "urban"]
  du  <- d[d$leg_id %in% urb, ]
  if (nrow(du) < MIN_PTS_FLT) { skipped["too_few_points"] <- skipped["too_few_points"] + 1; next }
  du <- add_enhancements(du, "CH4_ppb"); du <- add_enhancements(du, "C2H6_ppb")
  fl <- sub("AMMBEC-ARL-Suite_TwinOtter_", "", sub("\\.ict$", "", basename(p)))

  # ---- pooled fit, exactly as script 21 ----
  x <- du$CH4_ppb_enh; y <- du$C2H6_ppb_enh
  k <- is.finite(x) & is.finite(y) & x > MIN_ENH
  if (sum(k) < MIN_PTS_LEG) { skipped["no_gated"] <- skipped["no_gated"] + 1; next }
  pooled   <- york_slope(x[k], y[k], 1, 0.2)$slope
  pooled_r <- suppressWarnings(stats::cor(x[k], y[k]))

  # ---- per-leg fits ----
  sl <- c(); wt <- c()
  for (L in unique(du$leg_id)) {
    dl <- du[du$leg_id == L, ]
    xl <- dl$CH4_ppb_enh; yl <- dl$C2H6_ppb_enh
    kl <- is.finite(xl) & is.finite(yl) & xl > MIN_ENH
    if (sum(kl) < MIN_PTS_LEG) next
    s  <- tryCatch(york_slope(xl[kl], yl[kl], 1, 0.2)$slope, error = function(e) NA_real_)
    if (!is.finite(s)) next
    sl <- c(sl, s); wt <- c(wt, sum(kl))
    leg_rows[[length(leg_rows) + 1]] <- data.frame(
      flight = fl, leg_id = L, n_gated = sum(kl),
      agl_m         = round(mean(dl$ALTAGL[kl], na.rm = TRUE)),
      dCH4_mean_ppb = round(mean(xl[kl], na.rm = TRUE), 1),
      dC2H6_med_ppb = round(stats::median(yl[kl], na.rm = TRUE), 2),
      leg_slope     = round(s, 5),
      leg_r         = round(suppressWarnings(stats::cor(xl[kl], yl[kl])), 2),
      stringsAsFactors = FALSE)
  }
  if (!length(sl)) { skipped["no_fittable_leg"] <- skipped["no_fittable_leg"] + 1; next }

  leg_median <- stats::median(sl)
  leg_wmean  <- sum(sl * wt) / sum(wt)
  flt_rows[[length(flt_rows) + 1]] <- data.frame(
    flight = fl, n_legs_fitted = length(sl), n_gated = sum(k),
    pooled_slope = round(pooled, 5), pooled_r = round(pooled_r, 2),
    leg_median_slope = round(leg_median, 5), leg_wmean_slope = round(leg_wmean, 5),
    legs_all_nonneg  = all(sl >= 0),
    ff_pooled     = ff_clamped(pooled),
    ff_leg_median = ff_clamped(leg_median),
    ff_leg_wmean  = ff_clamped(leg_wmean),
    ff_pooled_unclamped = round(ff_unclamped(pooled), 1),
    stringsAsFactors = FALSE)
}

legs <- do.call(rbind, leg_rows); flts <- do.call(rbind, flt_rows)
if (is.null(flts)) {
  cat("no flights produced a fittable urban leg. Where they went:\n")
  for (nm in names(skipped)) cat(sprintf("  %-16s %d\n", nm, skipped[[nm]]))
  stop("nothing to report -- see the counts above.")
}
write.csv(legs, file.path(OUT_DIR, "perleg_fossil_fraction.csv"), row.names = FALSE)
write.csv(flts, file.path(OUT_DIR, "fossil_estimator_compare.csv"), row.names = FALSE)

cat("\n=== per-flight, three estimators (fossil %, clamped to [0,100]) ===\n")
print(flts[, c("flight","n_legs_fitted","pooled_r","pooled_slope","leg_median_slope",
               "legs_all_nonneg","ff_pooled","ff_leg_median","ff_leg_wmean")],
      row.names = FALSE)

cat("\n=== campaign medians ===\n")
cat(sprintf("  pooled (current headline) : %5.1f %%   over %d flights\n",
            stats::median(flts$ff_pooled), nrow(flts)))
cat(sprintf("  per-leg median            : %5.1f %%\n", stats::median(flts$ff_leg_median)))
cat(sprintf("  per-leg weighted mean     : %5.1f %%\n", stats::median(flts$ff_leg_wmean)))
adm <- flts[flts$pooled_slope >= 0, ]
if (nrow(adm))
  cat(sprintf("  pooled, slope >= 0 only   : %5.1f %%   over %d flights\n",
              stats::median(adm$ff_pooled), nrow(adm)))

cat("\n=== flights where pooling reverses the sign ===\n")
bad <- flts[flts$pooled_slope < 0 & flts$legs_all_nonneg, ]
if (!nrow(bad)) cat("  none: no flight has all-positive leg slopes and a negative pooled slope.\n") else {
  for (i in seq_len(nrow(bad)))
    cat(sprintf("  %-16s pooled %+0.4f (r %+0.2f) but every fitted leg is >= 0\n",
                bad$flight[i], bad$pooled_slope[i], bad$pooled_r[i]))
  cat("  For these the pooled slope is a between-leg contrast, not a source ratio.\n")
}

# ---- does the ethane background track altitude? the proposed mechanism ----
cat("\n=== leg ethane background vs altitude (the proposed mechanism) ===\n")
ok <- is.finite(legs$agl_m) & is.finite(legs$dC2H6_med_ppb)
if (sum(ok) >= 4) {
  rr <- suppressWarnings(stats::cor(legs$agl_m[ok], legs$dC2H6_med_ppb[ok], method = "spearman"))
  rc <- suppressWarnings(stats::cor(legs$agl_m[ok], legs$dCH4_mean_ppb[ok], method = "spearman"))
  cat(sprintf("  across %d urban legs, Spearman r(AGL, dC2H6 median) = %+0.2f\n", sum(ok), rr))
  cat(sprintf("                        Spearman r(AGL, dCH4 mean)   = %+0.2f\n", rc))
  cat("  The mechanism predicts ethane background RISING and methane enhancement FALLING\n")
  cat("  with altitude (aged ethane-rich basin air aloft over fresh urban air below),\n")
  cat("  which pools into a negative slope. Both signs as predicted supports it;\n")
  cat("  either one against it does not, and the negative pooled slopes need another\n")
  cat("  explanation before any flight is excluded on these grounds.\n")
}
cat("\nwrote", file.path(OUT_DIR, "perleg_fossil_fraction.csv"), "and",
    file.path(OUT_DIR, "fossil_estimator_compare.csv"), "\n")
cat("Nothing published was changed; this script only reports.\n")
