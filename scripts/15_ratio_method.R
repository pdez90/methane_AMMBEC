# 15_ratio_method.R ----------------------------------------------------------
# Second, mass-balance-free urban CH4 emission estimate, after Schafer, Peischl
# et al. (2025, ES&T). For each flight with urban legs, the CH4 emission is the
# CH4:CO (or CH4:CO2) enhancement-ratio slope times an independent CO (or CO2)
# emission inventory for the metro:
#
#     E_CH4 = (dCH4/dCO)_OLS * (MW_CH4/MW_CO) * E_CO
#
# The emission slope uses OLS (stable; the York fit over-steepens at low
# correlation, so it is reported only in the SI comparison). Unlike mass balance
# this method needs no BLH or wind, so it applies to ALL urban flights, not only
# those that enclosed the city.
#
# The CO/CO2 inventories (config: E_CO_DENVER, E_CO2_DENVER, Gg/yr) scale the
# result linearly and must be set from EPA NEI (CO) / Vulcan or ODIAC (CO2).
#
# Run:  Rscript scripts/15_ratio_method.R
# Out:  <OUT_DIR>/ratio_method_flux.csv
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
source(file.path(proj, "R", "read_icartt.R")); source(file.path(proj, "R", "paths.R"))
source(file.path(proj, "R", "massbalance.R")); source(file.path(proj, "R", "enhancements.R"))
source(file.path(proj, "R", "ratios.R")); source(file.path(proj, "R", "qc.R"))
source(file.path(proj, "R", "lidar_blh.R"))

MW_CH4 <- 16.04; MW_CO <- 28.01; MW_CO2 <- 44.01
GGYR_TO_THR <- 1e3 / (365.25*24)          # Gg/yr -> t/hr
# Reliability thresholds for accepting a per-flight slope into the emission
# estimate (used by the usable_* flags below and, downstream, by script 21 in
# preference to correlation alone). An anchor-enhancement span this small, a single
# contributing leg, or a leave-one-leg-out slope range wider than half the slope
# all indicate a fragile fit. Ranges are in enhancement units (ppb CO, ppm CO2).
R_MIN <- 0.7; MIN_LEGS <- 2L; MIN_CO_RANGE <- 10; MIN_CO2_RANGE <- 2; L1O_FRAC_MAX <- 0.5

# The emission ratio uses OLS (stable and conservative). The instrument-precision
# York fit over-steepens and blows up at low correlation, because the true plume
# scatter is far larger than the instrument precision, so York is reported only
# alongside in the SI. The ethane fossil fraction still uses York (there it agrees
# with OLS and RMA is the one that inflates); see script 29.
.ols <- function(x, y) { k <- is.finite(x) & is.finite(y); if (sum(k) < 10) return(NA_real_)
  unname(stats::coef(stats::lm(y[k] ~ x[k]))[2]) }
.ols_block_ci <- function(x, y, blocks, B = 2000) {
  ok <- is.finite(x) & is.finite(y) & !is.na(blocks) & blocks > 0
  x <- x[ok]; y <- y[ok]; blocks <- blocks[ok]
  if (length(x) < 10) return(c(NA_real_, NA_real_))
  ub <- unique(blocks); sl <- numeric(B)
  for (b in seq_len(B)) { idx <- unlist(lapply(sample(ub, length(ub), replace = TRUE),
                                               function(g) which(blocks == g)))
    sl[b] <- .ols(x[idx], y[idx]) }
  stats::quantile(sl, c(0.025, 0.975), na.rm = TRUE, names = FALSE)
}
# Stability diagnostics for a per-flight slope, so we can tell whether one leg or
# a too-narrow anchor range is driving it (reviewer request). Returns the number
# of distinct legs, the anchor enhancement span, and the min/max slope under
# leave-one-leg-out refitting (a wide l1o range => the slope is leg-fragile).
.slope_diag <- function(x, y, blocks) {
  ok <- is.finite(x) & is.finite(y) & !is.na(blocks) & blocks > 0
  x <- x[ok]; y <- y[ok]; blocks <- blocks[ok]
  ub <- unique(blocks); nl <- length(ub)
  xr <- if (length(x)) diff(range(x)) else NA_real_
  if (nl >= 2) {
    l1o <- vapply(ub, function(g) .ols(x[blocks != g], y[blocks != g]), numeric(1))
    lo <- if (all(is.na(l1o))) NA_real_ else min(l1o, na.rm = TRUE)
    hi <- if (all(is.na(l1o))) NA_real_ else max(l1o, na.rm = TRUE)
  } else { lo <- NA_real_; hi <- NA_real_ }
  list(n_legs = nl, x_range = xr, l1o_min = lo, l1o_max = hi)
}

rows <- list()
for (p in list_flights(DATA_DIR)) {
  if (!grepl("ARL-Suite", p)) next
  ic <- tryCatch(read_icartt(p), error = function(e) NULL)
  if (is.null(ic) || !all(c("CH4_ppb","CO_ppb","CO2_ppm") %in% names(ic$data))) next
  d <- detect_level_legs(ic$data)
  if (!any(d$leg_id > 0)) next
  legs <- tag_region(leg_metrics(d))
  urb_ids <- legs$leg_id[legs$region == "urban"]
  du <- d[d$leg_id %in% urb_ids, ]
  # NB: the enhancement RATIO is boundary-layer-insensitive, so we use the full
  # in-box urban-leg data here. The daytime/PBL/>200 m QC is instead applied as a
  # robustness check on the fossil fractions in scripts/22_qc_robustness.R.
  if (nrow(du) < 50) next
  for (v in c("CH4_ppb","CO_ppb","CO2_ppm","C2H6_ppb")) du <- add_enhancements(du, v)

  # CH4:CO (molar ppb/ppb): OLS primary + leg-block bootstrap CI; York kept for SI
  kco <- is.finite(du$CH4_ppb_enh) & is.finite(du$CO_ppb_enh) & du$CO_ppb_enh > 5
  xco <- du$CO_ppb_enh[kco]; yco <- du$CH4_ppb_enh[kco]
  co_ols  <- .ols(xco, yco); co_ci <- .ols_block_ci(xco, yco, du$leg_id[kco])
  co_york <- york_slope(xco, yco, 5, 1)$slope
  rco <- suppressWarnings(stats::cor(xco, yco))
  co_diag <- .slope_diag(xco, yco, du$leg_id[kco])
  # CH4:CO2 (ppb/ppm -> molar x1e-3): OLS primary + leg-block bootstrap CI (parity
  # with CH4:CO) + leave-one-leg-out stability.
  kc2 <- is.finite(du$CH4_ppb_enh) & is.finite(du$CO2_ppm_enh) & du$CO2_ppm_enh > 1
  xc2 <- du$CO2_ppm_enh[kc2]; yc2 <- du$CH4_ppb_enh[kc2]
  c2_ols  <- .ols(xc2, yc2); c2_york <- york_slope(xc2, yc2, 0.1, 1)$slope
  c2_ci   <- .ols_block_ci(xc2, yc2, du$leg_id[kc2])
  rc2 <- suppressWarnings(stats::cor(xc2, yc2))
  c2_diag <- .slope_diag(xc2, yc2, du$leg_id[kc2])

  e_co   <- co_ols   * (MW_CH4/MW_CO)  * E_CO_DENVER  * GGYR_TO_THR
  e_co_lo<- co_ci[1] * (MW_CH4/MW_CO)  * E_CO_DENVER  * GGYR_TO_THR
  e_co_hi<- co_ci[2] * (MW_CH4/MW_CO)  * E_CO_DENVER  * GGYR_TO_THR
  e_co2   <- c2_ols   * 1e-3 * (MW_CH4/MW_CO2) * E_CO2_DENVER * GGYR_TO_THR
  e_co2_lo<- c2_ci[1] * 1e-3 * (MW_CH4/MW_CO2) * E_CO2_DENVER * GGYR_TO_THR
  e_co2_hi<- c2_ci[2] * 1e-3 * (MW_CH4/MW_CO2) * E_CO2_DENVER * GGYR_TO_THR

  # explicit reliability flags: correlation AND multi-leg AND adequate anchor range
  # AND leave-one-leg-out stability. Downstream (script 21) uses these, not r alone.
  l1o_ok <- function(dg, s) is.finite(dg$l1o_min) && is.finite(dg$l1o_max) &&
    is.finite(s) && s != 0 && abs(dg$l1o_max - dg$l1o_min) <= L1O_FRAC_MAX * abs(s)
  usable_co  <- is.finite(co_ols) && is.finite(rco)  && rco  >= R_MIN &&
                co_diag$n_legs >= MIN_LEGS && is.finite(co_diag$x_range) &&
                co_diag$x_range >= MIN_CO_RANGE  && l1o_ok(co_diag, co_ols)
  usable_co2 <- is.finite(c2_ols) && is.finite(rc2)  && rc2  >= R_MIN &&
                c2_diag$n_legs >= MIN_LEGS && is.finite(c2_diag$x_range) &&
                c2_diag$x_range >= MIN_CO2_RANGE && l1o_ok(c2_diag, c2_ols)

  # ethane fossil fraction stays York (agrees with OLS; RMA inflates at low r)
  fit <- ethane_methane_ratio(du, "CH4_ppb", "C2H6_ppb", 20, method = "york")
  rows[[p]] <- data.frame(
    flight = sub("AMMBEC-ARL-Suite_TwinOtter_","",sub(".ict","",basename(p))),
    date = as.character(ic$meta$date), n = sum(kco),
    ch4_co_slope = round(co_ols,3), ch4_co_slope_york = round(co_york,3), r_co = round(rco,2),
    n_legs_co = co_diag$n_legs, xrange_co = round(co_diag$x_range,1),
    co_l1o_min = round(co_diag$l1o_min,3), co_l1o_max = round(co_diag$l1o_max,3),
    E_CH4_from_CO_t_hr = round(e_co,2),
    E_CH4_CO_lo = round(e_co_lo,2), E_CH4_CO_hi = round(e_co_hi,2),
    ch4_co2_slope = round(c2_ols,2), ch4_co2_slope_york = round(c2_york,2), r_co2 = round(rc2,2),
    n_legs_co2 = c2_diag$n_legs, xrange_co2 = round(c2_diag$x_range,2),
    co2_l1o_min = round(c2_diag$l1o_min,2), co2_l1o_max = round(c2_diag$l1o_max,2),
    E_CH4_from_CO2_t_hr = round(e_co2,2),
    E_CH4_CO2_lo = round(e_co2_lo,2), E_CH4_CO2_hi = round(e_co2_hi,2),
    usable_co = usable_co, usable_co2 = usable_co2,
    c2h6_ch4_slope_york = round(fit$slope, 5),   # UNROUNDED-enough for beta sweep (script 27)
    fossil_frac_york = round(fossil_fraction(fit$slope, SOURCE_C2H6_CH4),2),
    stringsAsFactors = FALSE)
}
res <- do.call(rbind, rows); res <- res[order(res$date), ]
write.csv(res, file.path(OUT_DIR, "ratio_method_flux.csv"), row.names = FALSE)

# Report ranges over the flights that pass the FULL reliability screen (usable_*),
# not correlation alone, so the console summary matches what enters the manuscript.
good  <- res[res$usable_co  %in% TRUE, ]
good2 <- res[res$usable_co2 %in% TRUE, ]
message("\n=== Ratio-to-inventory urban CH4 (E_CO=", E_CO_DENVER, " Gg/yr) ===")
message("Flights passing the CH4:CO reliability screen (r>=0.7, >=2 legs, range, l1o-stable): ",
        nrow(good), " of ", nrow(res))
if (nrow(good)) message("E_CH4 (CO-anchored, usable) range: ", round(min(good$E_CH4_from_CO_t_hr),1),
        " - ", round(max(good$E_CH4_from_CO_t_hr),1), " t/hr; median ",
        round(stats::median(good$E_CH4_from_CO_t_hr),1))
if (nrow(good2)) message("E_CH4 (CO2-anchored, usable) range: ", round(min(good2$E_CH4_from_CO2_t_hr),1),
        " - ", round(max(good2$E_CH4_from_CO2_t_hr),1), " t/hr; n=", nrow(good2))
excluded <- res$flight[(res$r_co %in% NA | res$r_co >= 0.7) & !(res$usable_co %in% TRUE)]
if (length(excluded)) message("CH4:CO with r>=0.7 but EXCLUDED by the stability/range screen: ",
        paste(excluded[!is.na(excluded)], collapse = ", "))
print(res[, c("flight","n","ch4_co_slope","r_co","n_legs_co","co_l1o_min","co_l1o_max",
              "usable_co","usable_co2","E_CH4_from_CO_t_hr","E_CH4_from_CO2_t_hr","fossil_frac_york")], row.names = FALSE)
message("\nAnchors (config.R): E_CO=", E_CO_DENVER, " Gg/yr (GRA2PES box), E_CO2=",
        E_CO2_DENVER, " Gg/yr (Vulcan box). NEI 7-county upper bound = ", E_CO_NEI, " Gg/yr.")
