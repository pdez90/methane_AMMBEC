# 21_tables.R -----------------------------------------------------------------
# Regenerates the manuscript's Table 1 and the three-method emission-estimate
# table directly from the pipeline, so every number in the paper is reproducible.
#
# Table 1  : per urban flight -> #urban legs, lidar BLH, York fossil % [95% CI],
#            closed-loop flux (where physically valid).
# Emissions: per flight -> closed-loop mass balance, CH4:CO2 x Vulcan,
#            CH4:CO x GRA2PES (box), CH4:CO x NEI (7-county upper bound).
#
# Out: <OUT_DIR>/table1.csv, <OUT_DIR>/emission_estimates.csv
# Run: Rscript scripts/21_tables.R   (after 13 and 15 have produced their CSVs)
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
source(file.path(proj, "R", "read_icartt.R")); source(file.path(proj, "R", "paths.R"))
source(file.path(proj, "R", "massbalance.R")); source(file.path(proj, "R", "enhancements.R"))
source(file.path(proj, "R", "ratios.R")); source(file.path(proj, "R", "qc.R"))

G <- 1000 / (365.25 * 24); MWc <- 16.04 / 28.01; MWc2 <- 16.04 / 44.01
cl <- read.csv(file.path(OUT_DIR, "closeloop_diagnostic.csv"), stringsAsFactors = FALSE)

rows <- list()
for (p in list_flights(DATA_DIR)) {
  if (!grepl("ARL-Suite", p)) next
  ic <- tryCatch(read_icartt(p), error = function(e) NULL); if (is.null(ic)) next
  if (!all(c("CH4_ppb","C2H6_ppb") %in% names(ic$data))) next
  d <- detect_level_legs(ic$data); if (!any(d$leg_id > 0)) next
  legs <- tag_region(leg_metrics(d))
  urb <- legs$leg_id[legs$region == "urban"]; du <- d[d$leg_id %in% urb, ]
  if (nrow(du) < 50) next
  du <- add_enhancements(du, "CH4_ppb"); du <- add_enhancements(du, "C2H6_ppb")
  x <- du$CH4_ppb_enh; y <- du$C2H6_ppb_enh; k <- is.finite(x) & is.finite(y) & x > 20
  # ESTIMATOR (config.R FOSSIL_ESTIMATOR): "within" fits one York slope after centring
  # each leg on its own mean, so a flight whose legs sit in different air masses (a
  # landfill plume in one, the industrial corridor in another) is not scored on the
  # contrast between them. "pooled" is one York slope through all gated points, the
  # estimator of the original submission. Both slopes are kept in table1_full.csv.
  # The leg-block bootstrap is unchanged: legs are resampled, and each resample is
  # fitted with the same estimator as the point value.
  fit_pool <- york_slope(x[k], y[k], 1, 0.2)
  fit_with <- york_within_leg(x[k], y[k], du$leg_id[k], 1, 0.2)
  fit <- if (FOSSIL_ESTIMATOR == "within") fit_with else fit_pool
  bo  <- york_boot(x[k], y[k], 1, 0.2, blocks = du$leg_id[k],
                   within = FOSSIL_ESTIMATOR == "within")
  ff <- function(s) round(100 * fossil_fraction(s, SOURCE_C2H6_CH4))
  fl <- sub("AMMBEC-ARL-Suite_TwinOtter_", "", sub(".ict", "", basename(p)))
  # Keep the RAW slope and the correlation alongside the clamped percentage.
  # fossil_fraction() clamps to [0,1]. Under the POOLED estimator two of the seven
  # flights have a negative slope (20240708_R0_L1 -0.0285, 20240710_R0_L1 -0.0129) and
  # enter the median as 0%; scripts 48/49 trace both to a between-leg contrast (one leg
  # is the DADS landfill plume with no ethane), and the within-leg estimator gives them
  # +0.0019 and +0.0044 (2% and 4%). Whichever estimator is adopted, the other's
  # numbers are emitted too, so the comparison is never done by hand.
  rows[[p]] <- data.frame(flight = fl, date = as.character(ic$meta$date),
    urban_legs = length(urb), fossil_pct = ff(fit$slope),
    fossil_lo = ff(bo$lo), fossil_hi = ff(bo$hi),
    ethane_slope = fit$slope,
    ethane_r = if (FOSSIL_ESTIMATOR == "within") fit_with$r else
               suppressWarnings(stats::cor(x[k], y[k])),
    ethane_slope_pooled = fit_pool$slope, ethane_slope_within = fit_with$slope,
    fossil_pct_pooled = ff(fit_pool$slope), fossil_pct_within = ff(fit_with$slope),
    n_legs_within = fit_with$n_blocks,
    stringsAsFactors = FALSE)
}
tab <- do.call(rbind, rows)
write.csv(tab, file.path(OUT_DIR, "table1_full.csv"), row.names = FALSE)
message("Fossil-fraction estimator: ", FOSSIL_ESTIMATOR,
        " (METHANE_FOSSIL_ESTIMATOR=pooled reproduces the submitted numbers)")
rmf <- read.csv(file.path(OUT_DIR, "ratio_method_flux.csv"), stringsAsFactors = FALSE)
# Script 15 must have run under the SAME estimator, or Table 1 and the attribution
# would silently mix two definitions of the fossil fraction.
if ("fossil_estimator" %in% names(rmf) && !all(rmf$fossil_estimator == FOSSIL_ESTIMATOR))
  stop("ratio_method_flux.csv was written with estimator '", rmf$fossil_estimator[1],
       "' but config.R now selects '", FOSSIL_ESTIMATOR, "'. Re-run scripts/15_ratio_method.R.")
u_co  <- rmf$usable_co  %in% c(TRUE, "TRUE")
u_co2 <- rmf$usable_co2 %in% c(TRUE, "TRUE")

## Emission estimates: two enhancement-ratio methods, gated on the reliability
## FLAGS (correlation AND >=2 legs AND anchor range AND leave-one-leg-out
## stability), not correlation alone. Closed-loop mass balance is not applicable
## to this basin-optimized geometry (script 13), so it is not an emission method.
em <- data.frame(flight = rmf$flight,
  CH4CO_GRA2PES_t_hr = ifelse(u_co,  rmf$E_CH4_from_CO_t_hr, NA),
  CH4CO_GRA2PES_lo   = ifelse(u_co,  rmf$E_CH4_CO_lo, NA),
  CH4CO_GRA2PES_hi   = ifelse(u_co,  rmf$E_CH4_CO_hi, NA),
  CH4CO2_Vulcan_t_hr = ifelse(u_co2, rmf$E_CH4_from_CO2_t_hr, NA),
  CH4CO2_Vulcan_lo   = ifelse(u_co2, rmf$E_CH4_CO2_lo, NA),
  CH4CO2_Vulcan_hi   = ifelse(u_co2, rmf$E_CH4_CO2_hi, NA),
  CH4CO_NEI_t_hr     = ifelse(u_co,  round(rmf$ch4_co_slope*MWc*E_CO_NEI*G, 1), NA),
  CH4CO_NEIbox_t_hr  = ifelse(u_co,  round(rmf$ch4_co_slope*MWc*E_CO_NEI_BOX*G, 1), NA),
  stringsAsFactors = FALSE)
write.csv(em, file.path(OUT_DIR, "emission_estimates.csv"), row.names = FALSE)

## Table 1 — flights with >= 3 urban legs. Fossil fraction + the two ratio
## emission estimates WITH bootstrap intervals + a usability flag. No closed-loop
## column, because no flight provides a valid closed-loop flux (script 13).
mm <- merge(tab, rmf[, c("flight","E_CH4_from_CO_t_hr","E_CH4_CO_lo","E_CH4_CO_hi","usable_co",
                         "E_CH4_from_CO2_t_hr","E_CH4_CO2_lo","E_CH4_CO2_hi","usable_co2")],
            by = "flight", all.x = TRUE)
mm <- mm[order(mm$date), ]; mm <- mm[mm$urban_legs >= 3, ]
uco  <- mm$usable_co  %in% c(TRUE, "TRUE"); uco2 <- mm$usable_co2 %in% c(TRUE, "TRUE")
fmtE <- function(e, lo, hi, ok) ifelse(ok & is.finite(e), sprintf("%.1f [%.1f-%.1f]", e, lo, hi), "not usable")
t1 <- data.frame(
  Flight = mm$flight, Urban_legs = mm$urban_legs,
  # A flight with only one leg that clears the 10-point gate has no leg-block interval
  # under the within-leg estimator (see york_boot); say so rather than print [x-x].
  Fossil_pct_CI = ifelse(is.finite(mm$fossil_lo) & is.finite(mm$fossil_hi),
                         sprintf("%d [%d-%d]", mm$fossil_pct, mm$fossil_lo, mm$fossil_hi),
                         ifelse(is.finite(mm$fossil_pct),
                                sprintf("%d [single leg; no CI]", mm$fossil_pct), "NA")),
  CH4CO_GRA2PES_t_hr = fmtE(mm$E_CH4_from_CO_t_hr, mm$E_CH4_CO_lo, mm$E_CH4_CO_hi, uco),
  CH4CO2_Vulcan_t_hr = fmtE(mm$E_CH4_from_CO2_t_hr, mm$E_CH4_CO2_lo, mm$E_CH4_CO2_hi, uco2),
  Usable = ifelse(uco & uco2, "CO, CO2", ifelse(uco, "CO", ifelse(uco2, "CO2", "none"))),
  stringsAsFactors = FALSE)
write.csv(t1, file.path(OUT_DIR, "table1.csv"), row.names = FALSE)

## ---- paper_values.json : every number the manuscript + SI quote, generated by R ----
## Uses safe summaries so an empty vector yields JSON null, never Inf/-Inf/NaN.
r1 <- function(x) if (length(x) == 1 && is.finite(x)) round(x, 1) else NA_real_
safe_min <- function(x){ x <- x[is.finite(x)]; if (length(x)) min(x) else NA_real_ }
safe_max <- function(x){ x <- x[is.finite(x)]; if (length(x)) max(x) else NA_real_ }
safe_med <- function(x){ x <- x[is.finite(x)]; if (length(x)) stats::median(x) else NA_real_ }
jn <- function(x) if (length(x) == 0 || is.na(x) || !is.finite(x)) "null" else as.character(round(x, 3))
# jn() keeps 3 decimals, which would print the 0.0247 median slope as 0.025;
# the ethane:methane slopes are quoted to 4 decimals (2.47%), so emit them that way.
jn4 <- function(x) if (length(x) == 0 || is.na(x) || !is.finite(x)) "null" else sprintf("%.4f", x)
ja <- function(x){ x <- x[is.finite(x)]; paste0("[", paste(vapply(x, function(z) as.character(round(z,1)), ""), collapse = ","), "]") }
rd <- function(f) read.csv(file.path(OUT_DIR, f), stringsAsFactors = FALSE)

foss <- mm$fossil_pct[is.finite(mm$fossil_pct)]
n_fossil_flights <- sum(is.finite(mm$fossil_pct))
# Same median, restricted to flights whose ethane:methane slope is physically admissible
# (>= 0). See the note where ethane_slope is recorded.
.adm <- is.finite(mm$fossil_pct) & is.finite(mm$ethane_slope) & mm$ethane_slope >= 0
foss_adm  <- mm$fossil_pct[.adm]
n_clamped <- sum(is.finite(mm$fossil_pct) & is.finite(mm$ethane_slope) & mm$ethane_slope < 0)
if (n_clamped > 0) {
  message("\n*** ", n_clamped, " of ", n_fossil_flights,
          " flights have a NEGATIVE ethane:methane slope and are clamped to 0% fossil.")
  for (i in which(is.finite(mm$fossil_pct) & is.finite(mm$ethane_slope) & mm$ethane_slope < 0))
    message(sprintf("      %-16s slope %+.4f  r %+.2f  -> reported as 0%%",
                    mm$flight[i], mm$ethane_slope[i], mm$ethane_r[i]))
  message("    campaign median fossil: ", safe_med(foss), "% over all ", n_fossil_flights,
          " flights; ", safe_med(foss_adm), "% over the ", length(foss_adm),
          " with slope >= 0. Both are in paper_values.json; quote whichever the text defends.")
}
# The other estimator's campaign median, for the sentence that compares them.
foss_pooled <- mm$fossil_pct_pooled[is.finite(mm$fossil_pct_pooled)]
foss_within <- mm$fossil_pct_within[is.finite(mm$fossil_pct_within)]
message(sprintf("    campaign median fossil, pooled York: %s%%; within-leg York: %s%%  (adopted: %s)",
                safe_med(foss_pooled), safe_med(foss_within), FOSSIL_ESTIMATOR))
ci_below50 <- sum(is.finite(mm$fossil_hi) & mm$fossil_hi < 50)   # intervals entirely biogenic
vul  <- sort(em$CH4CO2_Vulcan_t_hr[is.finite(em$CH4CO2_Vulcan_t_hr)])
gra  <- em$CH4CO_GRA2PES_t_hr[is.finite(em$CH4CO_GRA2PES_t_hr)]
nei  <- em$CH4CO_NEI_t_hr[is.finite(em$CH4CO_NEI_t_hr)]
neib <- em$CH4CO_NEIbox_t_hr[is.finite(em$CH4CO_NEIbox_t_hr)]
n_encl <- sum(cl$encloses_metro %in% c(TRUE, "TRUE"))       # 0 for this campaign
# Lidar mixing heights for the eight urban flights, filled by script 08 from the
# monthly velStats files. Section 2.4 quotes this range, so it is EMITTED here
# (blh_valid_lo/hi) rather than read off by hand. Requires script 08 to have run;
# if curtain_config.csv still has empty blh_m these come back null.
blh_all <- cl$blh_m / 1000
uf <- rd("urban_flux.csv"); n_dma <- sum(uf$n_urban_legs > 0); n_ge3 <- sum(uf$n_urban_legs >= 3)
inv <- rd("inventory_comparison.csv"); gs <- function(k) sum(inv$t_hr[inv$group == k])
epaF <- gs("fossil"); epaB <- gs("biogenic"); epaC <- gs("combustion")
epaO <- sum(inv$t_hr) - epaF - epaB - epaC; epaT <- epaF + epaB + epaC
ng <- sum(inv$t_hr[grepl("Distribution|PostMeter", inv$sector)])
wf <- rd("wind_fossil.csv"); wok <- is.finite(wf$fossil_pct) & is.finite(wf$pct_from_NE)
wr <- suppressWarnings(cor(wf$pct_from_NE[wok], wf$fossil_pct[wok]))
ne13 <- sort(wf$pct_from_NE[grepl("20240713", wf$flight)], decreasing = TRUE)
qcd <- rd("qc_robustness.csv")$delta; qcmax <- if (any(is.finite(qcd))) max(qcd, na.rm = TRUE) else NA_real_
# CAVEAT: whole-flight (script 02) diagnostic; NOT the urban endmember (see config.R).
.maxslope_measured <- TRUE
maxslope <- tryCatch({ fem <- rd("flight_ethane_methane.csv")
  v <- if ("wholeflight_c2h6_ch4_slope" %in% names(fem)) fem$wholeflight_c2h6_ch4_slope else fem$c2h6_ch4_slope
  s <- suppressWarnings(round(max(v, na.rm = TRUE), 3)); if (is.finite(s)) s else stop("no finite slope")
}, error = function(e) {
  .maxslope_measured <<- FALSE
  warning("*** flight_ethane_methane.csv unreadable; falling back to a hardcoded ",
          "max ethane slope of 0.08, which NO script produced in this run. Reason: ",
          conditionMessage(e), immediate. = TRUE)
  0.08
})
# A missing input must never substitute a hardcoded number in silence: the Vulcan
# box area is quoted in the manuscript, so a silent fallback would publish a
# constant that no script produced. Warn loudly and mark it in paper_values.json.
.vcells_measured <- TRUE
vcells <- tryCatch({ v <- rd("vulcan_co2_boxsum.csv")$box_cells_km2[1]
  if (length(v) && is.finite(v)) v else stop("no finite box_cells_km2") },
  error = function(e) {
    .vcells_measured <<- FALSE
    warning("*** vulcan_co2_boxsum.csv is missing or unreadable. Falling back to the ",
            "hardcoded 2755 km2 Vulcan box area, which NO script produced in this run. ",
            "Run scripts/16_vulcan_co2_boxsum.R (it is in run_all.R) before quoting ",
            "this number. Reason: ", conditionMessage(e), immediate. = TRUE)
    2755
  })

# Representative attribution from the PRIMARY box-consistent method only
# (CH4:CO x GRA2PES), paired per flight: median(E*f), NOT median(E)*median(f), and
# NOT merged across estimators. It is an inventory-proportional allocation of the
# top-down fossil/biogenic totals, not an independent inversion.
# Urban ethane:methane slopes (unrounded York, script 15) for the endmember
# discussion: the campaign median and maximum slope, and the median fossil fraction
# the same slopes would give under a DELIVERED-GAS endmember (config.R,
# SOURCE_C2H6_CH4_PIPELINE_MEAN, Plant et al. 2019 Table S5 six-city mean). This is
# the reversal case the Discussion quotes; it is emitted here so the number is not
# computed by hand.
sl_u <- suppressWarnings(as.numeric(rmf$c2h6_ch4_slope_york)); sl_u <- sl_u[is.finite(sl_u)]
med_slope <- safe_med(sl_u); max_slope <- safe_max(sl_u)
ff_pipe   <- if (is.finite(med_slope)) round(100 * max(0, min(1, med_slope / SOURCE_C2H6_CH4_PIPELINE_MEAN))) else NA_real_
ei <- rmf$E_CH4_from_CO_t_hr; fi <- rmf$fossil_frac_york
uu <- u_co & is.finite(ei) & is.finite(fi)
Ef <- safe_med((ei * fi)[uu]); Eb <- safe_med((ei * (1 - fi))[uu])
E_rep <- if (is.finite(Ef) && is.finite(Eb)) Ef + Eb else NA_real_
f_rep <- if (is.finite(E_rep) && E_rep > 0) Ef / E_rep else NA_real_
bden <- sum(inv$t_hr[inv$group == "biogenic"]); fden <- sum(inv$t_hr[inv$group == "fossil"])
shr <- function(sec) { v <- inv$t_hr[inv$sector == sec]; if (length(v)) v[1] else 0 }
attr_ww  <- Eb * shr("5D_Wastewater_Treatment_Domestic") / bden
attr_lf  <- Eb * shr("5A1_Landfills_MSW") / bden
attr_ngd <- Ef * shr("1B2b_Natural_Gas_Distribution") / fden

pv <- paste0("{\n",
 '"n_dma":', n_dma, ', "n_ge3":', n_ge3, ', "n_valid_loop":', n_encl, ',\n',
 '"fossil_min":', jn(safe_min(foss)), ', "fossil_max":', jn(safe_max(foss)), ', "fossil_median":', jn(safe_med(foss)), ',\n',
 '"fossil_median_slope_ge0":', jn(safe_med(foss_adm)), ', "n_fossil_clamped":', n_clamped,
 ', "n_fossil_slope_ge0":', length(foss_adm), ',\n',
 '"n_fossil_flights":', n_fossil_flights, ', "ci_below50":', ci_below50, ',\n',
 '"fossil_estimator":"', FOSSIL_ESTIMATOR, '", "fossil_median_pooled":', jn(safe_med(foss_pooled)),
 ', "fossil_median_within":', jn(safe_med(foss_within)), ',\n',
 '"massbal":[],\n',
 '"blh_valid_lo":', jn(safe_min(blh_all)), ', "blh_valid_hi":', jn(safe_max(blh_all)), ', "blh_min":', jn(safe_min(blh_all)), ',\n',
 '"vulcan":', ja(vul), ', "vulcan_n":', length(vul), ',\n',
 '"gra_lo":', jn(safe_min(gra)), ', "gra_hi":', jn(safe_max(gra)), ', "gra_median":', jn(safe_med(gra)), ', "gra_n":', length(gra), ',\n',
 '"nei_lo":', jn(safe_min(nei)), ', "nei_hi":', jn(safe_max(nei)), ',\n',
 '"nei_box_lo":', jn(safe_min(neib)), ', "nei_box_hi":', jn(safe_max(neib)), ', "nei_box_median":', jn(safe_med(neib)), ',\n',
 '"E_CO_NEI_BOX":', E_CO_NEI_BOX, ', "gra_box_over_county_co":', GRA2PES_BOX_OVER_COUNTY_CO, ',\n',
 '"E_CO2":', E_CO2_DENVER, ', "E_CO":', E_CO_DENVER, ', "E_CO_NEI":', E_CO_NEI, ', "E_CH4_GRA2PES":', E_CH4_GRA2PES, ', "source_ratio":', SOURCE_C2H6_CH4, ',\n',
 '"vulcan_cells":', vcells, ', "gra_cells":171, "max_ethane_slope":', maxslope, ',\n',
 '"vulcan_cells_measured":', tolower(as.character(.vcells_measured)), ', "max_ethane_slope_measured":', tolower(as.character(.maxslope_measured)), ',\n',
 '"box":{"lat_s":', URBAN_BOX$lat_s, ',"lat_n":', URBAN_BOX$lat_n, ',"lon_w":', URBAN_BOX$lon_w, ',"lon_e":', URBAN_BOX$lon_e, '},\n',
 '"epa_total":', jn(epaT), ', "epa_fossil":', jn(epaF), ', "epa_biogenic":', jn(epaB), ', "epa_combustion":', jn(epaC), ', "epa_other":', jn(epaO), ',\n',
 '"epa_fossil_pct":', round(100*epaF/epaT), ', "epa_fossil_frac":', round(100*epaF/(epaF+epaB)), ', "epa_ng":', jn(ng), ',\n',
 '"wind_r":', round(wr, 2), ', "wind_n":', sum(wok), ', "wind_ne_0713":', ja(ne13), ',\n',
 '"attr_total":', jn(E_rep), ', "attr_fossil_pct":', jn(round(100*f_rep)), ', "attr_fossil":', jn(Ef), ', "attr_biogenic":', jn(Eb), ',\n',
 '"attr_landfills":', jn(attr_lf), ', "attr_wastewater":', jn(attr_ww), ', "attr_ngdist":', jn(attr_ngd), ',\n',
 '"qc_max_delta":', jn(qcmax), ',\n',
 '"median_urban_slope":', jn4(med_slope), ', "max_urban_slope":', jn4(max_slope), ',\n',
 '"beta_pipeline_mean":', SOURCE_C2H6_CH4_PIPELINE_MEAN, ', "fossil_median_pipeline_mean":', jn(ff_pipe), '\n}')
writeLines(pv, file.path(OUT_DIR, "paper_values.json"))
message("Wrote paper_values.json (all manuscript + SI numbers, generated by R).")
message(sprintf("Urban ethane:methane slope: median %.4f, max %.4f. Under the delivered-gas endmember %.4f the median would read %s%% fossil.",
                med_slope, max_slope, SOURCE_C2H6_CH4_PIPELINE_MEAN, ifelse(is.finite(ff_pipe), ff_pipe, "NA")))
message(sprintf("Inventory groups: fossil %.2f, biogenic %.2f, combustion %.2f, other %.3f (%.1f%%) t/hr",
                epaF, epaB, epaC, epaO, 100*epaO/sum(inv$t_hr)))

message("== Table 1 (no closed-loop column; ratio estimates gated on usability flags) ==")
print(t1, row.names = FALSE)
message("\n== Emission estimates (t/hr; NA where the reliability flag is FALSE) ==")
print(em, row.names = FALSE)
if (length(gra)) message(sprintf("\nGRA2PES-CO anchored, usable flights: %.1f - %.1f t/hr (median %.1f), n=%d",
        safe_min(gra), safe_max(gra), safe_med(gra), length(gra))) else
  message("\nNo GRA2PES-CO estimate passed the reliability flags.")
