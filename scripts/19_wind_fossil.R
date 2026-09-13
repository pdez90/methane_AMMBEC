# 19_wind_fossil.R ------------------------------------------------------------
# Why does the urban methane look biogenic on some days and more fossil on
# others? For each urban flight we compute the mean boundary-layer wind over the
# Denver box and relate it to the York fossil fraction. Result (EXPLORATORY, small
# n): higher fossil fractions tended to occur on days with greater north-easterly
# flow from the Wattenberg/DJB gas field. The correlation is suggestive, not a
# strong causal claim, given only a handful of evaluable flights.
#
# Out: <OUT_DIR>/wind_fossil.csv and <OUT_DIR>/figures/SI_wind_fossil.png
# Run: Rscript scripts/19_wind_fossil.R
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
source(file.path(proj, "R", "read_icartt.R")); source(file.path(proj, "R", "paths.R"))
source(file.path(proj, "R", "massbalance.R")); source(file.path(proj, "R", "enhancements.R"))
source(file.path(proj, "R", "ratios.R"))

rows <- list()
for (p in list_flights(DATA_DIR)) {
  if (!grepl("ARL-Suite", p)) next
  ic <- tryCatch(read_icartt(p), error = function(e) NULL)
  if (is.null(ic) || !all(c("CH4_ppb","C2H6_ppb","WS","WD") %in% names(ic$data))) next
  d <- detect_level_legs(ic$data); if (!any(d$leg_id > 0)) next
  legs <- tag_region(leg_metrics(d))
  du <- d[d$leg_id %in% legs$leg_id[legs$region == "urban"], ]
  if (nrow(du) < 50) next
  # WIND is characterized from the boundary-layer INFLOW: all in-box samples
  # below ~1.6 km AGL (the air actually advected over the city), not the stacked
  # level legs. FOSSIL fraction still comes from the urban-leg ethane ratio.
  inbl <- with(d, Latitude >= URBAN_BOX$lat_s & Latitude <= URBAN_BOX$lat_n &
                  Longitude >= URBAN_BOX$lon_w & Longitude <= URBAN_BOX$lon_e &
                  is.finite(ALTAGL) & ALTAGL < 1600)
  dw <- d[inbl, ]
  ws <- dw$WS; wd <- dw$WD; ok <- is.finite(ws) & is.finite(wd)
  if (sum(ok) < 20) next
  # mean wind FROM-direction as a unit-vector average; %from the NE quadrant
  ubar <- mean(sin(wd[ok] * pi/180)); vbar <- mean(cos(wd[ok] * pi/180))
  wd_from <- (atan2(ubar, vbar) * 180/pi) %% 360
  pct_ne <- 100 * mean(wd[ok] >= 22.5 & wd[ok] < 90)
  du <- add_enhancements(du, "CH4_ppb"); du <- add_enhancements(du, "C2H6_ppb")
  fit <- ethane_methane_ratio(du, "CH4_ppb", "C2H6_ppb", 20, method = fossil_method())
  rows[[p]] <- data.frame(
    flight = sub("AMMBEC-ARL-Suite_TwinOtter_", "", sub(".ict", "", basename(p))),
    date = as.character(ic$meta$date), n = sum(ok),
    mean_WS = round(mean(ws[ok]), 1), wd_from = round(wd_from), pct_from_NE = round(pct_ne),
    fossil_pct = round(100 * fossil_fraction(fit$slope, SOURCE_C2H6_CH4)),
    stringsAsFactors = FALSE)
}
res <- do.call(rbind, rows); res <- res[order(res$date), ]
write.csv(res, file.path(OUT_DIR, "wind_fossil.csv"), row.names = FALSE)

ok <- is.finite(res$fossil_pct) & is.finite(res$pct_from_NE)
r <- suppressWarnings(cor(res$pct_from_NE[ok], res$fossil_pct[ok]))
png(file.path(OUT_DIR, "figures", "SI_wind_fossil.png"), width = 1120, height = 920, res = 200)
par(mar = c(4.5, 4.5, 3.6, 1.4))
# Labels sit ABOVE their point, which keeps long flight ids off the left and
# right edges. Padding on both axes leaves room for them; where two points are
# close in x AND y their labels would overlap, so the second drops below.
xv <- res$pct_from_NE[ok]; yv <- res$fossil_pct[ok]
xr <- range(xv); yrg <- range(yv)
plot(xv, yv, pch = 19, col = "#b5179e", cex = 1.6,
     xlim = c(xr[1] - 0.12 * diff(xr) - 3, xr[2] + 0.12 * diff(xr) + 3),
     ylim = c(yrg[1] - 0.13 * diff(yrg) - 2, yrg[2] + 0.13 * diff(yrg) + 2),
     xlab = "% of urban samples with flow FROM the NE (Wattenberg/DJB)",
     ylab = "York fossil fraction (%)", cex.main = 0.90,
     main = sprintf("Higher fossil fractions on days with more NE (gas-field) flow\nPearson r = %.2f, n = %d (exploratory)", r, sum(ok)))
if (sum(ok) > 2) abline(lm(yv ~ xv), lty = 2, col = "grey40")
lab_pos <- rep(3L, length(xv))
ordx <- order(xv)
for (i in seq_along(ordx)) {          # compare against EVERY earlier label, not
  b <- ordx[i]                        # only the x-adjacent one: two points can
  for (j in seq_len(i - 1L)) {        # collide with others sitting between them
    a <- ordx[j]
    if (abs(xv[b] - xv[a]) < 0.22 * diff(xr) &&
        abs(yv[b] - yv[a]) < 0.12 * diff(yrg) &&
        lab_pos[a] == lab_pos[b]) lab_pos[b] <- 1L
  }
}
text(xv, yv, res$flight[ok], pos = lab_pos, cex = 0.6)
dev.off()
message("wind-fossil: Pearson r = ", round(r, 2), " (n = ", sum(ok), ")")
print(res, row.names = FALSE)
