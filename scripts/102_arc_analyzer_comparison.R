# 102_arc_analyzer_comparison.R ---------------------------------------------------
# ARL car: how much does the choice of methane analyzer (Picarro G2401-m vs Aeris) change the
# plume ethane-to-methane ratios? Plumes are defined as in script 71 on the G2401 methane (lag
# aligned to the Aeris); for each plume this computes the York slope and the plume-integrated
# ratio (sum dC2H6 / sum dCH4) with the G2401 and with the Aeris methane, and, for plumes that are
# ethane-bearing (r >= 0.7, slope >= 0.01) under both, the median G2401/Aeris ratio of each.
# Out: <OUT_DIR>/arc_analyzer_plumes.csv, arc_analyzer_comparison.csv
# Run: Rscript scripts/102_arc_analyzer_comparison.R      (after script 71)
# -------------------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R")); for (f in c("read_icartt", "enhancements", "ratios")) source(file.path(proj, "R", paste0(f, ".R")))
src <- readLines(file.path(proj, "scripts", "71_arc_ground_ethane.R"))
for (pair in list(c("^\\.num <- function","^# CALIBRATION SCREEN"), c("^CAL_STATIONARY_KMH","^# ---- read one day"), c("^\\.col <- function","^# ---- per-day processing"))) {
  i <- grep(pair[1], src); j <- grep(pair[2], src) - 1; eval(parse(text = src[i:j])) }
sm <- function(x, k) as.numeric(stats::filter(x, rep(1/k, k), sides = 2))
res <- list()
ARC_DIR <- Sys.getenv("METHANE_ARC_DIR", unset = "")
if (!nzchar(ARC_DIR)) for (.c in c(file.path(DATA_DIR, "ARC"), file.path(dirname(DATA_DIR), "instruments", "ARC"))) if (dir.exists(.c)) { ARC_DIR <- .c; break }
ff <- arc_files(ARC_DIR); paths <- sort(if (length(ff$ict)) ff$ict else ff$zip)
for (p in paths) {
  d <- read_arc_day(p); d <- d[order(d$timestamp), ]; d <- d[!cal_flag(d), ]
  t <- as.numeric(d$timestamp); okp <- is.finite(d$CH4_picarro_ppb) & d$CH4_picarro_ppb > 1500
  L <- arc_lag(t, d$CH4_ppb, ifelse(okp, d$CH4_picarro_ppb, NA))
  d$CH4_aer <- d$CH4_ppb; d$CH4_g <- shift_series(t, ifelse(okp, d$CH4_picarro_ppb, NA), L[["lag"]])
  d <- d[is.finite(d$CH4_g) & is.finite(d$CH4_aer) & is.finite(d$C2H6_ppb), ]
  d$region <- region_of(d$lat, d$lon)
  d$CH4_ppb <- d$CH4_g; d <- add_enhancements(d, "CH4_ppb", window = 300, q = 0.05, n_sigma = 3)
  d$ea <- d$CH4_aer - rolling_baseline(d$CH4_aer, 300, 0.05)
  d$ee <- d$C2H6_ppb - rolling_baseline(d$C2H6_ppb, 300, 0.05)
  d$ee3 <- sm(d$ee, 3); d$ee5 <- sm(d$ee, 5); d$ea3 <- sm(d$ea, 3)
  pl <- d$plume & d$CH4_ppb_enh > 50; idx <- which(pl); if (!length(idx)) next
  tt <- as.numeric(d$timestamp); id <- integer(nrow(d)); id[idx] <- cumsum(c(TRUE, diff(tt[idx]) > 60))
  for (g in unique(id[id > 0])) {
    s <- d[id == g, ]; ok <- is.finite(s$CH4_ppb_enh) & is.finite(s$ee) & is.finite(s$ea) & is.finite(s$ee3) & is.finite(s$ee5)
    if (sum(ok) < 10) next; s <- s[ok, ]
    yg <- york_slope(s$CH4_ppb_enh, s$ee); ya <- york_slope(s$ea, s$ee)
    res[[length(res) + 1]] <- data.frame(date = d$date[1], region = names(which.max(table(s$region))), n = nrow(s),
      york_g = yg$slope, r_g = yg$r, york_a = ya$slope, r_a = ya$r,
      york_g_e3 = york_slope(s$CH4_ppb_enh, s$ee3)$slope, r_g_e3 = york_slope(s$CH4_ppb_enh, s$ee3)$r,
      york_g_e5 = york_slope(s$CH4_ppb_enh, s$ee5)$slope,
      york_a3_e3 = york_slope(s$ea3, s$ee3)$slope,
      int_g = sum(s$ee) / sum(s$CH4_ppb_enh), int_a = sum(s$ee) / sum(s$ea), maxg = max(s$CH4_ppb_enh))
  }
}
R <- do.call(rbind, res); write.csv(R, file.path(OUT_DIR, "arc_analyzer_plumes.csv"), row.names = FALSE)
summ <- list()
for (reg in c("urban", "basin")) {
  x <- R[R$region == reg, ]
  fa <- x$r_a >= 0.7 & x$york_a >= 0.01; fg <- x$r_g >= 0.7 & x$york_g >= 0.01; fg3 <- x$r_g_e3 >= 0.7 & x$york_g_e3 >= 0.01
  cat(sprintf("%s: plumes %d | fossil-type by aeris %d med york_a %.3f int_a %.3f | by g2401 %d med york_g %.3f int_g %.3f | g2401 vs 3-s C2H6 %d med %.3f\n",
    reg, nrow(x), sum(fa), median(x$york_a[fa]), median(x$int_a[fa]), sum(fg), median(x$york_g[fg]), median(x$int_g[fg]), sum(fg3), median(x$york_g_e3[fg3])))
  both <- fa & fg
  summ[[reg]] <- data.frame(region = reg, n_plumes = nrow(x), n_fossil_both = sum(both),
    york_g_over_a = round(median(x$york_g[both] / x$york_a[both]), 3), int_g_over_a = round(median(x$int_g[both] / x$int_a[both]), 3))
  cat(sprintf("   same plumes (%d): york_g/york_a median %.3f; int_g/int_a %.3f; york_g_e3/york_a %.3f; york_a3_e3/york_a %.3f; int_g/york_g %.3f\n",
    sum(both), median(x$york_g[both]/x$york_a[both]), median(x$int_g[both]/x$int_a[both]), median(x$york_g_e3[both]/x$york_a[both]), median(x$york_a3_e3[both]/x$york_a[both]), median(x$int_g[both]/x$york_g[both])))
}
write.csv(do.call(rbind, summ), file.path(OUT_DIR, "arc_analyzer_comparison.csv"), row.names = FALSE)
