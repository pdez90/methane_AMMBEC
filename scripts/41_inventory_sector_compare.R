# 41_inventory_sector_compare.R -----------------------------------------------
# Denver-box methane by sector across three inventories -- EPA gridded GHGI,
# GRA2PES v1.1 and GRA2PES v2.0beta -- plus box totals against this study's
# airborne range. EVERY number is read from a pipeline output; nothing hard-coded.
#
# Inputs (all produced by the pipeline):
#   <OUT_DIR>/inventory_comparison.csv        <- script 14 (EPA gridded GHGI sectors)
#   <OUT_DIR>/gra2pes_sector_v1_vs_v2.csv     <- script 40 (v1.1 AND v2 sectors)   [preferred]
#     ... or, if that is absent, v2 only:
#   <OUT_DIR>/gra2pes_sector_box_methane.csv  <- script 39 (v2 sectors)
#   <OUT_DIR>/paper_values.json               <- script 21 (airborne gra_lo/hi/median)
#   config.R : E_CH4_GRA2PES                  <- GRA2PES v1.1 box total
#
# Run:  Rscript scripts/41_inventory_sector_compare.R
# Out:  <OUT_DIR>/figures/inventory_sector_compare.png
#       <OUT_DIR>/inventory_sector_compare.csv
# Base R only.
# ----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
OUT <- OUT_DIR
dir.create(file.path(OUT, "figures"), showWarnings = FALSE, recursive = TRUE)
need <- function(p) { if (!file.exists(p)) stop("missing input: ", p,
  "\n  run the pipeline stage that writes it first."); p }

## ---- EPA gridded GHGI sectors ------------------------------------------------
epa <- read.csv(need(file.path(OUT, "inventory_comparison.csv")), stringsAsFactors = FALSE)
stopifnot(all(c("sector","t_hr") %in% names(epa)))

## ---- GRA2PES sectors: prefer the v1-vs-v2 table, else v2 only ----------------
f_both <- file.path(OUT, "gra2pes_sector_v1_vs_v2.csv")
f_v2   <- file.path(OUT, "gra2pes_sector_box_methane.csv")
HAVE_V1 <- file.exists(f_both)
if (HAVE_V1) {
  g <- read.csv(f_both, stringsAsFactors = FALSE)
  stopifnot(all(c("sector","v1_t_hr","v2_t_hr") %in% names(g)))
} else {
  g <- read.csv(need(f_v2), stringsAsFactors = FALSE)
  stopifnot(all(c("sector","box_t_hr") %in% names(g)))
  g$v1_t_hr <- NA_real_; g$v2_t_hr <- g$box_t_hr
  message("note: gra2pes_sector_v1_vs_v2.csv not found; plotting EPA vs v2 only.")
}

## ---- airborne range ----------------------------------------------------------
raw <- paste(readLines(need(file.path(OUT, "paper_values.json")), warn = FALSE), collapse = " ")
jnum <- function(k) { m <- regmatches(raw, regexpr(sprintf('"%s"\\s*:\\s*-?[0-9.]+', k), raw))
  if (!length(m)) NA_real_ else as.numeric(sub('.*:\\s*', '', m)) }
air_lo <- jnum("gra_lo"); air_hi <- jnum("gra_hi"); air_med <- jnum("gra_median")
if (!all(is.finite(c(air_lo, air_hi, air_med)))) stop("could not read gra_lo/hi/median")

## ---- map both naming schemes onto common classes ------------------------------
classify_epa <- function(s)
  ifelse(grepl("Landfill|Wastewater|Composting", s), "waste",
  ifelse(grepl("PostMeter", s), "postmeter",
  ifelse(grepl("Natural_Gas|Petroleum|Abandoned_Oil|Coal", s), "og",
  ifelse(grepl("Enteric|Manure|Field_Burning|Rice", s), "ag", "other"))))
classify_gra <- function(s) { s <- toupper(s)
  ifelse(s == "WASTE", "waste",
  ifelse(s == "RES", "postmeter",
  ifelse(s %in% c("OG","COALBED"), "og",
  ifelse(s == "AG", "ag", "other")))) }
CLASSES <- c("waste","og","postmeter","ag","other")
agg <- function(v, cls) sapply(CLASSES, function(k) sum(v[cls == k], na.rm = TRUE))
gc_ <- classify_gra(g$sector)
E <- agg(epa$t_hr, classify_epa(epa$sector))
V1 <- agg(g$v1_t_hr, gc_)
V2 <- agg(g$v2_t_hr, gc_)

epa_tot <- sum(epa$t_hr, na.rm = TRUE)
v1_tot  <- if (HAVE_V1) sum(g$v1_t_hr, na.rm = TRUE) else E_CH4_GRA2PES
v2_tot  <- sum(g$v2_t_hr, na.rm = TRUE)
stopifnot(abs(sum(E) - epa_tot) < 1e-6, abs(sum(V2) - v2_tot) < 1e-6)
if (HAVE_V1) {
  stopifnot(abs(sum(V1) - v1_tot) < 1e-6)
  if (abs(v1_tot - E_CH4_GRA2PES)/E_CH4_GRA2PES > 0.05)
    warning(sprintf("v1.1 sector sum %.3f differs from config E_CH4_GRA2PES %.3f by >5%%",
                    v1_tot, E_CH4_GRA2PES))
}

cmp <- data.frame(class = CLASSES, epa_t_hr = round(as.numeric(E),4),
                  v1_t_hr = round(as.numeric(V1),4), v2_t_hr = round(as.numeric(V2),4))
cmp$v2_over_epa <- round(cmp$v2_t_hr/cmp$epa_t_hr, 2)
cmp$v2_minus_v1 <- round(cmp$v2_t_hr - cmp$v1_t_hr, 4)
print(cmp, row.names = FALSE)
cat(sprintf("\ntotals: EPA %.2f | GRA2PES v1.1 %.2f | GRA2PES v2.0beta %.2f t/hr\n", epa_tot, v1_tot, v2_tot))
cat(sprintf("airborne: %.1f to %.1f t/hr, median %.1f\n", air_lo, air_hi, air_med))
write.csv(cmp, file.path(OUT, "inventory_sector_compare.csv"), row.names = FALSE)

## ---- figure -------------------------------------------------------------------
labs <- c("Waste","Oil & gas","Post-meter","Agriculture","Other")
sub2 <- c("landfills, wastewater","production + pipelines","residential","enteric, manure","combustion, industry")
M <- if (HAVE_V1) rbind(as.numeric(E), as.numeric(V1), as.numeric(V2)) else rbind(as.numeric(E), as.numeric(V2))
epaC<-"#4a7fb5"; v1C<-"#8c9a94"; v2C<-"#c1553b"; ink<-"#1c2321"; muted<-"#6b7671"; airC<-"#2a9d8f"
cols <- if (HAVE_V1) c(epaC,v1C,v2C) else c(epaC,v2C)
nm   <- if (HAVE_V1) c("EPA gridded GHGI","GRA2PES v1.1","GRA2PES v2.0 beta") else c("EPA gridded GHGI","GRA2PES v2.0 beta")
ymax <- max(M)*1.15

png(file.path(OUT, "figures", "inventory_sector_compare.png"), width = 2100, height = 1400, res = 200)
layout(matrix(c(1,2), nrow = 1), widths = c(2.25, 1.05))
par(mar = c(8,5.2,6,1), family = "sans")
bp <- barplot(M, beside = TRUE, col = cols, border = NA, ylim = c(0, ymax),
              names.arg = rep("",5), axes = FALSE, space = c(0.15,0.9))
axis(2, las = 1, col = muted, col.axis = muted, cex.axis = 0.85)
mtext("t CH4 / hr", side = 2, line = 3.2, cex = 0.9, col = muted)
grpx <- colMeans(bp)
text(grpx, -ymax*0.034, labs, xpd = NA, cex = 0.86, col = ink, adj = c(0.5,1), font = 2)
text(grpx, -ymax*0.093, sub2, xpd = NA, cex = 0.66, col = muted, adj = c(0.5,1))
for (i in seq_len(nrow(M))) for (j in seq_len(5)) if (M[i,j] > ymax*0.012)
  text(bp[i,j], M[i,j] + ymax*0.020, sprintf("%.2f", M[i,j]), cex = 0.66, col = cols[i], font = 2, xpd = NA)
rat <- M[nrow(M),]/M[1,]
for (j in seq_len(5)) {
  lab <- if (is.finite(rat[j]) && rat[j] >= 1) sprintf("%.0fx", rat[j]) else sprintf("%.1fx", rat[j])
  text(grpx[j], ymax*0.975, lab, cex = 0.88, font = 2, xpd = NA,
       col = if (is.finite(rat[j]) && rat[j] >= 2) v2C else muted)
}
text(grpx[1], ymax*1.022, "v2 / EPA", cex = 0.6, col = muted, xpd = NA)
title(main = "Denver-box methane by sector", adj = 0, cex.main = 1.28, col.main = ink, font.main = 2, line = 4.0)
mtext("three inventories, identical analysis box, July 2023", side = 3, adj = 0, line = 2.6, cex = 0.86, col = muted)
legend(x = grpx[2]*1.02, y = ymax*0.90, nm, fill = cols, border = NA, bty = "n", cex = 0.85, text.col = ink)
if (HAVE_V1) mtext(sprintf("v1.1 carried NO waste methane over this box (0.00); v2's %.1f t/hr is entirely new", M[3,1]),
      side = 1, line = 6.3, adj = 0, cex = 0.8, col = v2C, font = 2)

par(mar = c(8,4.8,6,2.5))
tot <- c(epa_tot, v1_tot, v2_tot); tnm <- c("EPA\nGHGI","GRA2PES\nv1.1","GRA2PES\nv2.0 beta")
tmax <- max(tot, air_hi)*1.15
bp2 <- barplot(tot, col = c(epaC,v1C,v2C), border = NA, ylim = c(0,tmax), axes = FALSE, names.arg = rep("",3), space = 0.7)
rect(par("usr")[1], air_lo, par("usr")[2], air_hi, col = paste0(airC,"22"), border = NA)
abline(h = air_med, col = airC, lwd = 2, lty = 2)
barplot(tot, col = c(epaC,v1C,v2C), border = NA, add = TRUE, axes = FALSE, names.arg = rep("",3), space = 0.7)
axis(2, las = 1, col = muted, col.axis = muted, cex.axis = 0.85)
text(bp2, tot + tmax*0.030, sprintf("%.1f", tot), cex = 0.86, font = 2, col = ink, xpd = NA)
text(bp2, -tmax*0.030, tnm, xpd = NA, cex = 0.72, col = ink, adj = c(0.5,1))
text(par("usr")[1]+0.12, air_hi + tmax*0.036, sprintf("airborne %.1f - %.1f", air_lo, air_hi), adj = 0, cex = 0.72, col = airC, font = 2)
text(par("usr")[1]+0.12, air_med + tmax*0.036, sprintf("median %.1f", air_med), adj = 0, cex = 0.68, col = airC)
title(main = "Box totals", adj = 0, cex.main = 1.1, col.main = ink, font.main = 2, line = 4.0)
mtext("green band = airborne range", side = 3, adj = 0, line = 2.6, cex = 0.74, col = muted)
dev.off()
cat("wrote", file.path(OUT, "figures", "inventory_sector_compare.png"), "\n")
