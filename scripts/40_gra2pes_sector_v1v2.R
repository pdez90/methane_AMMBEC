# 40_gra2pes_sector_v1v2.R ----------------------------------------------------
# Compare Denver-box methane BY SECTOR between GRA2PES v1.1 and v2.0beta, to show
# what the v2 update moved. Point it at two parent dirs, each holding extracted
# per-sector month-trees (<parent>/<SECTOR>/<MONTH>/<daytype>/*.nc):
#
#   v1.1 sector methane tarballs : GRA2PESv1.1_<SECTOR>_<YYYYMM>_methane.tar.gz
#   v2.0 sector tarballs         : GRA2PESv2.0beta_<SECTOR>_<YYYYMM>.tar.gz
# Sectors present in only one version are reported (NA in the other) — that is
# expected: v2 ADDS post-meter (RES), pipeline (OG), COALBED, rebuilt WASTE.
#
# Run:  Rscript scripts/40_gra2pes_sector_v1v2.R <v1_parent> <v2_parent> [MONTH] [daytype]
# Out:  <OUT_DIR>/gra2pes_sector_v1_vs_v2.csv
#       <OUT_DIR>/figures/gra2pes_sector_v1_vs_v2.png   (dumbbell: v1 -> v2 per sector)
# Uses the same validated cell/box core as scripts 18/36/39. R + ncdf4 only.
# ----------------------------------------------------------------------------
suppressMessages(library(ncdf4))
proj <- if (file.exists("config.R")) "." else ".."
if (file.exists(file.path(proj, "config.R"))) source(file.path(proj, "config.R"))
OUT <- if (exists("OUT_DIR")) OUT_DIR else path.expand("~/MethaneData_outputs")
dir.create(file.path(OUT, "figures"), showWarnings = FALSE, recursive = TRUE)
BOX <- if (exists("URBAN_BOX")) URBAN_BOX else
  list(lat_s = 39.50, lat_n = 39.95, lon_w = -105.20, lon_e = -104.55)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) stop("usage: Rscript 40_gra2pes_sector_v1v2.R <v1_parent> <v2_parent> [MONTH] [daytype]")
V1P <- args[1]; V2P <- args[2]
MONTH   <- if (length(args) >= 3) args[3] else "202307"
DAYTYPE <- if (length(args) >= 4) args[4] else "weekdy"
# INVENTORY VERSION TAG. GRA2PES filenames carry the version, e.g.
# GRA2PESv1.1_total_202307_weekdy_00to11Z.nc or GRA2PESv2.0beta_WASTE_...
# Output files and tables are tagged with it so a v2 run can never silently
# overwrite a v1 result, and so every CSV records which inventory produced it.
.gra_version <- function(mdir, daytype = "weekdy") {
  f <- list.files(file.path(mdir, daytype), pattern = "\\.nc$")
  if (!length(f)) return("unknown")
  m <- regmatches(f[1], regexpr("GRA2PES(v[0-9][0-9.]*[A-Za-z]*)", f[1]))
  if (!length(m)) return("unknown")
  sub("^GRA2PES", "", m)
}

MW_CH4 <- 16.04; NOMINAL_KM <- 4

.cell_km2 <- function(nc, lat, lon) {
  ax <- ncatt_get(nc,0,"DX"); ay <- ncatt_get(nc,0,"DY")
  if (isTRUE(ax$hasatt)&&isTRUE(ay$hasatt)&&is.finite(ax$value)&&is.finite(ay$value)&&ax$value>0&&ay$value>0)
    return(ax$value*ay$value/1e6)
  hav <- function(lo1,la1,lo2,la2){R<-6371.0088;p<-pi/180
    a<-sin((la2-la1)*p/2)^2+cos(la1*p)*cos(la2*p)*sin((lo2-lo1)*p/2)^2; 2*R*asin(pmin(1,sqrt(a)))}
  i<-max(1,floor(nrow(lat)/2)); j<-max(1,floor(ncol(lat)/2))
  hav(lon[i,j],lat[i,j],lon[i+1,j],lat[i+1,j])*hav(lon[i,j],lat[i,j],lon[i,j+1],lat[i,j+1])
}
.set_cell <- function(nc, lat, lon) {
  km2 <- .cell_km2(nc, lat, lon); edge <- sqrt(km2)
  if (!is.finite(km2)||km2<=0||abs(edge-NOMINAL_KM)>0.2)
    stop("cell edge ", signif(edge,4), " km, not ~", NOMINAL_KM, " km; refusing.")
  NOMINAL_KM^2
}
box_ch4 <- function(mdir) {          # t/hr methane over the box for one sector tree
  files <- list.files(file.path(mdir, DAYTYPE), pattern="\\.nc$", full.names=TRUE)
  if (!length(files)) return(NA_real_)
  box_mask<-NULL; cell<-NA; tot<-0; nt<-0L
  for (fp in files) {
    nc<-nc_open(fp)
    if (!"HC01"%in%names(nc$var)) { nc_close(nc); stop("no HC01 in ", basename(fp)) }
    if (is.null(box_mask)) {
      lon<-ncvar_get(nc, if("lon"%in%names(nc$var))"lon" else "XLONG")
      lat<-ncvar_get(nc, if("lat"%in%names(nc$var))"lat" else "XLAT")
      if (max(lon,na.rm=TRUE)>180) lon<-lon-360
      box_mask<-lat>=BOX$lat_s&lat<=BOX$lat_n&lon>=BOX$lon_w&lon<=BOX$lon_e
      stopifnot(sum(box_mask,na.rm=TRUE)>0); cell<-.set_cell(nc,lat,lon)
    }
    e<-ncvar_get(nc,"HC01"); dd<-dim(e)
    if (length(dd)==4){es<-e[,,1,];for(l in 2:dd[3])es<-es+e[,,l,];e<-es}
    nth<-if(length(dim(e))==3)dim(e)[3] else 1L
    for (h in seq_len(nth)){layer<-if(nth>1)e[,,h] else e
      stopifnot(all(dim(layer)==dim(box_mask))); tot<-tot+sum(layer[box_mask],na.rm=TRUE)*cell; nt<-nt+1L}
    nc_close(nc)
  }
  (tot/nt)*MW_CH4/1e6
}
sector_table <- function(parent, tag) {
  ds <- list.dirs(parent, recursive = FALSE)
  ds <- ds[dir.exists(file.path(ds, MONTH, DAYTYPE))]
  if (!length(ds)) stop(tag, ": no sector subdirs with ", MONTH, "/", DAYTYPE, " under ", parent)
  v <- setNames(rep(NA_real_, length(ds)), toupper(basename(ds)))
  for (i in seq_along(ds)) {
    v[i] <- tryCatch(box_ch4(file.path(ds[i], MONTH)),
                     error=function(e){message("  ", tag, " ", basename(ds[i]), ": ", conditionMessage(e)); NA_real_})
    message(sprintf("  %-5s %-12s %7.3f t/hr", tag, names(v)[i], v[i]))
  }
  v
}

message("v1.1 sectors:"); a <- sector_table(V1P, "v1.1")
message("v2.0 sectors:"); b <- sector_table(V2P, "v2.0")

secs <- sort(union(names(a), names(b)))
cmp <- data.frame(sector = secs,
                  v1_t_hr = as.numeric(a[secs]),
                  v2_t_hr = as.numeric(b[secs]), stringsAsFactors = FALSE)
cmp$v1_t_hr[is.na(cmp$v1_t_hr)] <- 0; cmp$v2_t_hr[is.na(cmp$v2_t_hr)] <- 0
cmp$change <- cmp$v2_t_hr - cmp$v1_t_hr
cmp <- cmp[order(-cmp$v2_t_hr), ]
cat(sprintf("\nDenver-box methane by sector (%s %s)\n  v1.1 total %.2f t/hr  ->  v2.0 total %.2f t/hr\n",
            MONTH, DAYTYPE, sum(cmp$v1_t_hr), sum(cmp$v2_t_hr)))
print(cmp, row.names = FALSE, digits = 3)
.d1 <- list.dirs(V1P, recursive = FALSE); .d1 <- .d1[dir.exists(file.path(.d1, MONTH, DAYTYPE))]
.d2 <- list.dirs(V2P, recursive = FALSE); .d2 <- .d2[dir.exists(file.path(.d2, MONTH, DAYTYPE))]
cmp$v1_version <- .gra_version(file.path(.d1[1], MONTH), DAYTYPE)
cmp$v2_version <- .gra_version(file.path(.d2[1], MONTH), DAYTYPE)
message("  versions detected: v1 = ", cmp$v1_version[1], " | v2 = ", cmp$v2_version[1])
if (identical(cmp$v1_version[1], cmp$v2_version[1]))
  warning("both parents resolve to the SAME inventory version (", cmp$v1_version[1],
          "); check the two paths point at different versions.")
write.csv(cmp, file.path(OUT, "gra2pes_sector_v1_vs_v2.csv"), row.names = FALSE)

# --- dumbbell: each sector a line from v1 to v2, red = up, blue = down ---------
pr <- cmp[cmp$v1_t_hr > 0 | cmp$v2_t_hr > 0, ]; pr <- pr[order(pr$v2_t_hr), ]
up <- "#b2182b"; dn <- "#2166ac"; ink <- "#1c2321"; muted <- "#6b7671"
png(file.path(OUT, "figures", "gra2pes_sector_v1_vs_v2.png"), width = 1600, height = 1150, res = 200)
par(mar = c(5,8,4.5,7), family = "sans")
xmax <- max(pr$v1_t_hr, pr$v2_t_hr)*1.12
plot(NA, xlim = c(0, xmax), ylim = c(0.5, nrow(pr)+0.5), axes = FALSE, xlab = "", ylab = "")
abline(v = axTicks(1), col = "#0000000d")
for (i in seq_len(nrow(pr))) {
  col <- if (pr$v2_t_hr[i] >= pr$v1_t_hr[i]) up else dn
  segments(pr$v1_t_hr[i], i, pr$v2_t_hr[i], i, col = col, lwd = 2.4)
  points(pr$v1_t_hr[i], i, pch = 21, bg = "white", col = muted, cex = 1.1)
  points(pr$v2_t_hr[i], i, pch = 19, col = col, cex = 1.25)
}
axis(2, at = seq_len(nrow(pr)), labels = pr$sector, las = 1, tick = FALSE, col.axis = ink, cex.axis = 0.9)
axis(1, col = muted, col.axis = muted, cex.axis = 0.85)
mtext("t CH4 / hr", side = 1, line = 2.4, cex = 0.9, col = muted)
title(main = "Denver-box methane by sector: v1.1 to v2.0 beta", adj = 0, cex.main = 1.25, col.main = ink, font.main = 2, line = 2.2)
mtext(sprintf("%s %s  ·  open = v1.1, filled = v2.0  ·  red = increase, blue = decrease", MONTH, DAYTYPE),
      side = 3, adj = 0, line = 0.9, cex = 0.82, col = muted)
mtext(sprintf("box total  v1.1 %.1f  ->  v2.0 %.1f t/hr", sum(cmp$v1_t_hr), sum(cmp$v2_t_hr)),
      side = 3, adj = 1, line = 0.9, cex = 0.82, col = ink, font = 2)
dev.off()
cat("wrote", file.path(OUT, "figures", "gra2pes_sector_v1_vs_v2.png"), "\n")
