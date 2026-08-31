# 39_gra2pes_sector_boxsum.R --------------------------------------------------
# Denver-metro BOX methane by GRA2PES sector, to explain what drives the box
# total (e.g. v2.0beta's ~16.5 t/hr). GRA2PES publishes per-sector gridded files
# separately from the 'total'; download the sector tarballs and extract each into
# its own subdirectory, then point this script at the parent:
#
#   ~/MethaneData/GRA2PES_v2/sectors/OG/202307/weekdy/*.nc
#   ~/MethaneData/GRA2PES_v2/sectors/RES/202307/weekdy/*.nc
#   ~/MethaneData/GRA2PES_v2/sectors/WASTE/202307/weekdy/*.nc   ... etc.
# (sector tarballs: v2.0beta_<SECTOR>_202307.tar.gz on the NOAA CSL beta server;
#  sector abbreviations OG, RES, COMM, WASTE, COALBED, AG, ONROAD_GAS ... are in
#  Table 2 of the v2 Readme.)
#
# Run:  Rscript scripts/39_gra2pes_sector_boxsum.R ~/MethaneData/GRA2PES_v2/sectors
# Out:  <OUT_DIR>/gra2pes_sector_box_methane.csv
#       <OUT_DIR>/figures/gra2pes_sector_box_methane.png
# Uses the SAME cell-area guard as scripts 18/36. R + ncdf4 only.
# ----------------------------------------------------------------------------
suppressMessages(library(ncdf4))
proj <- if (file.exists("config.R")) "." else ".."
if (file.exists(file.path(proj, "config.R"))) source(file.path(proj, "config.R"))
OUT <- if (exists("OUT_DIR")) OUT_DIR else path.expand("~/MethaneData_outputs")
dir.create(file.path(OUT, "figures"), showWarnings = FALSE, recursive = TRUE)
BOX <- if (exists("URBAN_BOX")) URBAN_BOX else
  list(lat_s = 39.50, lat_n = 39.95, lon_w = -105.20, lon_e = -104.55)

args    <- commandArgs(trailingOnly = TRUE)
PARENT  <- if (length(args) >= 1) args[1] else path.expand("~/MethaneData/GRA2PES_v2/sectors")
MONTH   <- if (length(args) >= 2) args[2] else "202307"
DAYTYPE <- if (length(args) >= 3) args[3] else "weekdy"
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

MW_CH4  <- 16.04; NOMINAL_KM <- 4
`%||%` <- function(a,b) if (is.null(a)||length(a)==0||(length(a)==1&&is.na(a))) b else a

# --- cell area: validate the nominal 4 km grid (identical guard to scripts 18/36)
.cell_km2 <- function(nc, lat, lon) {
  ax <- ncatt_get(nc,0,"DX"); ay <- ncatt_get(nc,0,"DY")
  if (isTRUE(ax$hasatt)&&isTRUE(ay$hasatt)&&is.finite(ax$value)&&is.finite(ay$value)&&ax$value>0&&ay$value>0)
    return(ax$value*ay$value/1e6)
  hav <- function(lo1,la1,lo2,la2){R<-6371.0088;p<-pi/180
    a<-sin((la2-la1)*p/2)^2+cos(la1*p)*cos(la2*p)*sin((lo2-lo1)*p/2)^2; 2*R*asin(pmin(1,sqrt(a)))}
  i<-max(1,floor(nrow(lat)/2)); j<-max(1,floor(ncol(lat)/2))
  dxk<-hav(lon[i,j],lat[i,j],lon[i+1,j],lat[i+1,j]); dyk<-hav(lon[i,j],lat[i,j],lon[i,j+1],lat[i,j+1])
  dxk*dyk
}
.set_cell <- function(nc, lat, lon) {
  km2 <- .cell_km2(nc, lat, lon); edge <- sqrt(km2)
  if (!is.finite(km2)||km2<=0||abs(edge-NOMINAL_KM)>0.2)
    stop("cell edge ", signif(edge,4), " km, not ~", NOMINAL_KM, " km; refusing (totals scale with cell area).")
  NOMINAL_KM^2                                   # exact projected 4 km cell
}

# --- box methane (t/hr) for one extracted sector month-tree -------------------
box_ch4 <- function(mdir) {
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
      stopifnot(all(dim(layer)==dim(box_mask)))
      tot<-tot+sum(layer[box_mask],na.rm=TRUE)*cell; nt<-nt+1L}
    nc_close(nc)
  }
  (tot/nt)*MW_CH4/1e6                              # mol/hr -> t/hr
}

# --- loop sectors -------------------------------------------------------------
sect_dirs <- list.dirs(PARENT, recursive = FALSE)
sect_dirs <- sect_dirs[dir.exists(file.path(sect_dirs, MONTH, DAYTYPE))]
if (!length(sect_dirs)) stop("no sector subdirs with ", MONTH, "/", DAYTYPE, " under ", PARENT,
                             "\n  extract each v2.0beta_<SECTOR>_", MONTH, ".tar.gz into its own subfolder.")
res <- data.frame(sector = basename(sect_dirs), box_t_hr = NA_real_, stringsAsFactors = FALSE)
for (i in seq_along(sect_dirs)) {
  res$box_t_hr[i] <- tryCatch(box_ch4(file.path(sect_dirs[i], MONTH)),
                              error = function(e){message("  ", basename(sect_dirs[i]), ": ", conditionMessage(e)); NA_real_})
  message(sprintf("  %-12s %6.3f t/hr", res$sector[i], res$box_t_hr[i]))
}
res <- res[order(-res$box_t_hr), ]
res$pct <- round(100*res$box_t_hr/sum(res$box_t_hr, na.rm=TRUE), 1)
cat(sprintf("\nBox methane by sector (%s %s): total %.2f t/hr\n", MONTH, DAYTYPE, sum(res$box_t_hr, na.rm=TRUE)))
print(res, row.names = FALSE)
res$inventory_version <- .gra_version(file.path(sect_dirs[1], MONTH), DAYTYPE)
message("  inventory version detected: ", res$inventory_version[1])
write.csv(res, file.path(OUT, "gra2pes_sector_box_methane.csv"), row.names = FALSE)

pr <- res[is.finite(res$box_t_hr) & res$box_t_hr > 0, ]; pr <- pr[order(pr$box_t_hr), ]
png(file.path(OUT, "figures", "gra2pes_sector_box_methane.png"), width = 1500, height = 1100, res = 200)
par(mar = c(5,7.5,4,6), family = "sans")
bp <- barplot(pr$box_t_hr, horiz = TRUE, names.arg = pr$sector, las = 1, border = NA,
              col = "#3a7d7b", xlim = c(0, max(pr$box_t_hr)*1.15), cex.names = 0.9, col.axis = "#6b7671")
text(pr$box_t_hr + max(pr$box_t_hr)*0.015, bp, sprintf("%.2f", pr$box_t_hr), adj = 0, cex = 0.8, xpd = NA, col = "#1c2321")
title(main = "Denver-box methane by GRA2PES sector", adj = 0, cex.main = 1.25, col.main = "#1c2321", font.main = 2, line = 1.6)
mtext(sprintf("%s %s  ·  t CH4 / hr  ·  box total %.1f t/hr", MONTH, DAYTYPE, sum(res$box_t_hr, na.rm=TRUE)),
      side = 3, adj = 0, line = 0.4, cex = 0.85, col = "#6b7671")
dev.off()
cat("wrote", file.path(OUT, "figures", "gra2pes_sector_box_methane.png"), "\n")
