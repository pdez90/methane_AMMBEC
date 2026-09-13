# 37_gra2pes_v1v2_methane_map.R ----------------------------------------------
# RUN ON YOUR OWN MACHINE (needs the gridded HC01 files, too big for the sandbox).
# Maps GRA2PES v1.1 vs v2.0beta anthropogenic methane over CONUS and their
# difference/ratio, and prints national totals, to answer: is the v2 methane
# increase national or Denver-specific?
#
# Inputs (July 2023 weekday tree for each, HC01 = Methane, mol km-2 hr-1):
#   v1.1 : ~/MethaneData/EmissionsInventory/202307        (methane-only tree)
#   v2.0 : ~/MethaneData/GRA2PES_v2/v2tree/202307
# Both are read, summed over the 20 vertical levels and averaged over the
# weekday diurnal cycle, then BINNED onto a common regular lon/lat grid so the
# two can be differenced even if their native grids differ.
#
# Run:  Rscript scripts/37_gra2pes_v1v2_methane_map.R
# Out:  <OUT_DIR>/figures/gra2pes_v1v2_methane_CONUS.png   (4 panels)
#       <OUT_DIR>/gra2pes_v1v2_national.csv
# R + ncdf4 only; uses the 'maps' package for state outlines IF installed.
# ----------------------------------------------------------------------------
suppressMessages(library(ncdf4))
proj <- if (file.exists("config.R")) "." else ".."
if (file.exists(file.path(proj, "config.R"))) source(file.path(proj, "config.R"))
OUT <- if (exists("OUT_DIR")) OUT_DIR else path.expand("~/MethaneData_outputs")
dir.create(file.path(OUT, "figures"), showWarnings = FALSE, recursive = TRUE)

HOME    <- Sys.getenv("HOME")
# Defaults resolve against config.R's INV_DIR, where run_gra2pes_anchors.sh and
# fetch_gra2pes_sectors.sh actually extract. The previous defaults (~/MethaneData/...)
# match neither layout in run_local.sh, so this script could not run without undocumented
# env vars -- the same stale-default bug fixed in scripts 16, 17 and 39.
.gra <- if (exists("INV_DIR")) file.path(INV_DIR, "GRA2PES") else file.path(HOME, "MethaneData")
.pick <- function(env, ...) {
  e <- Sys.getenv(env); if (nzchar(e)) return(e)
  for (d in c(...)) if (dir.exists(d)) return(d)
  c(...)[1]
}
V1_DIR  <- .pick("METHANE_V1_CH4", file.path(.gra, "ch4only", "202307"),
                 file.path(.gra, "sectors", "v1.1", "202307"),
                 file.path(HOME, "MethaneData", "EmissionsInventory", "202307"))
V2_DIR  <- .pick("METHANE_V2_CH4", file.path(.gra, "v2tree", "202307"),
                 file.path(.gra, "sectors", "v2.0beta", "202307"),
                 file.path(HOME, "MethaneData", "GRA2PES_v2", "v2tree", "202307"))
MW_CH4  <- 16.04
CELL_KM2 <- 16                    # 4 km GRA2PES grid
DAYTYPE <- "weekdy"               # v2tree holds weekdays only; match v1 to it

## ---- read one version: mean methane emission density over the daytype -------
read_ch4 <- function(mdir, tag) {
  d <- file.path(mdir, DAYTYPE)
  files <- list.files(d, pattern = "\\.nc$", full.names = TRUE)
  if (!length(files)) stop(tag, ": no .nc files under ", d,
                           "\n  (extract the ", DAYTYPE, " tree there first)")
  lon <- lat <- acc <- NULL; nt <- 0L
  for (fp in files) {
    nc <- nc_open(fp)
    if (!"HC01" %in% names(nc$var)) { nc_close(nc); stop(tag, ": no HC01 (Methane) in ", basename(fp)) }
    if (is.null(lon)) {
      lonn <- if ("lon" %in% names(nc$var)) "lon" else if ("XLONG" %in% names(nc$var)) "XLONG" else stop(tag, ": no lon var")
      latn <- if ("lat" %in% names(nc$var)) "lat" else if ("XLAT"  %in% names(nc$var)) "XLAT"  else stop(tag, ": no lat var")
      lon <- ncvar_get(nc, lonn); lat <- ncvar_get(nc, latn)
      if (max(lon, na.rm = TRUE) > 180) lon <- lon - 360
    }
    e <- ncvar_get(nc, "HC01"); dd <- dim(e)          # (x, y, level, time)
    if (length(dd) == 4) { es <- e[, , 1, ]; for (l in 2:dd[3]) es <- es + e[, , l, ]; e <- es }
    nth <- if (length(dim(e)) == 3) dim(e)[3] else 1L
    for (h in seq_len(nth)) {
      layer <- if (nth > 1) e[, , h] else e
      acc <- if (is.null(acc)) layer else acc + layer
      nt <- nt + 1L
    }
    nc_close(nc)
  }
  if (!identical(dim(acc), dim(lon))) {
    if (identical(dim(acc), rev(dim(lon)))) { acc <- t(acc) }   # reconcile (x,y) vs (y,x)
    else stop(tag, ": emission dims ", paste(dim(acc), collapse="x"),
              " do not match lon/lat ", paste(dim(lon), collapse="x"))
  }
  message(sprintf("  %s: %d files, %d hourly slices, grid %s", tag, length(files), nt,
                  paste(dim(acc), collapse = "x")))
  # mol km-2 hr-1  ->  kg CH4 km-2 hr-1
  list(lon = as.vector(lon), lat = as.vector(lat), dens = as.vector(acc / nt) * MW_CH4 / 1000)
}

## ---- bin scattered (lon,lat,value) onto a regular grid (mean per cell) -------
bin_grid <- function(lon, lat, val, lonb, latb) {
  nlon <- length(lonb) - 1L; nlat <- length(latb) - 1L
  ix <- findInterval(lon, lonb); iy <- findInterval(lat, latb)
  ok <- ix >= 1 & ix <= nlon & iy >= 1 & iy <= nlat & is.finite(val)
  key <- (iy[ok] - 1L) * nlon + ix[ok]
  s <- tapply(val[ok], key, mean)
  m <- matrix(NA_real_, nlon, nlat)
  m[as.integer(names(s))] <- s
  m
}

## ---- diverging / sequential ramps (inlined; no palette package needed) ------
viridis_cols <- colorRampPalette(c("#440154","#414487","#2a788e","#22a884","#7ad151","#fde725"))
rdbu_cols    <- colorRampPalette(c("#2166ac","#4393c3","#92c5de","#f7f7f7","#f4a582","#d6604d","#b2182b"))

colorbar <- function(zlim, cols, title, at = NULL, labels = NULL) {
  n <- length(cols); usr <- par("usr")
  xl <- usr[1] + 0.86*(usr[2]-usr[1]); xr <- usr[1] + 0.89*(usr[2]-usr[1])
  yb <- usr[3] + 0.10*(usr[4]-usr[3]); yt <- usr[3] + 0.45*(usr[4]-usr[3])
  ys <- seq(yb, yt, length.out = n + 1)
  for (i in seq_len(n)) rect(xl, ys[i], xr, ys[i+1], col = cols[i], border = NA, xpd = NA)
  rect(xl, yb, xr, yt, border = "#666666", xpd = NA)
  if (is.null(at)) { at <- pretty(zlim, 4); at <- at[at >= zlim[1] & at <= zlim[2]] }
  ya <- yb + (at - zlim[1])/(zlim[2]-zlim[1])*(yt - yb)
  if (is.null(labels)) labels <- format(at)
  text(xr + 0.01*(usr[2]-usr[1]), ya, labels, adj = 0, cex = 0.6, xpd = NA, col = "#333333")
  text(xl, yt + 0.03*(usr[4]-usr[3]), title, adj = 0, cex = 0.62, xpd = NA, col = "#333333", font = 2)
}

panel <- function(m, lonc, latc, zlim, cols, main, sub, has_maps) {
  image(lonc, latc, m, col = cols, zlim = zlim, xlab = "", ylab = "",
        axes = FALSE, useRaster = TRUE, asp = 1/cos(39*pi/180))
  if (has_maps) try(maps::map("state", add = TRUE, col = "#00000055", lwd = 0.4), silent = TRUE)
  box(col = "#999999")
  mtext(main, side = 3, adj = 0, line = 0.9, cex = 0.82, font = 2, col = "#1c2321")
  mtext(sub,  side = 3, adj = 0, line = 0.1, cex = 0.62, col = "#6b7671")
}

## ---- run --------------------------------------------------------------------
message("reading v1.1 ..."); v1 <- read_ch4(V1_DIR, "v1.1")
message("reading v2.0 ..."); v2 <- read_ch4(V2_DIR, "v2.0beta")

# common CONUS regular grid (~0.08 deg lon, 0.06 deg lat)
lonb <- seq(-125, -66, by = 0.08); latb <- seq(24, 50, by = 0.06)
lonc <- (head(lonb, -1) + tail(lonb, -1))/2; latc <- (head(latb, -1) + tail(latb, -1))/2
g1 <- bin_grid(v1$lon, v1$lat, v1$dens, lonb, latb)
g2 <- bin_grid(v2$lon, v2$lat, v2$dens, lonb, latb)

# national totals over CONUS (sum density * cell area); native-cell sums too
tot <- function(x) sum(x$dens, na.rm = TRUE) * CELL_KM2 / 1000    # kg/km2/hr * km2 -> t/hr
t1 <- tot(v1); t2 <- tot(v2)
message(sprintf("\nNATIONAL methane (%s, native cells): v1.1 = %.0f t/hr | v2.0beta = %.0f t/hr | ratio = %.2f",
                DAYTYPE, t1, t2, t2/t1))
write.csv(data.frame(version = c("v1.1","v2.0beta"), national_t_hr = round(c(t1,t2),1),
                     national_Gg_yr = round(c(t1,t2)*8.766,1), ratio_v2_v1 = c(NA, round(t2/t1,3))),
          file.path(OUT, "gra2pes_v1v2_national.csv"), row.names = FALSE)

has_maps <- requireNamespace("maps", quietly = TRUE)
if (!has_maps) message("  (install.packages('maps') for state outlines; plotting without them)")

# log10 density for the two magnitude panels, shared scale; diff & log2 ratio
l1 <- log10(pmax(g1, 1e-4)); l2 <- log10(pmax(g2, 1e-4))
zmag <- range(c(l1, l2), finite = TRUE)
dif <- g2 - g1; zd <- max(abs(quantile(dif, c(.01,.99), na.rm = TRUE))); zdl <- c(-zd, zd)
rat <- log2(pmax(g2,1e-4)/pmax(g1,1e-4)); zr <- max(abs(quantile(rat, c(.02,.98), na.rm=TRUE))); zrl <- c(-zr, zr)

png(file.path(OUT, "figures", "gra2pes_v1v2_methane_CONUS.png"),
    width = 2400, height = 1700, res = 200)
par(mfrow = c(2,2), mar = c(0.5,0.5,2.4,0.5), oma = c(0,0,2.2,0), family = "sans")
vc <- viridis_cols(64); dc <- rdbu_cols(64)
panel(l1, lonc, latc, zmag, vc, "v1.1 methane", "log10 kg CH4 / km2 / hr", has_maps)
colorbar(zmag, vc, "log10")
panel(l2, lonc, latc, zmag, vc, "v2.0 beta methane", "log10 kg CH4 / km2 / hr  (same scale)", has_maps)
colorbar(zmag, vc, "log10")
panel(dif, lonc, latc, zdl, dc, "v2.0 beta minus v1.1", "kg CH4 / km2 / hr  (red = v2 higher)", has_maps)
colorbar(zdl, dc, "diff")
panel(rat, lonc, latc, zrl, dc, "v2.0 beta / v1.1", "log2 ratio  (red = v2 higher)", has_maps)
colorbar(zrl, dc, "log2", at = c(-zr,0,zr), labels = sprintf("%.1fx", 2^c(-zr,0,zr)))
mtext(sprintf("GRA2PES methane, July 2023 weekday   —   national v1.1 %.0f t/hr,  v2.0beta %.0f t/hr  (%.1fx)",
              t1, t2, t2/t1), outer = TRUE, cex = 1.0, font = 2, col = "#1c2321", line = 0.3)
dev.off()
message("wrote ", file.path(OUT, "figures", "gra2pes_v1v2_methane_CONUS.png"))
