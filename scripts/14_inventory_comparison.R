# 14_inventory_comparison.R --------------------------------------------------
# Bottom-up inventory: sum the gridded U.S. EPA GHGI methane inventory
# (Maasakkers et al., v2 / Express Extension NetCDF) over the Denver-metro box and
# break it into fossil / biogenic / combustion sectors. This is the bottom-up
# reference compared, in the manuscript, against the top-down enhancement-ratio
# emission estimates (CH4:CO x GRA2PES and CH4:CO2 x Vulcan; scripts 15/21/23).
# No closed-loop flux is used (none is valid; see script 13).
#
# Set METHANE_GHGI to the gridded .nc file (e.g. the 2020 Express Extension,
# closest to the 2024 campaign). Requires ncdf4.
#
# Run:  Rscript scripts/14_inventory_comparison.R
# Out:  <OUT_DIR>/inventory_comparison.csv
# -----------------------------------------------------------------------------
proj <- if (file.exists("config.R")) "." else ".."
source(file.path(proj, "config.R"))
GHGI_FILE <- Sys.getenv("METHANE_GHGI", unset = file.path(DATA_DIR, "8367082",
                        "Express_Extension_Gridded_GHGI_Methane_v2_2020.nc"))
if (!requireNamespace("ncdf4", quietly = TRUE)) stop("ncdf4 required.")
if (!file.exists(GHGI_FILE)) stop("Gridded GHGI file not found: ", GHGI_FILE,
                                  " (set METHANE_GHGI).")

nc <- ncdf4::nc_open(GHGI_FILE)
lat <- ncdf4::ncvar_get(nc, "lat"); lon <- ncdf4::ncvar_get(nc, "lon")
area <- ncdf4::ncvar_get(nc, "grid_cell_area")            # cm^2  (lon,lat)
iy <- which(lat >= URBAN_BOX$lat_s & lat <= URBAN_BOX$lat_n)
ix <- which(lon >= URBAN_BOX$lon_w & lon <= URBAN_BOX$lon_e)
NA_ <- 6.022e23; MW <- 16.04                              # Avogadro, g/mol
# molec cm-2 s-1  ->  t hr-1 over the selected cells
to_thr <- function(v) {
  e <- ncdf4::ncvar_get(nc, v)                            # (lon, lat)
  sum(e[ix, iy] * area[ix, iy], na.rm = TRUE) * MW / NA_ * 3600 / 1e6
}
vars <- names(nc$var); emi <- vars[grepl("^emi_ch4", vars)]
val <- setNames(vapply(emi, to_thr, numeric(1)), sub("emi_ch4_", "", emi))
ncdf4::nc_close(nc)

grp <- function(pat) names(val)[grepl(pat, names(val))]
fossil <- grp("Natural_Gas|Petroleum|PostMeter|Coal|Abandoned_Oil")
bio    <- grp("Landfill|Wastewater|Enteric|Manure|Composting|Rice|Field_Burning")
comb   <- grp("Combustion")
F <- sum(val[fossil]); B <- sum(val[bio]); C <- sum(val[comb]); TOT <- sum(val)

out <- data.frame(sector = names(val), t_hr = round(as.numeric(val), 4),
                  group = ifelse(names(val) %in% fossil, "fossil",
                          ifelse(names(val) %in% bio, "biogenic",
                          ifelse(names(val) %in% comb, "combustion", "other"))))
out <- out[order(-out$t_hr), ]
write.csv(out, file.path(OUT_DIR, "inventory_comparison.csv"), row.names = FALSE)

message(sprintf("\n=== EPA gridded GHGI over Denver metro (%s) ===", basename(GHGI_FILE)))
message(sprintf("Total bottom-up:  %.2f t/hr", TOT))
message(sprintf("  fossil:     %.2f t/hr (%.0f%%)  [NG distribution+postmeter = %.2f]",
                F, 100*F/TOT, sum(val[grep("Distribution|PostMeter", names(val))])))
message(sprintf("  biogenic:   %.2f t/hr (%.0f%%)  [landfills+wastewater dominate]", B, 100*B/TOT))
message(sprintf("  combustion: %.2f t/hr (%.0f%%)", C, 100*C/TOT))
# Report the unmatched "other" residual explicitly, and list any such sectors, so
# nothing is silently dropped from the fossil/biogenic/combustion classification.
O <- TOT - F - B - C
oth <- out$sector[out$group == "other" & out$t_hr > 0]
message(sprintf("  other (unclassified): %.4f t/hr (%.2f%%)%s", O, 100*O/TOT,
                if (length(oth)) paste0("  <- ", paste(oth, collapse = ", ")) else ""))
if (abs(O) > 0.02 * TOT)
  warning(sprintf("Unclassified 'other' sectors carry %.1f%% of the inventory; extend the fossil/biogenic/combustion regexes.", 100*O/TOT))
message(sprintf("Bottom-up fossil fraction (fossil / [fossil+biogenic]): %.0f%%  (fossil / total = %.0f%%)",
                100*F/(F+B), 100*F/TOT))

# NB: the top-down comparison against this bottom-up total is made in the manuscript
# using the enhancement-ratio emission estimates (CH4:CO x GRA2PES, CH4:CO2 x Vulcan;
# scripts 15/21/23). No closed-loop flux is compared here, because none is valid
# (script 13 is a qualitative geometry diagnostic and reports no flux).
message("\nWrote ", file.path(OUT_DIR, "inventory_comparison.csv"))
