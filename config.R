# config.R -------------------------------------------------------------------
# Edit these paths for your machine, then every script below is reproducible.
# -----------------------------------------------------------------------------

# Root of the data tree (contains Aircraft/ and the quarterly mobile folders).
# Override by setting the env var METHANE_DATA_DIR, else this default is used.
DATA_DIR <- path.expand(Sys.getenv("METHANE_DATA_DIR",
                                   unset = "~/MethaneData"))

# Where outputs (manifests, summaries, figures) are written.
OUT_DIR <- path.expand(Sys.getenv("METHANE_OUT_DIR",
                                  unset = file.path(dirname(DATA_DIR), "MethaneData_outputs")))

# Denver-metropolitan-area bounding box for the URBAN methane budget (script 11).
# Legs inside this box (and south of the basin threshold) are treated as urban;
# legs north of BASIN_LAT are treated as DJB basin. Adjust to taste.
URBAN_BOX <- list(lat_s = 39.50, lat_n = 39.95, lon_w = -105.20, lon_e = -104.55)
BASIN_LAT <- 40.05

# Independent Denver-metro CO and CO2 emission inventories for the enhancement-
# ratio method (script 15; after Schafer/Peischl et al. 2025). These scale the
# CH4 emission linearly, exactly as the CO inventory does in the LA study.
#
# REAL values below are DERIVED FROM DATA, not placeholders (see scripts 16-18):
#   E_CO2_DENVER = 23,478 Gg CO2/yr  -> Vulcan v4.0 fossil-fuel CO2 (2022), summed
#     over the EXACT box (2,755 1-km cells; tC->CO2 x44.01/12). Anchors CH4:CO2.
#     Reproduce: scripts/16_vulcan_co2_boxsum.R.
#   E_CO_DENVER  = 121.5 Gg CO/yr    -> GRA2PES v1.1 'total' CO (Jul 2023) summed
#     over the EXACT box (171 4-km cells; 13.85 t/hr). BOX-CONSISTENT => the
#     primary CH4:CO anchor. Reproduce: scripts/18_gra2pes_boxsum_LOCAL.R 202307 CO.
#   E_CO_NEI     = 291.5 Gg CO/yr    -> EPA 2020 NEI 7-county sum (script 17). The
#     county footprint (~11,800 km2) is ~4x the box, so it OVER-scales box CO; kept
#     only as an upper-bound sensitivity case.
#   E_CH4_GRA2PES = 1.69 t/hr (14.8 Gg/yr) -> GRA2PES 'total' CH4 (=variable HC01)
#     box sum (script 18); a box-consistent BOTTOM-UP methane inventory to compare.
E_CO_DENVER   <- as.numeric(Sys.getenv("METHANE_E_CO",  unset = 121.5))  # Gg CO/yr  (GRA2PES box, primary)
E_CO_NEI      <- 291.5                                                    # Gg CO/yr  (NEI 7-county, sensitivity)
E_CO2_DENVER  <- as.numeric(Sys.getenv("METHANE_E_CO2", unset = 23478))  # Gg CO2/yr (Vulcan box)
E_CH4_GRA2PES <- 1.69                                                     # t/hr      (GRA2PES box CH4, bottom-up)

# ---- Ethane endmember (beta_source) -----------------------------------------
# beta_source must be the C2H6:CH4 ratio of UNDILUTED source gas, because the
# two-endmember mixing model divides the observed atmospheric slope by it
# (f_fossil = beta_observed / beta_source; see R/ratios.R). Two consequences
# drive the choice below:
#   (a) f scales as 1/beta_source, so too LOW an endmember inflates the fossil
#       fraction; and
#   (b) any AMBIENT enhancement ratio is a mixture of the fossil plume with
#       co-sampled biogenic methane, so it is a lower bound on the source ratio,
#       never the source ratio itself.
#
# PRIMARY: 0.11 mol/mol, from published compositional analyses of raw
# DJB/Wattenberg natural gas (published values span roughly 0.10-0.16). We take
# the LOW end of that published range deliberately: by (a) this yields a HIGHER
# fossil fraction than the middle or top of the range, so the adopted value works
# against the manuscript's biogenic-dominated conclusion rather than for it.
SOURCE_C2H6_CH4 <- 0.11

# SENSITIVITY: 0.0813 mol/mol, measured by the NOAA Air Resources Car (ARC) in DJB
# oil-and-gas production areas during this same campaign (summer 2024; AMMBEC final
# report to CDPHE, Baidar & Brown et al., 2025).
#
# This is NOT adopted as the primary endmember, for one specific reason: it is an
# ambient delta-C2H6/delta-CH4 enhancement ratio measured in production-area air,
# not a compositional assay of the gas itself. By (b) it is therefore diluted by
# whatever biogenic methane the ARC co-sampled -- and that report's own
# apportionment puts the DJB at roughly half biogenic -- so 0.0813 is a lower bound
# on the raw source ratio, and by (a) adopting it as the divisor would bias the
# fossil fractions high. It is also fit-for-purpose in that report's own method,
# which uses the ground-to-aircraft CONTRAST (a dilution factor) rather than an
# absolute source composition, so borrowing its numerator as an absolute endmember
# would be a misuse of the measurement.
#
# What is still outstanding is therefore a direct compositional analysis of
# undiluted wellhead and distribution gas, which neither value is. Script 27
# evaluates both, so the manuscript's measured-endmember sensitivity (median fossil
# fraction 22% -> 30%) is emitted by the pipeline rather than computed by hand, and
# shows the biogenic-leaning median survives the whole 0.08-0.15 range.
SOURCE_C2H6_CH4_ARC <- 0.0813

# Folder holding the monthly Doppler-lidar NetCDFs (velStats_YYYYMM.nc,
# windProf_YYYYMM.nc). Scripts pick the file matching each flight's month, so
# multiple months (Jun-Sep 2024) can coexist and each flight uses its own BLH.
LIDAR_DIR <- Sys.getenv("METHANE_LIDAR_DIR", unset = DATA_DIR)

# Gridded U.S. EPA GHGI methane NetCDF (Maasakkers et al. v2) for the bottom-up
# inventory comparison (script 14). The 2020 Express Extension is closest to 2024.
GHGI_FILE <- Sys.getenv("METHANE_GHGI", unset = file.path(DATA_DIR, "8367082",
                        "Express_Extension_Gridded_GHGI_Methane_v2_2020.nc"))

# Ancillary data (CSL Mobile Lab ICARTT files + Doppler-lidar NetCDFs).
# Point these at wherever you downloaded them; scripts 07-08 use them.
MOBILELAB_DIR <- Sys.getenv("METHANE_MOBILELAB_DIR",
                            unset = file.path(DATA_DIR, "MobileLab"))
VELSTATS_FILE <- Sys.getenv("METHANE_VELSTATS",
                            unset = file.path(DATA_DIR, "lidar", "velStats_202406.nc"))
WINDPROF_FILE <- Sys.getenv("METHANE_WINDPROF",
                            unset = file.path(DATA_DIR, "lidar", "windProf_202406.nc"))

# Per-flight mass-balance curtain config (script 05). You fill this in with the
# downwind-screen leg_ids and the boundary-layer height per flight; script 05
# writes a blank template here on first run if it doesn't exist.
CURTAIN_CONFIG <- Sys.getenv("METHANE_CURTAIN_CONFIG",
                             unset = file.path(getwd(), "curtain_config.csv"))

# Downloaded emission-inventory files (scripts 14, 16, 17, 18, 20). Point these
# at wherever you saved them (default: a MethaneData/EmissionsInventory folder).
INV_DIR    <- Sys.getenv("METHANE_INV_DIR", unset = file.path(DATA_DIR, "EmissionsInventory"))
VULCAN_FILE<- Sys.getenv("METHANE_VULCAN", unset = file.path(INV_DIR, "v4.tot.co2.usa.1km.lcc.mn.2022.tif"))
NEI_ZIP    <- Sys.getenv("METHANE_NEI",    unset = file.path(INV_DIR, "2020neiMar_county_tribe_allsector.zip"))

# Data-selection QC (R/qc.R; scripts 02, 15), following Schafer/Peischl et al.
# (2025): daytime, in-PBL, >200 m AGL, in-box. Mild for AMMBEC (all flights
# daytime; ~4-12% of samples <200 m AGL) and moves York fossil fractions <=3
# percentage points, so results are robust to it.
QC_ENABLE  <- TRUE
QC_AGL_MIN <- 200         # m AGL near-source floor (avoid airfield approaches)
QC_DAY     <- c(10, 17)   # local daytime hours retained (10:00-17:00)
QC_UTC_OFF <- -6          # Denver = MDT (UTC-6) during the campaign

# Reproducibility.
set.seed(42)

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(OUT_DIR, "figures"), showWarnings = FALSE, recursive = TRUE)
