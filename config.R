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
# SINGLE OUTPUT DIRECTORY. When METHANE_OUT_DIR is not set, prefer a sibling outputs/
# next to the repository if one exists. Without this, scripts that had their own
# fallback (18/39/40/43/45/46) wrote to <project>/../outputs while those that did not
# (16/17/36/37/38/41) wrote to ~/MethaneData_outputs, so the anchors ended up split
# across two directories and scripts/45 could never see all of them at once. Resolving
# it here means every script that sources config.R agrees, and no script needs its own
# copy of the rule. Both candidates are relative to the working directory, so this holds
# whether a script is run from the repository root or from scripts/.
if (!nzchar(Sys.getenv("METHANE_OUT_DIR"))) {
  for (.cand in c(file.path(getwd(), "..", "outputs"),
                  file.path(getwd(), "..", "..", "outputs"))) {
    if (dir.exists(.cand)) { OUT_DIR <- normalizePath(.cand); break }
  }
  rm(.cand)
}

# A missing data root is the single most common cause of a script reporting "no flights"
# or "too few samples" as though the DATA were at fault. Say what actually happened, and
# where the environment comes from, rather than letting the default fail quietly.
if (!dir.exists(DATA_DIR))
  message("\n*** METHANE_DATA_DIR does not exist: ", DATA_DIR,
          "\n    Nothing that reads flight data will find anything.",
          "\n    Running one script by hand? Set the environment first:",
          "\n      eval \"$(bash run_local.sh --print-env)\"\n")

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
#   E_CO2_DENVER = 23,622 Gg CO2/yr  -> Vulcan v4.0 fossil-fuel CO2 (2022), summed
#     over the EXACT box (2,856 1-km cells; tC->CO2 x44.01/12). Anchors CH4:CO2.
#     Reproduce: scripts/16_vulcan_co2_boxsum.R.
#   E_CO_DENVER  = 121.5 Gg CO/yr    -> GRA2PES v1.1 'total' CO (Jul 2023) summed
#     over the EXACT box (171 4-km cells; 13.85 t/hr). BOX-CONSISTENT => the
#     primary CH4:CO anchor. Reproduce: scripts/18_gra2pes_boxsum_LOCAL.R 202307 CO.
#   E_CO_NEI     = 292.7 Gg CO/yr    -> EPA 2020 NEI 7-county sum (script 17). The
#     county footprint (~11,800 km2) is ~4x the box, so it OVER-scales box CO; kept
#     only as an upper-bound sensitivity case.
#   E_CH4_GRA2PES = 1.69 t/hr (14.8 Gg/yr) -> GRA2PES 'total' CH4 (=variable HC01)
#     box sum (script 18); a box-consistent BOTTOM-UP methane inventory to compare.
E_CO_DENVER   <- as.numeric(Sys.getenv("METHANE_E_CO",  unset = 121.5))  # Gg CO/yr  (GRA2PES box, primary)
# 292.7 Gg CO/yr: re-derived 12 Sep 2026 by scripts/17 from the EPA file
# 2020neiMar_county_tribe_allsector.zip as posted on that date (322,612 short tons
# over the seven counties x 0.90718474). The 291.5 used through Aug 2026 came from an
# earlier posting of the same file; EPA re-posts these summaries, so the anchor is
# tied to the retrieval date, not just the file name. Difference 0.4%.
E_CO_NEI      <- 292.7                                                    # Gg CO/yr  (NEI 7-county, sensitivity)
# Box-consistent NEI CO anchor. The seven-county NEI total redistributed onto the
# analysis box using the GRA2PES box/county CO ratio, both summed over identical
# footprints by scripts/36_gra2pes_county_downscale.R (July 2023, 171 box cells,
# 705 county cells, 16 km2 nominal Lambert cells):
#   E_CO_NEI_BOX = E_CO_NEI * GRA2PES_BOX_OVER_COUNTY_CO  (DERIVED below, not typed)
# Provenance: MethaneData_outputs/gra2pes_CO_202307_county_box.csv
# NOTE the box holds 78% of the seven-county CO on 24% of the area, so the
# footprint correction is only 1.28x. The residual 1.87x between this and
# E_CO_DENVER is a genuine NEI-vs-GRA2PES difference at identical box scale,
# NOT a footprint artifact.
GRA2PES_BOX_OVER_COUNTY_CO <- 0.7789                                      # scripts/36
# DERIVED, so it can never drift from its two inputs. It was a transcribed constant
# (227.1) until 12 Sep 2026; with E_CO_NEI = 292.7 it is 228.0.
E_CO_NEI_BOX  <- round(E_CO_NEI * GRA2PES_BOX_OVER_COUNTY_CO, 1)          # Gg CO/yr  (NEI downscaled to the box)
E_CO2_DENVER  <- as.numeric(Sys.getenv("METHANE_E_CO2", unset = 23622.3))  # Gg CO2/yr (Vulcan box)
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
# PRIMARY: 0.102 mol/mol, the lowest of the four published Colorado Front Range
# delta-C2H6/delta-CH4 ratios compiled in Table 2 of Kille et al. (2019, GRL),
# "Literature Comparison of Tracer Ratios to CH4". That table lists, in MOLE
# fraction: 10.2 +/- 0.2% (Gilman 2017), 16.1 +/- 2.1% (Kille et al. 2019),
# 17% (Tzompa-Sosa et al. 2017) and 18.7 +/- 3.2% (Fried et al. 2015).
#
# We adopt the LOWEST of the four deliberately, because by (a) a lower endmember
# yields a HIGHER fossil fraction, so the adopted value works against this
# analysis's biogenic-dominated conclusion rather than for it. Adopting Kille's
# own 16.1% instead would scale every fossil fraction reported here by
# 0.102/0.161 = 0.63, so roughly a third lower.
#
# UNITS. These are MOLE ratios. Gas composition is often quoted as a WEIGHT
# ratio, and the two differ by MW_CH4/MW_C2H6 = 16.04/30.07 = 0.533, so a 10%
# weight ratio is only 5.3% by mole. The two must not be mixed; a weight-ratio
# value used here unconverted would nearly double every fossil fraction.
#
# UPSTREAM vs DOWNSTREAM. The endmember must describe the gas that LEAKS, which
# escapes upstream at wellheads, separators and tanks, before processing.
# Statewide gas-plant composition (CDPHE data, quoted in the 2021 DJB airborne
# report as ~9.9% by WEIGHT, i.e. ~5.3% by mole) describes gas AFTER ethane has
# been stripped for sale, so it is ethane-poor by construction. The downstream
# and upstream numbers are not in conflict; they are different points in the
# supply chain, and a leak-based analysis needs the upstream one. The same
# reasoning is why pipeline-gas ratios from other basins do not transfer: the
# L.A. Basin pipeline value of 1.65-2.4% (Schafer et al. 2025) is processed dry
# gas, whereas the DJB is a wet-gas play (>6% on the Yacovitch et al. 2014 scale
# quoted by Kille et al.).
#
# PROVENANCE NOTE. An earlier revision of this analysis used 0.11 without a
# traceable source. It was replaced with the citable 0.102 above; the change
# raises the reported fossil fractions by roughly three percentage points.
SOURCE_C2H6_CH4 <- 0.102

# FOSSIL-FRACTION SLOPE ESTIMATOR (Table 1, script 15's fossil_frac_york, and every
# number downstream of them: paper_values.json fossil_*, the attribution, script 27).
#   "within" - within-leg (fixed-effects) York: one York slope fitted to the points
#              after centring each urban leg on its own mean. Removes the between-leg
#              term that a pooled fit carries, which on 20240708_R0_L1 and
#              20240710_R0_L1 is negative (one leg is a landfill plume with no ethane,
#              another the industrial corridor) and flips the pooled sign. Every
#              flight gets a physical slope; nothing is clamped. Scripts 48/49.
#   "pooled" - one York slope through all gated urban points of the flight (the
#              estimator the original submission used; 2 of 7 flights clamp to 0%).
# Set METHANE_FOSSIL_ESTIMATOR=pooled to reproduce the submitted numbers. The
# verification file (scripts/verify_paper_values.R) pins expectations per estimator.
FOSSIL_ESTIMATOR <- Sys.getenv("METHANE_FOSSIL_ESTIMATOR", unset = "within")
if (!FOSSIL_ESTIMATOR %in% c("within", "pooled"))
  stop("METHANE_FOSSIL_ESTIMATOR must be 'within' or 'pooled', got: ", FOSSIL_ESTIMATOR)

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
# fraction 24% -> 30%) is emitted by the pipeline rather than computed by hand, and
# shows the biogenic-leaning median survives the whole 0.08-0.15 range.
SOURCE_C2H6_CH4_ARC <- 0.0813

# DISTRIBUTION-GAS (delivered pipeline) endmember, for the Discussion's reversal
# test only. Plant et al. (2019, GRL) SI Table S5 lists the C2H6:CH4 ratio of gas
# delivered to six East Coast cities from utility gas-quality data: Washington 3.58,
# New York 2.02, Baltimore 3.67, Philadelphia 3.02, Boston 2.04, Providence 1.87
# percent. Their mean is 2.70 percent (0.0270 mol/mol) and their range 1.87 to 3.67
# percent. These are LITERATURE values for OTHER cities, not a Denver assay, so they
# are not adopted as beta_source. Scripts 21 and 27 use them only to report what the
# campaign median fossil fraction would read if Denver's urban fossil methane were
# delivered gas of that composition, which is the reversal case the Discussion states.
SOURCE_C2H6_CH4_PIPELINE_MEAN  <- 0.0270
SOURCE_C2H6_CH4_PIPELINE_RANGE <- c(0.0187, 0.0367)

# DENVER DELIVERED GAS, measured. Xcel Energy / Public Service Company of Colorado
# "Colorado Monthly Gas Quality Report", Denver zone (volume-weighted average of all
# supplies into the zone; ASTM D3588 / GPA 2145), published for gas-transport
# customers at corporate.my.xcelenergy.com/s/gas-transport/psco (Gas Quality). The
# monthly compositions for 2018 and 2023-2025 are committed in psco_gas_quality.csv
# (c2h6_ch4_mol = ethane mol% / methane mol%). June and July 2024, the campaign
# months, read 0.1097 and 0.1105; the 2023-2025 monthly range is 0.095 to 0.141 (March 2023 to October 2025).
# Denver's delivered gas is DJB residue gas with ethane rejected into the sales
# stream, so it is ethane-RICH, unlike the delivered gas of the East Coast cities
# above. Script 52 uses this as the Denver-specific distribution endmember.
SOURCE_C2H6_CH4_DENVER_DELIVERED       <- 0.110
SOURCE_C2H6_CH4_DENVER_DELIVERED_RANGE <- c(0.095, 0.141)

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
# NB: default is the DATA_DIR root, where the monthly NetCDFs live (matching
# LIDAR_DIR above). An earlier default pointed at a lidar/ subfolder that does
# not exist, which made run_all.R silently skip script 08.
VELSTATS_FILE <- Sys.getenv("METHANE_VELSTATS",
                            unset = file.path(DATA_DIR, "velStats_202406.nc"))
WINDPROF_FILE <- Sys.getenv("METHANE_WINDPROF",
                            unset = file.path(DATA_DIR, "windProf_202406.nc"))

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

# ---- CDPHE mobile survey processing (R/read_mobile.R; scripts 03, 04, 06, 26) --
# The CDPHE surveys come from the same Picarro G2204 and inlet as the mobile
# air-toxics measurements, so they carry the same two artefacts and take the same
# two corrections, measured by CDPHE and applied in the toxics analysis:
#   1. INLET DELAY. A reading is reported after the air that produced it has
#      travelled the 3 m inlet and the analyser, so it must be attributed to the
#      position where that air entered: 21 s on the CAT lab, 17 s on the EMU lab.
#      At survey speed that is 150-230 m of road.
#   2. NATIVE CADENCE. The Picarro acquires about every 5 s; CDPHE delivers on a
#      common 1-s grid by carrying the last reading forward, so the delivered 1-s
#      series repeats values that are not independent measurements. CH4 is
#      averaged over each 5-s acquisition block, over the seconds that carry a
#      value, with no gap filling. The delivered signal is kept as CH4_ppmv_raw.
# Measurements within 100 m of the ATOPs depot are garage air, not ambient.
# Set METHANE_MOBILE_RAW=1 (or MOBILE_CORRECT <- FALSE) to read the delivered
# signal uncorrected, reproducing the pre-September-2026 behaviour of 03/04/06/26.
MOBILE_CORRECT         <- !identical(Sys.getenv("METHANE_MOBILE_RAW"), "1")
MOBILE_DELAY_S         <- c(CAT = 21, EMU = 17)   # seconds, CDPHE-measured
MOBILE_CADENCE_S       <- 5                       # Picarro G2204 acquisition cycle
MOBILE_GARAGE          <- c(lat = 39.785359, lon = -105.104331)  # ATOPs depot
MOBILE_GARAGE_RADIUS_M <- 100

# Data-selection QC (R/qc.R; scripts 02, 15), following Schafer/Peischl et al.
# (2025): daytime, in-PBL, >200 m AGL, in-box. Mild for AMMBEC (all flights
# daytime; ~4-12% of samples <200 m AGL); the largest per-flight change in the
# fossil fraction is 3 percentage points (2024-07-13 L2, within-leg York; see
# results/qc_robustness.csv), with the flight ranking preserved.
QC_ENABLE  <- TRUE
QC_AGL_MIN <- 200         # m AGL near-source floor (avoid airfield approaches)
QC_DAY     <- c(10, 17)   # local daytime hours retained (10:00-17:00)
QC_UTC_OFF <- -6          # Denver = MDT (UTC-6) during the campaign

# Reproducibility.
set.seed(42)

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(OUT_DIR, "figures"), showWarnings = FALSE, recursive = TRUE)
