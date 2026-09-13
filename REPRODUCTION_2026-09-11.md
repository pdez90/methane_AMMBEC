# Reproduction run, 11 September 2026

Full `run_all.R` from the raw data recovered after the file loss, against commit
21292e1. R 4.3.3, base R plus terra / ncdf4 / sf / png. Inputs: the 22 Twin Otter
ARL-Suite ICARTT files, the two monthly Doppler-lidar velStats files, the Mobile Lab
files for 2024-07-09, the gridded EPA GHGI methane v2 (2020) and the Vulcan v4.0
2022 GeoTIFF. `bash run_local.sh` reproduces it from the current folder layout.

## RESOLVED (12 Sep): every manuscript number reproduces, mixing heights included

With `velStats_202406.nc` AND `velStats_202407.nc` both taken from **Dalek 2** (the
stationary DSRC lidar, in `Dalek/`), `paper_values.json` is **byte-identical to the
committed copy** — blh_valid_lo/hi back to 0.352 / 3.352 km, qc_max_delta back to 12 —
and `verify_paper_values.R` prints ALL CHECKS PASSED. The earlier discrepancy was
entirely the mixed-instrument pair described below. `run_local.sh` now points at
`Dalek/`; the PUMAS copies are kept in `Pumas/` and must not be mixed in.

Note the Dalek files are the full months (1 June - 31 July, 141 gates, BLH up to
6.3 / 6.5 km) where the PUMAS pair covers only 18-30 June and 1-13 July with 112 gates.

## Result: every manuscript number reproduces except the mixing heights (superseded)

`scripts/verify_paper_values.R` passes all 14 anchors, both provenance flags and the
Vulcan cross-check. These outputs are **byte-identical** to the committed `results/`:

`table1.csv`, `emission_estimates.csv`, `ratio_method_flux.csv`, `urban_flux.csv`,
`beta_sensitivity.csv`, `beta_breakeven.csv`, `beta_endmember_ARC.csv`,
`beta_endmember_pipeline.csv`, `basin_*.csv`, `source_attribution*.csv`,
`inventory_comparison.csv`, `biogenic_grid.csv`, `fossil_signature_grid.csv`,
`facility_separation_scan.csv`, `facility_sector_fits.csv`,
`regression_comparison.csv`, `enh_threshold_*.csv`, `background_scatter.csv`,
`fig3_leg_structure.csv`, `flight_ethane_methane.csv`, `wind_fossil.csv`,
`vulcan_co2_boxsum.csv`, `toc_graphic_values.csv`,
`mobilelab_20240709_hotspots.csv`.

So the fossil fractions (0 to 57%, median 24%), the emission estimates
(4.6 to 10.7 t/hr, median 7.6; Vulcan 4.1; NEI box 8.6 to 20.0), the endmember sweep
and break-even 0.049, the 0.063 to 0.0813 implied range, and the 92% delivered-gas
case are all regenerated from raw data.

## The one discrepancy: boundary-layer height

`paper_values.json` differs in exactly three fields, all from the Doppler lidar:

| field | committed | this run |
|---|---|---|
| `blh_valid_lo` | 0.352 | 0.590 |
| `blh_valid_hi` | 3.352 | 1.401 |
| `qc_max_delta` | 12 | 2 |

The reason is the velStats files themselves, not the code. The files on disk now are
a different distribution from the ones the paper was built on:

- **Different file layout.** The originals carried `yDay` / `year` / `latitude` /
  `longitude`; these carry a `time` dimension in seconds plus `lat` / `lon`.
  `R/lidar_blh.R` now reads both (see below), so this alone is not the issue.
- **Different sites.** `velStats_202406.nc` is at 39.991 N, -105.264 (NOAA Boulder,
  matching the TOPAZ surface file) and `velStats_202407.nc` is at 40.183 N, -104.726
  (the DJB, ~50 km north-east). The campaign is 13 of 15 flight days in July, so
  almost every flight's mixing height now comes from the basin lidar rather than a
  Denver-side one.
- **Different range.** These files top out at 2.897 km (June) and 1.837 km (July), so
  the published 3.352 km upper bound cannot come from them at any threshold.

`qc_max_delta` follows from the same thing: the in-PBL data-selection filter uses the
retrieved mixing height, so a different BLH gives a different QC sensitivity. It is a
robustness statistic, not an input to any reported emission or fossil fraction, which
is why every other output is unchanged.

**Cause found (11 Sep, later).** NOAA CSL publishes `velStats_YYYYMM.nc` under the
same filename for two different Doppler lidars:

- **Dalek 2**, stationary at the DSRC in Boulder, 39.991 N -105.264:
  `csl.noaa.gov/groups/csl3/measurements/dsrc/dalek02/plots/monthlyNetcdf/`
- **PUMAS**, the truck-mounted MicroDop parked in the DJ Basin, 40.183 N -104.726:
  `csl.noaa.gov/groups/csl3/measurements/2024ammbec/pumas/plots/monthlyNetcdf/`

The June file on disk is Dalek 2 and the July file is PUMAS, so the campaign series
concatenated two instruments 50 km apart. `scripts/08_lidar_blh.R` now compares the
files' coordinates and stops if they are more than 5 km apart
(`METHANE_ALLOW_MIXED_LIDAR=1` overrides). With the current pair it stops, so BLH is
absent until one instrument's June AND July files are downloaded.

PUMAS also publishes `BLHStats_YYYYMM.nc`, a boundary-layer-height product, rather
than the wVar-threshold retrieval this code performs; worth comparing before
restating section 2.4. The Twin Otter's own scanning Doppler lidar (Baidar, Brewer)
would give along-track BLH and is not in the Aircraft folder at all.

## Code changes made during this run

1. **`run_all.R` no longer stops silently.** Eight stage scripts end with
   `quit(save = "no")` when an optional input is missing. `base::quit()` kills the
   whole Rscript process, so the first such stage ended the run with exit status 0
   and no message: with the CDPHE surveys absent, stage 26 quit and stages 28, 29,
   30, 31, 27, 42, 32, 33, 34 and 35 never ran, while the log still looked clean.
   `run_all.R` now shadows `quit()` with a condition it catches, so the stage stops,
   the run continues, and the summary lists which stages exited early.
2. **`R/lidar_blh.R` reads both velStats layouts.** `yDay`/`year` as before, or a
   `time` dimension in seconds. The seconds are taken from the start of the file's
   own `year` variable, because the units string on these files reads "Seconds since
   1 Jan 2020" on files whose `year` is 2024 and whose values are less than one year
   of seconds; a hard check stops the read if the reconstructed dates do not land in
   that year. Gates with NaN heights are dropped (138 of 250 in these files).
3. **`REPRODUCIBILITY.md`** anchor table now says 23,622.3 Gg over 2,856 cells, not
   the superseded 23,478 over 2,755.
4. **`run_local.sh`** added: runs setup, pipeline and verification against the
   current folder layout, extracts the Vulcan GeoTIFF if only the archive is there,
   and refuses to run if duplicate `... (1).ict` downloads are in `Aircraft/`.

## CDPHE mobile surveys: recovered, and now processed as the toxics paper does

The 173 CAT survey CSVs were recovered (169 enter the manifest, exactly the file set
the committed results used). They arrive from the same Picarro G2204 and inlet as the
mobile air-toxics measurements, and the toxics manuscript's sections 2.1.1 / S1
establish two corrections that this pipeline was not applying:

1. **Inlet delay**, measured by CDPHE: 21 s on the CAT lab, 17 s on the EMU lab. A
   reading must be attributed to the position where the air entered the inlet, which
   at survey speed is 150-230 m back along the road.
2. **Native-cadence averaging**: the Picarro acquires about every 5 s, but CDPHE
   delivers on a 1-s grid by carrying the last reading forward. In the first survey
   file, 77% of consecutive delivered 1-s values are repeats. Averaging each 5-s
   acquisition block (over seconds that carry a value, no gap filling) stops those
   repeats being counted as independent measurements.

Measurements within 100 m of the ATOPs depot (garage air) are also dropped.
`R/read_mobile.R` now does all three, driven by constants in `config.R`
(`MOBILE_DELAY_S`, `MOBILE_CADENCE_S`, `MOBILE_GARAGE*`); the delivered signal is kept
as `CH4_ppmv_raw` for any plume-shape work, exactly as the toxics analysis keeps the
un-averaged H2S. `METHANE_MOBILE_RAW=1` restores the old uncorrected behaviour.

What changes (committed -> corrected):

| quantity | committed | corrected |
|---|---|---|
| CollinsAerospace trend | -0.0082 ppmv/yr, p = 0.370 | -0.0073, p = 0.417 |
| HEPTerminal trend | +0.0079 ppmv/yr, p = 0.471 | +0.0084, p = 0.439 |
| SuncorP66 trend | +0.0078 ppmv/yr, p = 0.306 | +0.0054, p = 0.467 |
| survey rows (first file) | 16,174 | 15,290 |
| peak enhancements | higher | lower (5-s averaging flattens single-second spikes) |

The manuscript's mobile sentence ("|slope| about 0.008 ppmv/yr, p = 0.31 to 0.47")
becomes |slope| 0.005 to 0.008 ppmv/yr, p = 0.42 to 0.47. No trend is significant
either way, so the conclusion stands, but the quoted numbers change, as do Figure S2
and the per-survey maxima. Nothing in the aircraft analysis is affected.

## Stages that could not run

| Stage | Missing input |
|---|---|
| Fig 1 locator inset | `EmissionsInventory/denver7_metro_outline.csv` — `Rscript scripts/make_metro_outline.R` (needs `tigris` and internet) |
| 17 (NEI anchor 291.5) | 2020 NEI county zip |
| 18, 36 (E_CO_DENVER 121.5, box/county ratio 0.7789 -> 227.1) | `GRA2PESv1.1_total_202307.tar.gz`, the ALL-SPECIES archive. The methane-only v1.1 archives are on disk; the all-species one is not. Cross-check: the v1.0 CO GeoTIFF for July 2021 gives 121.9 Gg/yr over the same box against config's 121.5 (0.3%). |
| 37, 39, 40 (v1/v2 sector table) | per-sector archives `GRA2PESv1.1_<SECTOR>_202307_methane.tar.gz` and `v2.0beta_<SECTOR>_202307.tar.gz`. Not needed for the manuscript numbers: their results are in `config.R` and are gated by `verify_paper_values.R` |

`manifest_flights.csv` also differs from the committed copy: the recovered Aircraft
folder has the `R1` revisions of eleven AircraftData files where the original had
`R0`, adds the CU-Radiometer files, and only the 22 ARL-Suite files were used here.
None of those files feed any reported number — scripts 15 and 19 read ARL-Suite only.
