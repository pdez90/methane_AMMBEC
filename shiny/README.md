# AMMBEC Denver methane explorer (Shiny)

Interactive companion to *Airborne ethane reveals biogenic-dominated urban methane in
Denver*, published at https://01a10532-57af-fb84-da0d-c6bc3e2ba41c.share.connect.posit.cloud/
The app reads the pipeline's summary outputs from `data/` and lets a reader

- choose the ethane endmember β (with the paper's regional reference values, the
  published Front Range values, East Coast delivered gas and the assumed lean-basin
  scenario as presets), see every flight's fossil fraction move, run the two-endmember
  mixture and the emission split;
- look at each flight's track, constant-altitude transects and the per-transect ethane:methane scatter;
- see the NOAA ARL car's plume ratios and the CDPHE facility surveys;
- look at the EPA gridded GHGI and GRA2PES v2.0beta cells next to the aircraft cells (descriptive);
- play the HRRR-STILT particle clouds of any fitted urban transect backward in time (and read how STILT was run);
- inspect the HRRR-STILT footprints of the urban transects convolved with the EPA GHGI and GRA2PES v2.0beta.

Nothing is re-fitted in the browser: the fossil fraction is the within-leg York slope
divided by β, and every other number is read from the CSVs the R pipeline wrote.

## Files

```
shiny/
  app.R            UI and server
  R/helpers.R      data loading, endmember arithmetic, shared plot pieces
  data/            written by scripts/86_shiny_export.R (summary CSVs, 10-s tracks,
                   transect samples, EPA cells, binned CDPHE plume samples); ~13 MB
  manifest.json    written by rsconnect::writeManifest() for Posit Connect Cloud
```

Packages: shiny, bslib, ggplot2, DT, jsonlite (all CRAN).

## Refresh the data

After re-running the pipeline:

```
Rscript scripts/86_shiny_export.R
```

## Run locally

```
Rscript -e 'shiny::runApp("shiny")'
```

## Deploy to Posit Connect Cloud

1. On a machine with R and the packages above, write the manifest (once, and again
   whenever `app.R` or the package set changes):

   ```
   Rscript -e 'install.packages("rsconnect"); rsconnect::writeManifest("shiny")'
   ```

   and commit `shiny/manifest.json`.
2. In Connect Cloud, **Publish > Shiny (R)**, pick the `pdez90/methane_AMMBEC` repository
   and branch `main`, set the primary file to `shiny/app.R`, and publish. Connect Cloud
   installs the packages listed in the manifest and serves the app; every push to the
   branch can be republished from the content page.

## Data notes

- `basemap_*.png` / `basemap_extents.csv` (script 109) are OpenStreetMap street tiles (lightened) stored as
  images so the app needs no map service or API key; the app works without them.
- `stilt_particles.csv.gz` / `stilt_centroids.csv` (script 107) are the thinned STILT particle clouds
  behind the STILT tab's animation; `www/stilt_particles_*.gif|mp4` (script 108) are downloadable movies.

- `urban_samples.csv` / `basin_samples.csv` hold the 1 Hz samples on the constant-altitude transects (clipped by region) with the
  paper's enhancements (rolling 5th-percentile background over the flight's urban transects
  joined end to end); `gated` marks samples above the 20 ppb methane gate that enter the fits.
  `urban_samples.csv` also carries the raw CH4, C2H6 and CO and the along-track CH4 background, which
  the Background tab uses to draw the background under the chosen setting.
- `background_grid_sensitivity.csv` / `background_grid_summary.csv` (script 104), `background_aloft_sensitivity.csv`
  (script 100) and `background_mode_sensitivity.csv` (script 85) hold the per-flight fits behind the Background tab.
- `tracks.csv` is every flight subsampled to 10 s.
- `epa_cells.csv` is derived from the EPA gridded GHGI (2020 Express Extension) over the
  analysis box; `gra2pes_cells_*.csv` are the 4-km GRA2PES box cells from script 57.
  GRA2PES v2.0beta is a beta product (NOAA CSL, June 2026).
- `cdphe_hotspots.csv` bins the CDPHE survey plume samples to about 50 m (counts and the
  95th percentile of the enhancement); the raw survey files are not redistributed.
- ARL car plumes (`arc_ground_plumes.csv`) are the plume-level York fits of SI section S2.2.
