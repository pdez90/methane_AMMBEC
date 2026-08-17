#!/usr/bin/env bash
# run.sh — one command to reproduce the entire AMMBEC methane analysis.
#
#   cd /path/to/methane_AMMBEC
#   bash run.sh
#
# Everything (parsing, source apportionment, mass balance, urban budget,
# figures) runs on base R plus the ncdf4 package (for the Doppler-lidar NetCDF).
# Install ncdf4 once if needed:   Rscript -e 'install.packages("ncdf4")'
# -----------------------------------------------------------------------------
set -euo pipefail
cd "$(dirname "$0")"                 # the code/ directory

# --- point these at your data (defaults assume the standard layout) ----------
export METHANE_DATA_DIR="${METHANE_DATA_DIR:-$HOME/MethaneData}"
export METHANE_MOBILELAB_DIR="${METHANE_MOBILELAB_DIR:-$METHANE_DATA_DIR}"
export METHANE_LIDAR_DIR="${METHANE_LIDAR_DIR:-$METHANE_DATA_DIR}"   # monthly velStats_/windProf_*.nc
export METHANE_VELSTATS="${METHANE_VELSTATS:-$METHANE_DATA_DIR/velStats_202407.nc}"
export METHANE_WINDPROF="${METHANE_WINDPROF:-$METHANE_DATA_DIR/windProf_202407.nc}"
export METHANE_OUT_DIR="${METHANE_OUT_DIR:-$(dirname "$METHANE_DATA_DIR")/MethaneData_outputs}"

echo "DATA_DIR = $METHANE_DATA_DIR"
echo "OUT_DIR  = $METHANE_OUT_DIR"
echo "LIDAR    = $METHANE_LIDAR_DIR  (per-flight BLH from monthly velStats)"

Rscript run_all.R

echo
echo "Done. Outputs in: $METHANE_OUT_DIR"
echo "Key results:"
echo "  combined_flight_summary.csv   — per-flight ethane ratio + fossil fraction + flux"
echo "  urban_flux.csv / urban_legs.csv — Denver-metro urban budget"
echo "  closeloop_diagnostic.csv      — closed-loop fluxes (measured BLH) + validity"
echo "  figures/Fig1..Fig4            — the main-text figures"
