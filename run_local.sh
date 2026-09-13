#!/usr/bin/env bash
# run_local.sh — reproduce the analysis from the data as it sits on this machine,
# without moving or copying anything.
#
#   cd methane_AMMBEC
#   bash run_local.sh            # setup check, full pipeline, anchor verification
#
# config.R reads every path from an environment variable, so this script is the only
# place the machine-specific layout lives. The canonical layout in README.md
# (everything under ~/MethaneData) still works unchanged.
#
# Two layouts are recognised, so this keeps working before and after
# reorganize_data.sh has been run:
#
#   AFTER (by instrument)                    BEFORE (as downloaded)
#   instruments/Aircraft/                    Aircraft/
#   instruments/Lidar_Dalek2/                Dalek/
#   instruments/MobileLab/                   MobileLab/
#   instruments/CDPHE_mobile/                MethaneData/
#   inventories/EPA_GHGI/                    EPA_methane/
#   inventories/Vulcan/                      Vulcan_CO2/
#   inventories/  (NEI zip, metro outline)   EmissionsInventory/
#   outputs/                                 MethaneData_outputs/
#
# THE LIDAR MATTERS. NOAA publishes velStats_YYYYMM.nc under the same name for two
# instruments: Dalek 2 at the DSRC in Boulder, and the PUMAS truck (in the DJ Basin
# in July). The paper's 0.35-3.35 km mixing heights come from the DALEK pair; the
# PUMAS copies are kept separately and must not be mixed in. scripts/08 refuses a
# mixed pair. See REPRODUCTION_2026-09-11.md.
# -----------------------------------------------------------------------------
set -euo pipefail
cd "$(dirname "$0")"                       # the repository root (holds config.R)
BASE="${METHANE_BASE:-$(cd .. && pwd)}"; export METHANE_BASE="$BASE"    # the Methane_AMMBEC folder above it

if [ -d "$BASE/instruments" ]; then
  LAYOUT="by instrument"
  export METHANE_DATA_DIR="$BASE/instruments"
  export METHANE_LIDAR_DIR="$BASE/instruments/Lidar_Dalek2"
  export METHANE_MOBILELAB_DIR="$BASE/instruments/MobileLab"
  export METHANE_INV_DIR="$BASE/inventories"
  export METHANE_GHGI="$BASE/inventories/EPA_GHGI/Express_Extension_Gridded_GHGI_Methane_v2_2020.nc"
  export METHANE_VULCAN="$BASE/inventories/Vulcan/v4.tot.co2.usa.1km.lcc.mn.2022.tif"
  VULCAN_ZIP="$BASE/inventories/Vulcan/v4.tot.co2.usa.1km.lcc.mn.allyrs.zip"
  AIRCRAFT_DIR="$BASE/instruments/Aircraft"
  export METHANE_OUT_DIR="$BASE/outputs"
else
  LAYOUT="as downloaded"
  export METHANE_DATA_DIR="$BASE"
  export METHANE_LIDAR_DIR="$BASE/Dalek"
  export METHANE_MOBILELAB_DIR="$BASE/MobileLab"
  export METHANE_INV_DIR="$BASE/EmissionsInventory"
  export METHANE_GHGI="$BASE/EPA_methane/Express_Extension_Gridded_GHGI_Methane_v2_2020.nc"
  export METHANE_VULCAN="$BASE/Vulcan_CO2/v4.tot.co2.usa.1km.lcc.mn.2022.tif"
  VULCAN_ZIP="$BASE/Vulcan_CO2/v4.tot.co2.usa.1km.lcc.mn.allyrs.zip"
  AIRCRAFT_DIR="$BASE/Aircraft"
  export METHANE_OUT_DIR="$BASE/MethaneData_outputs"
fi
export METHANE_VELSTATS="$METHANE_LIDAR_DIR/velStats_202406.nc"
export METHANE_WINDPROF="$METHANE_LIDAR_DIR/windProf_202406.nc"
export METHANE_NEI="$METHANE_INV_DIR/2020neiMar_county_tribe_allsector.zip"

# PRINT-ENV MODE. Running a single script by hand needs the same environment the full
# pipeline sets up; exporting only METHANE_OUT_DIR leaves METHANE_DATA_DIR at config.R's
# ~/MethaneData default, so list_flights() finds nothing and scripts report "no flights"
# or "too few samples" as though the data were bad. Use:
#     eval "$(bash run_local.sh --print-env)"
# then run any script directly.
if [ "${1:-}" = "--print-env" ]; then
  for v in METHANE_BASE METHANE_DATA_DIR METHANE_LIDAR_DIR METHANE_MOBILELAB_DIR \
           METHANE_INV_DIR METHANE_GHGI METHANE_VULCAN METHANE_OUT_DIR \
           METHANE_VELSTATS METHANE_WINDPROF METHANE_NEI; do
    eval "val=\${$v:-}"
    [ -n "$val" ] && printf 'export %s=%s\n' "$v" "$(printf '%q' "$val")"
  done
  exit 0
fi

mkdir -p "$METHANE_INV_DIR" "$METHANE_OUT_DIR"

echo "Layout   = $LAYOUT"
echo "DATA_DIR = $METHANE_DATA_DIR"
echo "LIDAR    = $METHANE_LIDAR_DIR"
echo "OUT_DIR  = $METHANE_OUT_DIR"
echo

# The Vulcan GeoTIFF ships inside the all-years archive; extract the 2022 year once.
if [ ! -f "$METHANE_VULCAN" ] && [ -f "$VULCAN_ZIP" ]; then
  echo "Extracting the 2022 Vulcan GeoTIFF from the all-years archive..."
  unzip -o -j "$VULCAN_ZIP" "v4.tot.co2.usa.1km.lcc.mn.2022.tif" \
        -d "$(dirname "$METHANE_VULCAN")" >/dev/null
fi

# Duplicate downloads ("... (1).ict") would be read as extra flights and would
# double-count a day. Stop rather than silently producing a 23-flight campaign.
if ls "$AIRCRAFT_DIR"/*"(1)"*.ict >/dev/null 2>&1; then
  echo "ERROR: duplicate ICARTT files are present in $AIRCRAFT_DIR:"
  ls "$AIRCRAFT_DIR"/*"(1)"*.ict
  echo "Rename or remove them (they are byte-identical copies) and run again."
  exit 1
fi

Rscript setup.R
echo
Rscript run_all.R
echo
Rscript scripts/verify_paper_values.R
