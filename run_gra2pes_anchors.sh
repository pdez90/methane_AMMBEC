#!/usr/bin/env bash
# run_gra2pes_anchors.sh — extract the GRA2PES v1.1 July 2023 archives and re-derive
# the three inventory anchors that config.R carries by hand.
#
#   cd ~/Downloads/Methane_AMMBEC/methane_AMMBEC
#   bash run_gra2pes_anchors.sh            # extract (if needed) and run 18 + 36
#   bash run_gra2pes_anchors.sh --clean    # afterwards: delete the extracted trees
#
# Expected results, from the 25 Aug 2026 run:
#   E_CO_DENVER           121.5 Gg CO/yr      script 18, all-species tree, CO
#   E_CH4_GRA2PES         1.691 t CH4/hr      script 18, methane-only tree, CH4
#   box/county CO ratio   0.7789 -> 227.1     script 36, all-species tree, CO
#
# TWO ARCHIVES, TWO DIRECTORIES. The all-species and methane-only archives both
# expand to 202307/{weekdy,satdy,sundy}/ with IDENTICAL member filenames, so untarring
# one over the other silently overwrites it. They go in separate directories here.
#
# ALL THREE DAY TYPES. Script 18 weights weekday, Saturday and Sunday files by their
# day counts and simply skips a day type whose folder is missing — so a weekday-only
# extraction runs happily and returns a weekday-only number, NOT the published 121.5.
# That is why this script extracts the archives whole.
#
# DISK. The all-species archive is 21 GB compressed and expands to substantially more.
# Check free space before starting; --clean removes the extracted trees (not the
# archives) when you are done.
# -----------------------------------------------------------------------------
set -euo pipefail
cd "$(dirname "$0")"
BASE="${METHANE_BASE:-$(cd .. && pwd)}"
if [ -d "$BASE/inventories/GRA2PES" ]; then GRA="$BASE/inventories/GRA2PES"; else GRA="$BASE/GRA2PES"; fi
MONTH="${MONTH:-202307}"
ALLSPEC="$GRA/allspec"; CH4ONLY="$GRA/ch4only"

if [ "${1:-}" = "--clean" ]; then
  echo "Removing extracted trees (archives are kept):"
  echo "  $ALLSPEC"; echo "  $CH4ONLY"
  rm -rf "$ALLSPEC" "$CH4ONLY"
  echo "done."; exit 0
fi

# run_local.sh sets every path config.R reads; reuse it rather than duplicating.
export METHANE_BASE="$BASE"
if [ -d "$BASE/instruments" ]; then
  export METHANE_DATA_DIR="$BASE/instruments" METHANE_INV_DIR="$BASE/inventories" \
         METHANE_OUT_DIR="$BASE/outputs"
else
  export METHANE_DATA_DIR="$BASE" METHANE_INV_DIR="$BASE/EmissionsInventory" \
         METHANE_OUT_DIR="$BASE/MethaneData_outputs"
fi
mkdir -p "$METHANE_OUT_DIR"

echo "GRA2PES dir: $GRA"
df -h "$GRA" | tail -1
echo

# Every .nc must actually open. An extraction interrupted by a full disk leaves a
# truncated member behind, and ncdf4 then fails mid-run with "NetCDF: HDF error"
# after the script has already spent minutes reading the good files.
verify_tree () {                  # verify_tree <dir>  -> prints bad files, returns count
  local dir="$1"
  Rscript -e '
    suppressMessages(library(ncdf4))
    f <- list.files(commandArgs(TRUE)[1], pattern="[.]nc$", recursive=TRUE, full.names=TRUE)
    bad <- character(0)
    for (p in f) {
      ok <- tryCatch({ nc <- nc_open(p); nc_close(nc); TRUE }, error=function(e) FALSE)
      if (!ok) bad <- c(bad, p)
    }
    cat(sprintf("  %d file(s) checked, %d unreadable\n", length(f), length(bad)))
    for (b in bad) cat("  BAD  ", b, "\n")
    quit(status = if (length(bad)) 1 else 0)
  ' "$dir"
}

# A GUI unarchiver (The Unarchiver, Archive Utility) wraps the contents in a folder
# named after the archive, so the month tree ends up one level deeper than tar puts it.
# Flatten that, rather than telling the user their extraction was wrong.
normalise () {                    # normalise <destination>
  local dest="$1"
  [ -d "$dest/$MONTH" ] && return 0
  local inner
  inner="$(find "$dest" -mindepth 2 -maxdepth 3 -type d -name "$MONTH" 2>/dev/null | head -1)"
  [ -n "$inner" ] || return 0
  echo "  found the month tree one level down; moving $inner -> $dest/$MONTH"
  mv "$inner" "$dest/$MONTH"
}

extract () {                      # extract <archive> <destination>
  local tar="$1" dest="$2"
  [ -f "$tar" ] || { echo "MISSING archive: $tar"; return 1; }
  normalise "$dest"
  if [ -d "$dest/$MONTH" ] && [ -n "$(ls -A "$dest/$MONTH" 2>/dev/null)" ]; then
    echo "already extracted: ${dest#$BASE/}/$MONTH — checking the files open"
    if verify_tree "$dest"; then return 0; fi
    echo "  re-extracting over the damaged files (a previous extraction was cut short)"
  else
    echo "extracting $(basename "$tar") -> ${dest#$BASE/}/  (this takes a while)"
  fi
  mkdir -p "$dest"
  tar xzf "$tar" -C "$dest"
  if ! verify_tree "$dest"; then
    echo
    echo "STILL UNREADABLE after re-extracting. The archive itself is probably"
    echo "incomplete (a download cut short). Check it, then download it again:"
    echo "  gzip -t \"$tar\"       # silence means the archive is intact"
    return 1
  fi
  ls "$dest/$MONTH"
}

extract "$GRA/GRA2PESv1.1_total_${MONTH}_methane.tar.gz" "$CH4ONLY"
extract "$GRA/GRA2PESv1.1_total_${MONTH}.tar.gz"         "$ALLSPEC"
echo

echo "== script 18: box CH4 from the methane-only tree (expect 1.691 t/hr) =="
Rscript scripts/18_gra2pes_boxsum_LOCAL.R "$CH4ONLY/$MONTH" CH4
echo
echo "== script 18: box CO from the all-species tree (expect 121.5 Gg/yr) =="
Rscript scripts/18_gra2pes_boxsum_LOCAL.R "$ALLSPEC/$MONTH" CO
echo

OUTLINE="$METHANE_INV_DIR/denver7_metro_outline.csv"
if [ -f "$OUTLINE" ]; then
  echo "== script 36: box/county CO ratio (expect 0.7789 -> E_CO_NEI_BOX 227.1) =="
  Rscript scripts/36_gra2pes_county_downscale.R "$ALLSPEC/$MONTH" CO
else
  echo "SKIPPING script 36: $OUTLINE not found."
  echo "  Rscript scripts/make_metro_outline.R   # needs tigris + sf, writes that file"
fi

cat <<EOF

Outputs in $METHANE_OUT_DIR:
  gra2pes_CH4_${MONTH}_boxsum.csv, gra2pes_CO_${MONTH}_boxsum.csv
  gra2pes_CO_${MONTH}_v1.1_county_box.csv   (script 36)

Compare against config.R: E_CH4_GRA2PES 1.69, E_CO_DENVER 121.5,
GRA2PES_BOX_OVER_COUNTY_CO 0.7789, E_CO_NEI_BOX 227.1.
When you are done:  bash run_gra2pes_anchors.sh --clean
EOF
