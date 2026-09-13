#!/usr/bin/env bash
# fetch_gra2pes_sectors.sh — download and extract the per-sector GRA2PES archives
# that scripts/39 and scripts/40 need for the v1.1-vs-v2.0beta sector comparison.
#
#   bash fetch_gra2pes_sectors.sh              # the 12 sectors with box methane
#   bash fetch_gra2pes_sectors.sh ALL          # all 18 sectors
#   bash fetch_gra2pes_sectors.sh WASTE OG     # just these
#
# WHAT THIS DOES AND DOES NOT DO
#   v1.1 methane sector archives are public and are fetched here.
#   v2.0beta archives sit behind a form (name / institution / email / intended use)
#   at https://csl.noaa.gov/groups/csl4/gra2pes/datasets/beta/ — fill it in once in a
#   browser, download GRA2PESv2.0beta_<SECTOR>_202307.tar.gz for the same sectors into
#   $V2_TARS, and this script extracts whatever it finds there.
#
# DISK. Each v1.1 sector archive is ~3.9 GB compressed. Only the weekday tree is
# extracted (scripts 39/40 default to weekdy), which is about a third of the members,
# and each archive is deleted after extraction unless KEEP_TARS=1. Twelve sectors
# still need roughly 25-30 GB free, eighteen about 40 GB. Check before starting.
# -----------------------------------------------------------------------------
set -euo pipefail
cd "$(dirname "$0")"
BASE="${METHANE_BASE:-$(cd .. && pwd)}"
MONTH="${MONTH:-202307}"
DAYTYPE="${DAYTYPE:-weekdy}"
V1_URL="https://csl.noaa.gov/groups/csl4/gra2pes/datasets/data_v1.1/2023/v1.1_methane_beta"
# Works with either folder layout (see run_local.sh / reorganize_data.sh).
if [ -d "$BASE/inventories/GRA2PES" ]; then GRA="$BASE/inventories/GRA2PES"; else GRA="$BASE/GRA2PES"; fi
V1_DIR="$GRA/sectors/v1.1"
V2_DIR="$GRA/sectors/v2.0beta"
V2_TARS="${V2_TARS:-$GRA}"

# The twelve sectors that carried non-zero methane over the Denver box in the
# 25 Aug 2026 run. The other six (AVIATION, COOKING, FUG, RAIL, SHIPPING,
# ONROAD_DSL) are needed only to check the sector sum against the box total.
CORE=(WASTE OG RES AG INTERNATIONAL ONROAD_GAS COMM INDF INDP OFFROAD EGU VCP)
EXTRA=(AVIATION COOKING FUG RAIL SHIPPING ONROAD_DSL)

case "${1:-}" in
  "")   SECTORS=("${CORE[@]}") ;;
  ALL)  SECTORS=("${CORE[@]}" "${EXTRA[@]}") ;;
  *)    SECTORS=("$@") ;;
esac

echo "Sectors: ${SECTORS[*]}"
echo "Month:   $MONTH   day type: $DAYTYPE"
echo "v1.1 ->  $V1_DIR"
echo "v2.0 ->  $V2_DIR   (archives read from $V2_TARS)"
df -h "$BASE" | tail -1
echo

# Extract only the members matching a glob, portably. GNU tar (Linux) needs --wildcards
# to treat the pattern as a glob; bsdtar (macOS) globs by default and REJECTS that flag
# outright ("Option --wildcards is not supported"), so the flag has to be chosen at
# runtime rather than assumed.
tar_extract_glob() {                       # $1 archive  $2 dest dir  $3 glob
  if tar --version 2>/dev/null | grep -qi 'gnu tar'; then
    tar xzf "$1" -C "$2" --wildcards "$3"
  else
    tar xzf "$1" -C "$2" "$3"
  fi
}

for S in "${SECTORS[@]}"; do
  # ---- v1.1 (public) --------------------------------------------------------
  dest="$V1_DIR/$S"
  if [ -d "$dest/$MONTH/$DAYTYPE" ] && [ -n "$(ls -A "$dest/$MONTH/$DAYTYPE" 2>/dev/null)" ]; then
    echo "[$S] v1.1 already extracted"
  else
    tar_name="GRA2PESv1.1_${S}_${MONTH}_methane.tar.gz"
    mkdir -p "$dest"
    # Use an archive already fetched by download_gra2pes_v1.1.sh if there is one,
    # and leave that copy alone; only archives downloaded here are removed after.
    tar_path="$GRA/$tar_name"; ours=0
    if [ ! -f "$tar_path" ]; then
      tar_path="$V1_DIR/$tar_name"; ours=1
      if [ ! -f "$tar_path" ]; then
        echo "[$S] downloading $tar_name (~3.9 GB)"
        curl -f -L --retry 3 -C - -o "$tar_path" "$V1_URL/$tar_name"
      fi
    fi
    echo "[$S] extracting $DAYTYPE members from $(basename "$tar_path")"
    tar_extract_glob "$tar_path" "$dest" "*${DAYTYPE}*"
    [ "${KEEP_TARS:-0}" = "1" ] || [ $ours -eq 0 ] || rm -f "$tar_path"
  fi

  # ---- v2.0beta (form-gated; extracted if the archive is already downloaded) --
  v2dest="$V2_DIR/$S"
  v2tar="$V2_TARS/GRA2PESv2.0beta_${S}_${MONTH}.tar.gz"
  if [ -d "$v2dest/$MONTH/$DAYTYPE" ] && [ -n "$(ls -A "$v2dest/$MONTH/$DAYTYPE" 2>/dev/null)" ]; then
    echo "[$S] v2.0beta already extracted"
  elif [ -f "$v2tar" ]; then
    mkdir -p "$v2dest"
    echo "[$S] extracting v2.0beta $DAYTYPE members"
    tar_extract_glob "$v2tar" "$v2dest" "*${DAYTYPE}*"
  else
    echo "[$S] v2.0beta archive not found at $v2tar — download it from the beta page"
  fi
done

cat <<EOF

Extraction done. Now, from the repository root:

  Rscript scripts/39_gra2pes_sector_boxsum.R "$V1_DIR" $MONTH $DAYTYPE
  Rscript scripts/39_gra2pes_sector_boxsum.R "$V2_DIR" $MONTH $DAYTYPE
  Rscript scripts/40_gra2pes_sector_v1v2.R  "$V1_DIR" "$V2_DIR" $MONTH $DAYTYPE

Expected from the 25 Aug 2026 run: v1.1 sector sum 1.694 t CH4/hr (vs 1.691 box total),
v2.0beta 16.510, with WASTE 0.000 -> 12.198 the whole story, and INDF, INDP, OFFROAD,
EGU and VCP identical between versions.
EOF
