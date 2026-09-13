#!/usr/bin/env bash
# download_gra2pes_v1.1.sh — fetch the PUBLIC GRA2PES v1.1 archives.
#
#   bash download_gra2pes_v1.1.sh --list              # what the server has, with sizes
#   bash download_gra2pes_v1.1.sh --totals            # the two total_202307 archives
#   bash download_gra2pes_v1.1.sh --sectors           # the 12 methane sectors that matter
#   bash download_gra2pes_v1.1.sh --sectors ALL       # all 18 methane sectors
#   bash download_gra2pes_v1.1.sh --sectors WASTE OG  # just these
#   MONTH=202306 bash download_gra2pes_v1.1.sh --sectors WASTE
#
# PUBLIC ONLY. v2.0beta sits behind a form (name / institution / email / intended
# use) at https://csl.noaa.gov/groups/csl4/gra2pes/datasets/beta/ and is not fetched
# here — download those by hand into the same folder and the other scripts pick them
# up. Nothing in this script needs credentials.
#
# Downloads land in inventories/GRA2PES/ (or GRA2PES/ in the pre-reorganize layout).
# Each file: resumed if interrupted (curl -C -), skipped if already complete, and tested
# with `gzip -t` afterwards — a truncated archive is the one failure mode that costs
# you an hour later, when a half-written .nc breaks a run mid-way.
# -----------------------------------------------------------------------------
set -uo pipefail
cd "$(dirname "$0")"
BASE="${METHANE_BASE:-$(cd .. && pwd)}"
if [ -d "$BASE/inventories/GRA2PES" ]; then GRA="$BASE/inventories/GRA2PES"; else GRA="$BASE/GRA2PES"; fi
MONTH="${MONTH:-202307}"
YEAR="${MONTH:0:4}"
ROOT="https://csl.noaa.gov/groups/csl4/gra2pes/datasets/data_v1.1/$YEAR"
METH="$ROOT/v1.1_methane_beta"          # per-sector METHANE archives live here
mkdir -p "$GRA"

# The twelve sectors carrying non-zero methane over the Denver box (25 Aug 2026 run).
CORE=(WASTE OG RES AG INTERNATIONAL ONROAD_GAS COMM INDF INDP OFFROAD EGU VCP)
REST=(AVIATION COOKING FUG RAIL SHIPPING ONROAD_DSL)

get () {                                  # get <url> <filename>
  local url="$1" name="$2" dest="$GRA/$2"
  if [ -f "$dest" ] && gzip -t "$dest" 2>/dev/null; then
    echo "  have   $name"; return 0
  fi
  echo "  fetch  $name"
  if ! curl -f -L --retry 5 --retry-delay 10 -C - --progress-bar -o "$dest" "$url"; then
    echo "  FAILED $name  (re-run to resume)"; return 1
  fi
  if ! gzip -t "$dest" 2>/dev/null; then
    echo "  CORRUPT after download: $name — delete it and re-run"; return 1
  fi
  echo "  ok     $name"
}

case "${1:---list}" in
  --list)
    echo "Public GRA2PES v1.1, $YEAR — totals:"
    curl -fsSL "$ROOT/" | grep -o 'href="[^"]*'"$MONTH"'[^"]*"' | cut -d'"' -f2 | sort -u | sed 's/^/  /'
    echo
    echo "Per-sector METHANE archives ($METH):"
    curl -fsSL "$METH/" \
      | sed -n 's/.*href="\([^"]*'"$MONTH"'_methane[^"]*\)".*/\1/p' | sort -u | sed 's/^/  /'
    echo
    echo "v2.0beta is form-gated: https://csl.noaa.gov/groups/csl4/gra2pes/datasets/beta/"
    ;;

  --totals)
    echo "Totals for $MONTH -> ${GRA#$BASE/}"
    get "$ROOT/GRA2PESv1.1_total_${MONTH}.tar.gz"         "GRA2PESv1.1_total_${MONTH}.tar.gz"
    get "$METH/GRA2PESv1.1_total_${MONTH}_methane.tar.gz" "GRA2PESv1.1_total_${MONTH}_methane.tar.gz"
    echo
    echo "Next:  bash run_gra2pes_anchors.sh"
    ;;

  --sectors)
    shift
    case "${1:-}" in
      "")   SECTORS=("${CORE[@]}") ;;
      ALL)  SECTORS=("${CORE[@]}" "${REST[@]}") ;;
      *)    SECTORS=("$@") ;;
    esac
    echo "Sectors for $MONTH (~3.9 GB each): ${SECTORS[*]}"
    df -h "$GRA" | tail -1
    echo
    fail=0
    for S in "${SECTORS[@]}"; do
      get "$METH/GRA2PESv1.1_${S}_${MONTH}_methane.tar.gz" \
          "GRA2PESv1.1_${S}_${MONTH}_methane.tar.gz" || fail=$((fail+1))
    done
    echo
    [ $fail -eq 0 ] && echo "All archives present and intact." \
                    || echo "$fail archive(s) failed — re-run to resume."
    echo "Then:  bash fetch_gra2pes_sectors.sh    # extracts weekdays and runs 39/40"
    echo "Download the matching GRA2PESv2.0beta_<SECTOR>_${MONTH}.tar.gz by hand"
    echo "into ${GRA#$BASE/} for the v1-vs-v2 comparison."
    ;;

  *) echo "usage: $0 [--list | --totals | --sectors [ALL|SECTOR...]]"; exit 2 ;;
esac
