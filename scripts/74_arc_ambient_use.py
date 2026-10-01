"""74_arc_ambient_use.py -- use the ARC ground record as a measurement rather than as context.

Two things the files give that the manuscript previously took on trust or did without:
  1. The published 0.0813 mol/mol production-area ratio is reproduced here from the ARC files
     themselves, by the same gated enhancement-ratio construction the aircraft uses.
  2. The ground ambient ratio inside the analysis box can be compared with the airborne urban
     slopes -- two platforms, different sampling geometries, same air mass -- which is an
     independent ground check on the central measurement of the paper.

Numbers come from scripts/71_arc_ground_ethane.R (arc_ground_ambient.csv, arc_ground_summary.csv)
and from the aircraft pipeline (table1.csv). Edits are exact-anchor and red.

usage: python3 scripts/74_arc_ambient_use.py <main.docx> <si.docx> <main_out> <si_out>
           --outputs <OUT_DIR> --aircraft <AIRCRAFT_OUT_DIR>
"""
import sys, csv, re, math, argparse, statistics
from pathlib import Path
sys.path.insert(0, str(Path(__file__).resolve().parent))
import docx_edit_lib as L
from docx_edit_lib import R

ap = argparse.ArgumentParser(); ap.add_argument("main_in"); ap.add_argument("si_in"); ap.add_argument("main_out"); ap.add_argument("si_out")
ap.add_argument("--outputs", required=True); ap.add_argument("--aircraft")
a = ap.parse_args(); OUT = Path(a.outputs); AIR = Path(a.aircraft or a.outputs); L.OUTDIR = str(OUT)

S = {r["quantity"]: r["value"] for r in csv.DictReader(open(OUT / "arc_ground_summary.csv"))}
W0, W1 = S["window"].split(" to ")                  # the flight window script 71 used
AM = [r for r in csv.DictReader(open(OUT / "arc_ground_ambient.csv")) if W0 <= r["date"] <= W1]
def f3(v): return f"{float(v):.3f}"
def vals(reg): return sorted(float(r["slope"]) for r in AM if r["region"] == reg)
u, b = vals("urban"), vals("basin")
U_MED, U_LO, U_HI = f3(statistics.median(u)), f3(min(u)), f3(max(u))
B_MED, B_LO, B_HI = f3(statistics.median(b)), f3(min(b)), f3(max(b))
ARC_PUB = S["config_SOURCE_C2H6_CH4_ARC"]; BETA = float(S["config_SOURCE_C2H6_CH4"])

# Airborne urban slopes, read directly (rebuilding them from the rounded fossil per cent in
# table1.csv moved the median by 0.001).
air = sorted(float(r["ethane_slope"]) for r in csv.DictReader(open(AIR / "table1_full.csv"))
             if r["ethane_slope"] not in ("", "NA"))
A_MED, A_LO, A_HI = f3(statistics.median(air)), f3(min(air)), f3(max(air))
# The two comparative claims in the text are checked here, not assumed.
rel = abs(float(B_MED) - float(ARC_PUB)) / float(ARC_PUB)
assert rel <= 0.15, f"basin ambient {B_MED} is {rel:.0%} from the published {ARC_PUB}; rewrite the 'reproduces' sentence"
REPRO = f"reproduces to within {math.ceil(rel * 100 / 5) * 5}%"
assert abs(statistics.median(u) - statistics.median(air)) <= 0.01 and min(u) <= max(air) and min(air) <= max(u), \
    "ground and airborne urban ratios no longer agree; rewrite the comparison sentences"
print(f"ambient ground: urban {U_MED} ({U_LO}-{U_HI}, n={len(u)} days), basin {B_MED} ({B_LO}-{B_HI}, n={len(b)}); "
      f"published production-area {ARC_PUB}; airborne urban {A_MED} ({A_LO}-{A_HI}, n={len(air)} flights)")

# ---- SI S2.3: the ambient paragraph, after the plume-class results -------------------------
R("A road plume is a single source sampled at close range, so these ratios describe the facilities "
  "the car drove past rather than a boundary-layer aggregate, and they are not used to constrain the "
  "urban fossil fraction.",
  "Applying instead the gated enhancement-ratio construction used for the flights, that is, fitting every "
  f"sample whose methane enhancement exceeds {float(S['ambient_gate_ppb']):.0f} ppb rather than resolving plumes, gives an ambient ground "
  f"ratio of {B_MED} mol mol-1 in the basin as a median over days ({B_LO} to {B_HI}). That {REPRO}, from "
  f"the campaign's own ground files, the {ARC_PUB} mol mol-1 production-area ratio this work takes from the "
  "AMMBEC report to CDPHE, and it falls below the plume-resolved value above because an ambient fit averages "
  f"the ethane-bearing and ethane-free sources the car passed. Inside the analysis box the same construction "
  f"gives {U_MED} mol mol-1 ({U_LO} to {U_HI} across {len(u)} days), against {A_MED} mol mol-1 ({A_LO} to {A_HI}) "
  "for the airborne urban slopes of the same weeks: a vehicle at street level and an aircraft in the mixed "
  "layer, with different sampling geometries and footprints, return urban enhancement ratios of similar "
  "central value and spread, and both lie far below every fossil endmember considered. A road plume is a "
  "single source sampled at close range, so the plume-resolved ratios describe the facilities the car drove "
  "past rather than a boundary-layer aggregate, and neither set is used to compute the urban fossil fraction.", "si")

# ---- main text 4.1: the ground check, where the urban slopes are first reported --------------
R("The campaign-level source-mix inference is therefore based on the distribution of flight-level slopes, "
  "their median, and th",
  f"A ground check supports the airborne slopes: applying the same gated construction to the NOAA ARL car's "
  f"1 Hz record inside the box gives an ambient ratio of {U_MED} mol mol-1 as a median over {len(u)} days "
  f"({U_LO} to {U_HI}), against {A_MED} mol mol-1 ({A_LO} to {A_HI}) for the flights, so a street-level platform "
  "and the aircraft return the same urban enhancement ratio within their day-to-day spread (section S2.3). "
  "The campaign-level source-mix inference is therefore based on the distribution of flight-level slopes, "
  "their median, and th")

# ---- main text, Limitation 1: the lower bound is now reproduced here -------------------------
R("ground-level production-area ratio measured during this campaign (0.0813; section S5). Both are ambient "
  "enhancement ratios, diluted by whatever biogenic methane was co-sampled,",
  "ground-level production-area ratio measured during this campaign (0.0813; section S5), which the same "
  f"construction applied to the ground car's own files reproduces at {B_MED} mol mol-1 as a median over days "
  f"({B_LO} to {B_HI}; section S2.3). Both are ambient enhancement ratios, diluted by whatever biogenic "
  "methane was co-sampled,")

L.apply(a.main_in, a.main_out, "main", "main text")
L.apply(a.si_in, a.si_out, "si", "SI")
