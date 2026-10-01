"""72_arc_ground_edits.py -- add the NOAA ARL Air Resources Car (ARC) ground dataset to the
manuscript: a sentence pair in the main-text ground-context paragraph, and a new SI subsection
S2.3 with the coverage and the plume ratios.

Every number is read from scripts/71_arc_ground_ethane.R's outputs; the edits are exact-anchor
replacements written in red (docx_edit_lib.py). No figure or table is inserted, so SI figure,
table and section numbering is untouched.

usage: python3 scripts/72_arc_ground_edits.py <main.docx> <si.docx> <main_out> <si_out>
           --outputs <OUT_DIR>
"""
import sys, csv, argparse
from pathlib import Path
sys.path.insert(0, str(Path(__file__).resolve().parent))
import docx_edit_lib as L
from docx_edit_lib import R, I

ap = argparse.ArgumentParser(); ap.add_argument("main_in"); ap.add_argument("si_in"); ap.add_argument("main_out"); ap.add_argument("si_out")
ap.add_argument("--outputs", required=True)
a = ap.parse_args(); OUT = Path(a.outputs); L.OUTDIR = str(OUT)

S = {r["quantity"]: r["value"] for r in csv.DictReader(open(OUT / "arc_ground_summary.csv"))}
CV = list(csv.DictReader(open(OUT / "arc_ground_coverage.csv")))
PL = list(csv.DictReader(open(OUT / "arc_ground_plumes.csv")))
BS = next(csv.DictReader(open(OUT / "basin_summary.csv")))          # aircraft basin ratio (script 31)

# Every setting quoted in the text is read back from what script 71 actually used.
W0, W1 = S["window"].split(" to ")
GATE, BGW, METRO = float(S["gate_ppb"]), float(S["bg_window_s"]), float(S["metro_lat"])
ARC_PUB = float(S["config_SOURCE_C2H6_CH4_ARC"]); AIR_BASIN = float(BS["screened_median"])
DENVER_RANGE = (0.061, 0.110)                       # the adopted Denver-relevant range (sections 3.2, S5)
def ds(d): return f"{int(d[6:8])} {'June' if d[4:6] == '06' else 'July'}"

days = sorted(c["date"] for c in CV); win = [c for c in CV if W0 <= c["date"] <= W1]
n_all, n_win = len(days), len(win)
metro_win = sorted(((int(c["n_urban_metro"]), c["date"]) for c in win if int(c["n_urban_metro"]) > 0), reverse=True)
metro_s, metro_day = metro_win[0]
others = [f"{v} on {ds(d)}" for v, d in metro_win[1:]]
other_txt = ("; " + ", ".join(others) + ", and none on the other days") if others else "; none on the other days"
urban_s = sum(int(c["n_urban"]) for c in win); basin_s = sum(int(c["n_basin"]) for c in win)
cal_all = int(S["calibration_seconds_removed"]); cal_win = sum(int(c["n_calibration_removed"]) for c in win)

def n(cls, reg): return sum(1 for p in PL if p["class"] == cls and p["region"] == reg)
u_all = sum(1 for p in PL if p["region"] == "urban"); b_all = sum(1 for p in PL if p["region"] == "basin")
u_fo, u_fr = n("fossil-type", "urban"), n("ethane-free", "urban")
b_fo, b_fr = n("fossil-type", "basin"), n("ethane-free", "basin")
u_lat_min = min(float(p["lat"]) for p in PL if p["class"] == "fossil-type" and p["region"] == "urban")
U_NORTH = f"{int(u_lat_min * 100) / 100:.2f}"
# Three decimals throughout, matching how the endmembers are quoted elsewhere (0.102, 0.0813).
def f3(v): return f"{float(v):.3f}"
def r3(v): return " to ".join(f3(x) for x in v.split(" to "))
U = f3(S["fossil_type_ratio_median_urban"]); U_CI = r3(S["fossil_type_ratio_ci_urban"])
B = f3(S["fossil_type_ratio_median_basin"]); B_CI = r3(S["fossil_type_ratio_ci_basin"])
U_IQR = r3(S["fossil_type_ratio_iqr_urban"]); B_IQR = r3(S["fossil_type_ratio_iqr_basin"])
U_WIN = f3(S["fossil_type_ratio_median_window_urban"]); B_WIN = f3(S["fossil_type_ratio_median_window_basin"])
u_fo_win = int(S["plumes_fossil_type_window_urban"]); b_fo_win = int(S["plumes_fossil_type_window_basin"])

# The interpretive sentences below are only true for some values; check them rather than assume.
assert float(B) > ARC_PUB > AIR_BASIN, "basin plume ratio no longer above the ambient ratios"
u_in_range = DENVER_RANGE[0] <= float(U) <= DENVER_RANGE[1]
u_clause = ("fall within the Denver-relevant endmember range of 0.061 to 0.110 mol mol-1 adopted here" if u_in_range
            else f"fall {'below' if float(U) < DENVER_RANGE[0] else 'above'} the Denver-relevant endmember range of 0.061 to 0.110 mol mol-1 adopted here")
print(f"ARC: {n_all} drives, {n_win} in {W0}-{W1}; metro {metro_s} s on {metro_day}{other_txt}; urban {urban_s} s, "
      f"basin {basin_s} s in window; calibration removed {cal_all} s ({cal_win} in window); box plumes {u_all} "
      f"({u_fo} ethane-bearing north of {U_NORTH}, {u_fr} ethane-free) median {U} {U_CI}; basin {b_all} ({b_fo}/{b_fr}) "
      f"median {B} {B_CI}")

# ---- main text ---------------------------------------------------------------------------
R("Because the mobile laboratory did not measure ethane, these data cannot be used for source "
  "apportionment and are included only to provide qualitative source context. Instrumentation, "
  "data merging, and record counts are described in section S2.",
  "Because the mobile laboratory did not measure ethane, these data cannot be used for source "
  "apportionment and are included only to provide qualitative source context. A second ground "
  f"platform, the NOAA ARL Air Resources Car, drove on {n_all} days between {ds(days[0])} and {ds(days[-1])} 2024, "
  f"{n_win} of them within the flight window, with an Aeris MIRA Ultra measuring CH4 and C2H6. Its coverage of "
  f"the city is limited: within the flight window only the {ds(metro_day)} drive sampled appreciably inside the "
  f"analysis box south of {METRO:.2f}°N, and on the other days the car crossed mainly the northern edge of the box "
  f"while transiting to and from the DJB. Across all its drives, ethane-bearing plumes give median ratios of {U} "
  f"mol mol-1 inside the box ({u_fo} plumes, all north of {U_NORTH}°N) and {B} mol mol-1 in the basin ({b_fo} plumes), "
  "which we use as source-composition context rather than as a constraint on the urban split. Instrumentation, "
  "data merging, and record counts for both ground platforms are described in section S2.")

# ---- SI: new S2.3 ------------------------------------------------------------------------
si = (
 f"[[H2]]S2.3 NOAA ARL Air Resources Car ({ds(days[0])} to {ds(days[-1])} 2024)\n\n"
 f"The NOAA Air Resources Laboratory operated an instrumented car (ARC) on {n_all} days between {ds(days[0])} and "
 f"{ds(days[-1])} 2024, {n_win} of them within the {ds(W0)} to {ds(W1)} flight window, carrying an Aeris MIRA Ultra "
 "(CH4 and C2H6 at 1 Hz), a Picarro G2401-m (CO2, CH4, CO, H2O), a Picarro G2201i (δ13C of CH4), NOx, O3, NH3, "
 "black-carbon and particle monitors, differential GPS and winds. Unlike the CSL mobile laboratory these data carry "
 "ethane and can therefore speak to source composition. Their coverage of the city is limited: of the drives within "
 f"the flight window, only that of {ds(metro_day)} sampled the analysis box appreciably south of {METRO:.2f}°N "
 f"({metro_s:,} one-second records{other_txt}), the latitude below which the box is metropolitan Denver rather than "
 "the Commerce City industrial corridor along its northern edge. On the other days the car crossed mainly that "
 f"northern edge while transiting to and from the DJB, so within the flight window {basin_s:,} of its one-second "
 f"records fall north of 40.05°N against {urban_s:,} inside the box.\n\n"
 "We treated each drive as the mobile analogue of a flight (script 71 of the analysis repository). Zero-air and "
 "span-gas cycles run while the car was parked were removed first, from the first cylinder sample of each parked "
 f"period to the last plus two minutes either side ({cal_all:,} one-second records over all drives, {cal_win:,} of "
 f"them within the flight window). A rolling 5th-percentile background over {BGW:.0f} s was then removed from CH4 "
 "and C2H6; plumes were flagged where the methane enhancement exceeded both three times the robust noise scale of "
 f"that day's quiet residuals and {GATE:.0f} ppb; runs separated by less than 60 s were merged and plumes with fewer "
 "than 10 usable points dropped; and one York slope of the ethane enhancement on the methane enhancement was fitted "
 "per plume. A plume in which ethane tracks methane (r at least 0.7, slope at least 0.01 mol mol-1) is called "
 "ethane-bearing; one whose methane rises with no ethane response (|r| below 0.3) is called ethane-free. Because "
 "the ethane-bearing class requires a positive, well-correlated slope, its median describes the composition of the "
 "fossil plumes the car crossed, not the share of plumes that are fossil.\n\n"
 f"Over all drives, the car crossed {u_all} plumes inside the box, of which {u_fo} are ethane-bearing, all north of "
 f"{U_NORTH}°N, with a median ethane-to-methane ratio of {U} mol mol-1 (95% confidence interval {U_CI}; "
 f"interquartile range {U_IQR}), and {u_fr} are ethane-free. In the basin {b_fo} of {b_all} plumes are "
 f"ethane-bearing, with a median of {B} mol mol-1 ({B_CI}; interquartile range {B_IQR}), and {b_fr} are "
 f"ethane-free. Restricted to the flight window the medians are {U_WIN} ({u_fo_win} plumes) and {B_WIN} "
 f"({b_fo_win} plumes) mol mol-1. A road plume is a single source sampled at close range, so these ratios describe "
 "the facilities the car drove past rather than a boundary-layer aggregate, and they are not used to constrain the "
 "urban fossil fraction. They are nevertheless consistent with the treatment of the endmember in section 3.2 and "
 f"section S5. The ethane-bearing basin plumes sit above the {ARC_PUB} mol mol-1 production-area enhancement ratio "
 f"and well above the {AIR_BASIN:.3f} mol mol-1 basin ratio measured from the aircraft, as expected if both of those "
 f"aggregate measurements are diluted by co-sampled biogenic methane, while the ethane-bearing plumes inside the box "
 f"{u_clause}. The δ13C channel is recorded on every drive, but no plume gave a Keeling intercept constrained well "
 "enough (coefficient of determination at least 0.5) on the uncalibrated delta to be reported, so the isotopic data "
 "are left to a dedicated analysis."
)
I("The transect captured frequent methane enhancements of up to ~1,161 ppb", si, "si")

L.apply(a.main_in, a.main_out, "main", "main text")
L.apply(a.si_in, a.si_out, "si", "SI")
