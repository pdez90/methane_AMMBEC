"""65_arc_ground_edits.py -- add the NOAA ARL Air Resources Car (ARC) ground dataset to the
manuscript: a sentence pair in the main-text ground-context paragraph, and a new SI subsection
S2.3 with the coverage and the plume ratios.

Every number is read from scripts/64_arc_ground_ethane.R's outputs; the edits are exact-anchor
replacements written in red (docx_edit_lib.py). No figure or table is inserted, so SI figure,
table and section numbering is untouched.

usage: python3 scripts/65_arc_ground_edits.py <main.docx> <si.docx> <main_out> <si_out>
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

WIN = ("20240701", "20240713")                      # the ARC drove 1 to 13 July
win = [c for c in CV if WIN[0] <= c["date"] <= WIN[1]]
n_days = len(win)
metro_days = [c["date"] for c in win if int(c["n_urban_metro"]) > 0]
metro_main = max(win, key=lambda c: int(c["n_urban_metro"]))
urban_s = sum(int(c["n_urban"]) for c in win); basin_s = sum(int(c["n_basin"]) for c in win)
metro_s = int(metro_main["n_urban_metro"])
other_metro = [(c["date"], int(c["n_urban_metro"])) for c in win
               if int(c["n_urban_metro"]) > 0 and c["date"] != metro_main["date"]]
other_txt = ("; " + ", ".join(f"{v} records on {d[6:8]} July" for d, v in other_metro) +
             ", and none on the other days") if other_metro else "; none on the other days"
METRO_LAT = f'{float(S["metro_lat"]):.2f}'

def n(cls, reg): return sum(1 for p in PL if p["class"] == cls and p["region"] == reg)
u_all = sum(1 for p in PL if p["region"] == "urban"); b_all = sum(1 for p in PL if p["region"] == "basin")
u_fo, u_fr = n("fossil-type", "urban"), n("ethane-free", "urban")
b_fo, b_fr = n("fossil-type", "basin"), n("ethane-free", "basin")
# Three decimals throughout, matching how the endmembers are quoted elsewhere (0.102, 0.0813).
def f3(v): return f"{float(v):.3f}"
def r3(v): return " to ".join(f3(x) for x in v.split(" to "))
U = f3(S["fossil_type_ratio_median_urban"]); U_CI = r3(S["fossil_type_ratio_ci_urban"])
B = f3(S["fossil_type_ratio_median_basin"]); B_CI = r3(S["fossil_type_ratio_ci_basin"])
U_IQR = r3(S["fossil_type_ratio_iqr_urban"]); B_IQR = r3(S["fossil_type_ratio_iqr_basin"])
U_WIN = f3(S["fossil_type_ratio_median_window_urban"]); B_WIN = f3(S["fossil_type_ratio_median_window_basin"])
print(f"ARC: {n_days} drives {WIN[0]}-{WIN[1]}; metro days {metro_days}; urban {urban_s} s, basin {basin_s} s; "
      f"urban plumes {u_all} ({u_fo} ethane-bearing, {u_fr} ethane-free) median {U} {U_CI}; "
      f"basin {b_all} ({b_fo}/{b_fr}) median {B} {B_CI}")

# ---- main text ---------------------------------------------------------------------------
R("Because the mobile laboratory did not measure ethane, these data cannot be used for source "
  "apportionment and are included only to provide qualitative source context. Instrumentation, "
  "data merging, and record counts are described in section S2.",
  "Because the mobile laboratory did not measure ethane, these data cannot be used for source "
  "apportionment and are included only to provide qualitative source context. A second ground "
  f"platform, the NOAA ARL Air Resources Car, drove on {n_days} days between 1 and 13 July 2024 with an "
  "Aeris MIRA Ultra measuring CH4 and C2H6, but its coverage of the city is limited: only on 9 July did it "
  f"sample appreciably inside the analysis box south of {METRO_LAT}°N, and on the other days it crossed "
  "mainly the northern edge of the box while transiting to and from the DJB. Its ethane-bearing plumes give "
  f"median ground-level ratios of {U} mol mol-1 inside the box and {B} mol mol-1 in the basin, which we "
  "use as source-composition context rather than as a constraint on the urban split. Instrumentation, "
  "data merging, and record counts for both ground platforms are described in section S2.")

# ---- SI: new S2.3 ------------------------------------------------------------------------
si = (
 "[[H2]]S2.3 NOAA ARL Air Resources Car (1 to 13 July 2024)\n\n"
 f"The NOAA Air Resources Laboratory operated an instrumented car (ARC) on {n_days} days between 1 and 13 July 2024, "
 "carrying an Aeris MIRA Ultra (CH4 and C2H6 at 1 Hz), a Picarro G2401-m (CO2, CH4, CO, H2O), a Picarro G2201i "
 "(d13C-CH4), NOx, O3, NH3, black-carbon and particle monitors, differential GPS and winds. Unlike the CSL mobile "
 "laboratory these data carry ethane and can therefore speak to source composition. Their coverage of the city is "
  f"limited: of the {n_days} drives, only that of 9 July sampled the analysis box appreciably south of "
 f"{METRO_LAT}°N ({metro_s:,} one-second records{other_txt}), the latitude below which the box is metropolitan Denver rather than the "
 "Commerce City industrial corridor along its northern edge. On the other days the car crossed mainly that northern "
 f"edge while transiting to and from the DJB, so {basin_s:,} of its one-second records fall north of 40.05°N "
 f"against {urban_s:,} inside the box.\n\n"
 "We treated each drive as the mobile analogue of a flight (script 64 of the analysis repository). A rolling 5th-"
 "percentile background over 300 s was removed from CH4 and C2H6; plumes were flagged where the methane enhancement "
 "exceeded both three times the robust noise scale of that day's quiet residuals and 50 ppb; runs separated by less "
 "than 60 s were merged and plumes with fewer than 10 usable points dropped; and one York slope of the ethane "
 "enhancement on the methane enhancement was fitted per plume. A plume in which ethane tracks methane (r at least "
 "0.7, slope at least 0.01 mol mol-1) is called ethane-bearing; one whose methane rises with no ethane response "
 "(|r| below 0.3) is called ethane-free.\n\n"
 f"Inside the box the car crossed {u_all} plumes, of which {u_fo} are ethane-bearing with a median "
 f"ethane-to-methane ratio of {U} mol mol-1 (95% confidence interval {U_CI}; interquartile range {U_IQR}) and "
 f"{u_fr} are ethane-free. In the basin {b_fo} of {b_all} plumes are ethane-bearing, with a median of {B} mol mol-1 "
 f"({B_CI}; interquartile range {B_IQR}), and {b_fr} are ethane-free. Restricting both sets to 1 to 13 July alone "
 f"gives {U_WIN} and {B_WIN} mol mol-1. A road plume is a single source sampled at close range, so these ratios "
 "describe the facilities the car drove past rather than a boundary-layer aggregate, and they are not used to "
 "constrain the urban fossil fraction. They are nevertheless consistent with the treatment of the endmember in "
 "section 3.2 and section S5. The ethane-bearing basin plumes sit above the 0.0813 mol mol-1 production-area "
 "enhancement ratio and well above the 0.036 mol mol-1 basin ratio measured from the aircraft, as expected if both "
 "of those aggregate measurements are diluted by co-sampled biogenic methane, while the ethane-bearing plumes "
 "inside the box fall within the Denver-relevant endmember range of 0.061 to 0.110 mol mol-1 adopted here. The "
 "d13C-CH4 channel is recorded on every drive, but no plume gave a Keeling intercept constrained well enough "
 "(coefficient of determination at least 0.5) on the uncalibrated delta to be reported, so the isotopic data are "
 "left to a dedicated analysis."
)
I("The transect captured frequent methane enhancements of up to ~1,161 ppb", si, "si")

L.apply(a.main_in, a.main_out, "main", "main text")
L.apply(a.si_in, a.si_out, "si", "SI")
