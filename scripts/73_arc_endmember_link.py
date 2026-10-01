"""73_arc_endmember_link.py -- make the ARC ground dataset load-bearing: tie the plume ratios of
SI S2.3 to the endmember argument in the main text (3.2 and Limitation 1) and in SI S5.

The paper already rests on one ARC-derived number, the 0.0813 mol/mol production-area enhancement
ratio quoted from the AMMBEC report to CDPHE. These edits say what the campaign's own ARC files
show about it: the ethane-bearing basin plumes sit above 0.0813, which is what is expected if an
ambient production-area ratio is diluted by co-sampled biogenic methane, so 0.0813 is a lower
bound on fossil source composition rather than the composition itself.

Numbers come from scripts/71_arc_ground_ethane.R; edits are exact-anchor and red.

usage: python3 scripts/73_arc_endmember_link.py <main.docx> <si.docx> <main_out> <si_out>
           --outputs <OUT_DIR>
"""
import sys, csv, argparse
from pathlib import Path
sys.path.insert(0, str(Path(__file__).resolve().parent))
import docx_edit_lib as L
from docx_edit_lib import R

ap = argparse.ArgumentParser(); ap.add_argument("main_in"); ap.add_argument("si_in"); ap.add_argument("main_out"); ap.add_argument("si_out")
ap.add_argument("--outputs", required=True)
a = ap.parse_args(); OUT = Path(a.outputs); L.OUTDIR = str(OUT)

S = {r["quantity"]: r["value"] for r in csv.DictReader(open(OUT / "arc_ground_summary.csv"))}
PL = list(csv.DictReader(open(OUT / "arc_ground_plumes.csv")))
def f3(v): return f"{float(v):.3f}"
def r3(v): return " to ".join(f3(x) for x in v.split(" to "))
B, B_CI = f3(S["fossil_type_ratio_median_basin"]), r3(S["fossil_type_ratio_ci_basin"])
U, U_CI = f3(S["fossil_type_ratio_median_urban"]), r3(S["fossil_type_ratio_ci_urban"])
b_fo = sum(1 for p in PL if p["class"] == "fossil-type" and p["region"] == "basin")
b_all = sum(1 for p in PL if p["region"] == "basin")
u_fo = sum(1 for p in PL if p["class"] == "fossil-type" and p["region"] == "urban")
b_fr = sum(1 for p in PL if p["class"] == "ethane-free" and p["region"] == "basin")
ARC = S["config_SOURCE_C2H6_CH4_ARC"]
# Break-even endmember (script 27); the closing clause of the S5 sentence is checked, not assumed.
BE = float(next(csv.DictReader(open(OUT / "beta_breakeven.csv")))["beta_breakeven"])
assert min(float(B), float(ARC)) > BE, "a quoted ratio is no longer above the break-even endmember"
print(f"link: basin {b_fo}/{b_all} plumes {B} ({B_CI}); urban {u_fo} plumes {U} ({U_CI}); report value {ARC}")

# ---- main text 3.2 ---------------------------------------------------------------------------
# Deliberately NOT edited here: that paragraph ends on reference 19, so text appended to it would
# capture the citation. Section 3.2 already points to S5, which now carries the plume-scale result.

# ---- main text, Limitation 1: the same point where the endmember uncertainty is discussed ----
R("The lower part of this range is constrained by the 2021 aircraft flux partition (0.061) and the "
  "ground-level production-area ratio measured during this campaign (0.0813; section S5); using these "
  "values raises the campaign median to 36-48% fossil",
  "The lower part of this range is constrained by the 2021 aircraft flux partition (0.061) and the "
  "ground-level production-area ratio measured during this campaign (0.0813; section S5). Both are ambient "
  "enhancement ratios, diluted by whatever biogenic methane was co-sampled, and so bound the fossil source "
  f"composition from below rather than estimating it: resolved plume by plume, the ground car's ethane-bearing basin "
  f"plumes run higher, at a median of {B} mol mol-1, while those inside the analysis box ({U} mol mol-1) fall within "
  "the adopted range (section S2.3). Using the two ambient values as endmembers raises the campaign median to 36-48% fossil")

# ---- SI S5: the independent-constraint sentence ---------------------------------------------
R("The ground-level ratio of 0.0813 mol mol−1 measured in DJB production areas during this campaign "
  "lies well above the break-even and provides an independent contemporary atmospheric constraint.",
  "The ground-level ratio of 0.0813 mol mol−1 measured in DJB production areas during this campaign "
  "lies well above the break-even and provides an independent contemporary atmospheric constraint. Resolving the same "
  f"ground record plume by plume puts it higher still: {b_fo} of the {b_all} basin plumes crossed by the ground car carry ethane "
  f"tracking methane, at a median of {B} mol mol−1 (95% confidence interval {B_CI}; section S2.3), while {b_fr} show methane "
  "with no ethane response. An ambient ratio averaged over both populations is therefore a lower bound on the composition of the "
  f"fossil sources themselves, and every value in that comparison lies above the {BE:.3f} mol mol−1 break-even.", "si")

L.apply(a.main_in, a.main_out, "main", "main text")
L.apply(a.si_in, a.si_out, "si", "SI")
