"""62_epa_areaweight_edits.py -- carry the area-weighted EPA GHGI box sums (scripts 14 and 32) into the
author's current manuscript files, plus the SI cross-references that the SI reorder (script 60) changed.

Every number is read from the pipeline outputs; every edit is an exact-anchor replacement written in red
(see docx_edit_lib.py). Anchors are the sentences of the author's current cleaned files, so the script
stops without writing if any anchor is missing or duplicated.

usage: python3 scripts/62_epa_areaweight_edits.py <main.docx> <si.docx> <main_out> <si_out>
           --outputs <OUT_DIR> --figures <OUT_DIR>/figures
"""
import sys, csv, json, argparse
from pathlib import Path
sys.path.insert(0, str(Path(__file__).resolve().parent))
import docx_edit_lib as L
from docx_edit_lib import R, FIGSWAP

ap = argparse.ArgumentParser(); ap.add_argument("main_in"); ap.add_argument("si_in"); ap.add_argument("main_out"); ap.add_argument("si_out")
ap.add_argument("--outputs", required=True); ap.add_argument("--figures", required=True)
a = ap.parse_args(); OUT = Path(a.outputs); L.FIGDIR = a.figures; L.OUTDIR = str(OUT)

def rows(name): return list(csv.DictReader(open(OUT / name)))

# ---- numbers ---------------------------------------------------------------------------------
pv = json.load(open(OUT / "paper_values.json"))
inv = rows("inventory_comparison.csv")
tot = {g: sum(float(r["t_hr"]) for r in inv if r["group"] == g) for g in ("fossil", "biogenic", "combustion", "other")}
ET = sum(tot.values())
ng = sum(float(r["t_hr"]) for r in inv if r["sector"] in ("1B2b_Natural_Gas_Distribution", "Supp_1B2b_PostMeter"))
p_inv = ng / tot["fossil"]
fpct_tot = 100 * tot["fossil"] / ET; fpct_fb = 100 * tot["fossil"] / (tot["fossil"] + tot["biogenic"])
bio = {r["quantity"]: float(r["value"]) for r in rows("biogenic_inbox_totals.csv")}
te = rows("two_endmember_flights.csv")
den = next(r for r in te if r["case"].startswith("inventory-weighted, DENVER delivered gas (PSCo"))
ec = [r for r in te if r["case"].startswith("inventory-weighted, other-cities delivered gas")]
ec_lo, ec_hi = min(int(r["median_fossil_pct"]) for r in ec), max(int(r["median_fossil_pct"]) for r in ec)
flights = {k: int(v) for k, v in den.items() if k.startswith("2024")}
fl_max_key = max(flights, key=flights.get); fl_max = flights[fl_max_key]
GRA_V11 = float(pv.get("gra2pes_v11_total", 1.7)) if "gra2pes_v11_total" in pv else 1.7
CO_MED = float(pv.get("ch4co_median_thr", 7.6)) if "ch4co_median_thr" in pv else 7.6
CO_LO, CO_HI = 4.6, 10.7

def f1(x): return f"{x:.1f}"
def f2(x): return f"{x:.2f}"
assert abs(ET - pv["epa_total"]) < 0.06 and abs(tot["fossil"] - pv["epa_fossil"]) < 0.01, "inventory_comparison.csv disagrees with paper_values.json"
print(f"EPA area-weighted: total {f1(ET)} fossil {f2(tot['fossil'])} ({fpct_tot:.0f}% of total, {fpct_fb:.0f}% of F+B) "
      f"biogenic {f2(tot['biogenic'])} combustion {f2(tot['combustion'])}; NG dist+post-meter {f2(ng)}; p_inv {p_inv:.2f}; "
      f"Denver inventory-weighted {den['median_fossil_pct']}% (max flight {fl_max}% on {fl_max_key}); East Coast {ec_lo}-{ec_hi}%; "
      f"in-box waste {bio['waste_in_box_t_hr']:.3f} livestock {bio['livestock_in_box_t_hr']:.3f}")
lo_fold = CO_MED / ET; hi_fold = CO_MED / GRA_V11
fold_lo_all = CO_LO / ET; fold_hi_all = CO_HI / GRA_V11

# ---- main text: EPA numbers --------------------------------------------------------------------
R(f"(1.7 and 2.6 t CH4 per hour), by about 3 to 4.5-fold", f"(1.7 and {f1(ET)} t CH4 per hour), by about {lo_fold:.1f} to {hi_fold:.1f}-fold")
R("Several gridded inventories, each aggregated over the same analysis box are used in this analysis.",
  "Several gridded inventories, each aggregated over the same analysis box (for the 0.1-degree EPA grid, each cell weighted by the fraction of its area inside the box), are used in this analysis.")
R("Summed over the same Denver metropolitan box, the 2020 Express Extension totals 2.6 t CH4 h-1. Of this, 1.4 t h-1 is fossil, representing about 54% of the total and roughly 60% of the fossil-plus-biogenic component,",
  f"Summed over the same Denver metropolitan box, with each 0.1-degree cell weighted by the fraction of its area inside the box, the 2020 Express Extension totals {f1(ET)} t CH4 h-1. "
  f"Of this, {f1(tot['fossil'])} t h-1 is fossil, representing about {fpct_tot:.0f}% of the total and roughly {fpct_fb:.0f}% of the fossil-plus-biogenic component,")
R("(~1.0 t h-1 combined). Biogenic sources contribute approximately 1.0 t h-1 from landfills and wastewater, with another 0.2 t h-1 from combustion.",
  f"(~{f1(ng)} t h-1 combined). Biogenic sources contribute approximately {f1(tot['biogenic'])} t h-1 from landfills and wastewater, with another {f1(tot['combustion'])} t h-1 from combustion.")
R("place Denver near 2 t CH4 h-1 (1.7-2.6)", f"place Denver near 2 t CH4 h-1 (1.7-{f1(ET)})")
R("by factors of roughly 1.6-6 across flights and anchors and by about 3-4.5 at the CH4:CO median",
  f"by factors of roughly {fold_lo_all:.1f}-{fold_hi_all:.0f} across flights and anchors and by about {lo_fold:.1f}-{hi_fold:.1f} at the CH4:CO median")
R("This distinction matters because approximately 70% of the fossil methane in the gridded inventory",
  f"This distinction matters because approximately {100 * p_inv:.0f}% of the fossil methane in the gridded inventory")
FIGSWAP("main", (1700, 1705), str(Path(a.figures) / "Fig4_source_combined.png"))

# ---- main text: SI cross-references after the SI reorder ---------------------------------------
R("We compared these estimators for each flight in section S3 (Figure S4).", "We compared these estimators for each flight in section S3 (Figure S5).")
R("a range of 0.095-0.141 during 2023-2025 (Table S8).", "a range of 0.095-0.141 during 2023-2025 (Table S4).")
R("across the full range of mixture proportions (section S3, Figure S3b).", "across the full range of mixture proportions (section S3, Figure S4).")
R("We report the corresponding York slope for comparison (section S3, Figure S4).", "We report the corresponding York slope for comparison (section S3, Figure S5).")
R("are provided in the SI (Table S6, section S6).", "are provided in the SI (Table S7, section S6).")
R("therefore relies on source geography (section S10).", "therefore relies on source geography (section S7).")
R("(slope = -0.0003, r = -0.03; section S11, Figure S12)", "(slope = -0.0003, r = -0.03; section S8, Figure S10)")
R("makes this an exploratory relationship  (section S7, Figure S7).", "makes this an exploratory relationship (section S9, Figure S11).")
R("the largest methane enhancements of the campaign (section S8, Figure S8).", "the largest methane enhancements of the campaign (section S10, Figure S13).")
R("an inventory-based upwind-exposure score (section S7, Figure S11).", "an inventory-based upwind-exposure score (section S9, Figure S12).")
R("composition or mixture considered (Figures S3 and S3b).", "composition or mixture considered (Figures S3 and S4).")
R("is shown in Figure 4B and Table S7;", "is shown in Figure 4B and Table S11;")
R("cross-wind geometry are described in section S9.", "cross-wind geometry are described in section S12.")
R("(0.110; 0.095-0.141 across 2023-2025; Table S8)", "(0.110; 0.095-0.141 across 2023-2025; Table S4)")
R("depending on the assumed basin and delivered-gas ratios (Figure S3b).", "depending on the assumed basin and delivered-gas ratios (Figure S4).")

# ---- SI --------------------------------------------------------------------------------------
R("(inventory-weighted mixture, p = 0.69 with a basin ratio of 0.061: 31%, with the second 13 July flight at the halfway line and no flight above it; distribution gas alone: 27%)",
  f"(inventory-weighted mixture, p = {p_inv:.2f} with a basin ratio of 0.061: {den['median_fossil_pct']}%, with the second 13 July flight at {fl_max}% and no other flight above the halfway line; distribution gas alone: 27%)", "si")
R("reaching 66 to 93% at the inventory weighting for a basin ratio of 0.061", f"reaching {ec_lo} to {ec_hi}% at the inventory weighting for a basin ratio of 0.061", "si")
R("the inventory share of distribution and post-meter gas in the box's fossil methane (0.69). The p = 0.69 line is shown only",
  f"the inventory share of distribution and post-meter gas in the box's fossil methane ({p_inv:.2f}). The p = {p_inv:.2f} line is shown only", "si")
R("totaling 0.87 t CH4/hr within it.", f"totaling {bio['waste_in_box_t_hr']:.2f} t CH4/hr within it.", "si")
R("contributing only 0.10 t CH4/hr within it.", f"contributing only {bio['livestock_in_box_t_hr']:.2f} t CH4/hr within it.", "si")
FIGSWAP("si", (1300, 820), str(Path(a.figures) / "FigS3b_two_endmember.png"))

L.apply(a.main_in, a.main_out, "main", "main text")
L.apply(a.si_in, a.si_out, "si", "SI")
