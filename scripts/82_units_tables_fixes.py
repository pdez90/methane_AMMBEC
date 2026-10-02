"""82_units_tables_fixes.py -- the 2 Oct 2026 round of fixes after the final vet.

Analysis changes behind it (rerun before this script; every number below is read from the outputs):
  * R/read_mobile.R: the CDPHE inlet delay now moves each reading to the GPS position at its
    sampling time (it used to shift only the clock). Scripts 03, 04, 06, 26: plume map (Figure S2)
    and the survey trend fits (SI S2.1) change slightly.
  * 24: the near-complex wind direction is a circular median (a plain median breaks across north);
    Table S11 is rebuilt.
  * 59: crossing ratios and medians are computed from unrounded rates; text values in 4.3 and S11.
  * 33: 29 June L1 had no Dalek lidar retrieval and used a 2000 m default; it now uses the PUMAS
    Doppler lidar over the flight window (Table S2).
  * 19: in-box boundary-layer samples use the same 1.5 km cutoff as Figure 4B, S8 and S11
    (config BL_AGL_MAX_M); the text said 1.6 km. No reported value changes.
Manuscript changes:
  * Emission-rate units are t/h throughout (t CH4/h where the species is named), main text, SI,
    tables and the regenerated figures (Figure 4, S10, S17).
  * Abstract: the GRA2PES v2.0beta total.
  * Table 1: footnote for the 5 July n/a.
  * DADS distances labelled (plume peak 3.2 km; mean linked-sample distance 5.5 km; mean distance
    to the v2.0beta cell centre in Table S10).
  * Figures swapped: main Figure 4; SI Figures S2, S10, S17.

usage: python3 scripts/82_units_tables_fixes.py <main.docx> <si.docx> <main_out> <si_out> --outputs <OUT_DIR>
"""
import sys, csv, re, copy, argparse, statistics as st
from decimal import Decimal, ROUND_HALF_UP
from pathlib import Path
sys.path.insert(0, str(Path(__file__).resolve().parent))
import docx
from docx.oxml.ns import qn
import docx_edit_lib as L
from docx_edit_lib import R, FIGSWAP, runs_text, all_paragraphs, replace_in_paragraph

ap = argparse.ArgumentParser(); ap.add_argument("main_in"); ap.add_argument("si_in"); ap.add_argument("main_out"); ap.add_argument("si_out")
ap.add_argument("--outputs", required=True)
a = ap.parse_args(); OUT = Path(a.outputs); L.OUTDIR = str(OUT); L.FIGDIR = str(OUT / "figures")
rd = lambda f: list(csv.DictReader(open(OUT / f)))
hu = lambda x, q: str(Decimal(str(x)).quantize(Decimal(q), rounding=ROUND_HALF_UP))
REPO = Path(__file__).resolve().parent.parent
BL_M = float(re.search(r"^BL_AGL_MAX_M\s*<-\s*([0-9.]+)", open(REPO / "config.R").read(), re.M).group(1))

# ---- numbers ----------------------------------------------------------------------------------
V2 = rd("gra2pes_cells_v2.0beta_summary.csv")[0]; v2_total = float(V2["ch4_total_t_hr"])
T1 = {r["flight"]: r for r in rd("table1_full.csv")}
na_fl = [f for f, r in T1.items() if r["fossil_pct"] in ("", "NA") and int(r["urban_legs"]) >= 3]
assert na_fl == ["20240705_R0_L1"] and T1["20240705_R0_L1"]["n_legs_within"] == "0", na_fl
na_legs = int(T1["20240705_R0_L1"]["urban_legs"])
TR = {r["site"]: r for r in rd("mobile_trend_fits.csv")}
slopes = [abs(float(r["slope_ppmv_per_yr"])) for r in TR.values()]; pvals = [float(r["p_value"]) for r in TR.values()]
CS = [r for r in rd("denver_curtain_screen.csv") if r["flight"] == "20240629_R0_L1"]
assert CS and all(r["blh_src"] == "lidar (PUMAS)" for r in CS), "rerun scripts/33 with METHANE_PUMAS_DIR set"
blh29 = int(float(CS[0]["blh_m"]))
E = [r for r in rd("v2_pointsource_encounters.csv") if r["hotspot_in_fetch"] == "TRUE"]
num = lambda r, k: float(r[k])
rb = [num(r, "ratio_bio_over_v2waste") for r in E]; rt = [num(r, "ratio_total_over_v2total") for r in E]
SUM = {r["hotspot_id"]: r for r in rd("v2_pointsource_summary.csv")}
sm = [r for r in E if r["hotspot_id"] == "3"]
sm_ratio = float(SUM["3"]["ratio_bio_over_v2waste_median"])
light = [r for r in sm if num(r, "ws_ms") < 2]; steady = [r for r in sm if r not in light]
assert len(light) == 1
ex_bio = st.median(num(r, "Q_bio_t_hr") for r in steady); ex_ratio = st.median(num(r, "ratio_bio_over_v2waste") for r in steady)
HS = {r["hotspot_id"]: r for r in rd("v2_pointsource_hotspots.csv")}
cell_km = float(HS["1"]["facility_km"])
CE = [r for r in rd("carbonmapper_encounters.csv") if r["criterion"] == "strict" and "DADS" in r["source"]]
assert len(CE) == 1; dads_d, dads_mis, dads_n = CE[0]["dist_km"], CE[0]["mismatch_deg"], CE[0]["n"]
FS = rd("facility_separation_scan.csv")
print(f"v2 total {v2_total:.2f}; trend |slope| {min(slopes):.4f}-{max(slopes):.4f}, p {min(pvals):.2f}-{max(pvals):.2f}; "
      f"29 Jun BLH {blh29} m (PUMAS); v2 pooled {st.median(rb):.2f}/{st.median(rt):.2f}, range {min(rb):.2f}-{max(rb):.2f}; "
      f"Suncor {sm_ratio:.2f}, excluding light wind {ex_bio:.2f} t/h {ex_ratio:.2f}; DADS {dads_d} km {dads_mis} deg n {dads_n}")

# ---- main text ----------------------------------------------------------------------------------
R("exceed two box-consistent bottom-up inventories (1.7 and 2.8 t CH4 per hour), by about 2.7 to 4.5-fold at the CH4:CO median.",
  "exceed the two published box-consistent bottom-up inventories (1.7 and 2.8 t CH4/h), by about 2.7 to 4.5-fold at the "
  f"CH4:CO median, but fall below the {v2_total:.1f} t CH4/h of the GRA2PES v2.0beta release, which adds substantial waste methane.")
R("contains a methane plume peaking at 93 ppb, 3.2 km downwind of the DADS landfill.",
  "contains a methane plume whose 93 ppb peak lies 3.2 km downwind of the DADS landfill.")
R("(below 1.6 km above ground)", f"(below {BL_M / 1000:.1f} km above ground)")
R("or 1.77 times the inventory value in the median", f"or {sm_ratio:.2f} times the inventory value in the median")

# ---- SI -----------------------------------------------------------------------------------------
R("slopes of 0.005 to 0.008 ppmv per year in magnitude, p = 0.42 to 0.47",
  f"slopes of {hu(min(slopes), '0.001')} to {hu(max(slopes), '0.001')} ppmv per year in magnitude, "
  f"p = {hu(min(pvals), '0.01')} to {hu(max(pvals), '0.01')}", "si")
R("None of the 24 screens meets all four conditions, so no curtain flux is computed.",
  "None of the 24 screens meets all four conditions, so no curtain flux is computed. Mixing heights are Dalek 2 lidar "
  "retrievals (Boulder); that lidar has no data on 29 June, so the flight uses the PUMAS lidar in the DJ Basin.", "si")
R("The 235 m leg is a single plume peaking at 93 ppb of methane enhancement 3.2 km from the DADS landfill;",
  "The 235 m leg is a single plume whose 93 ppb methane-enhancement peak lies 3.2 km from the DADS landfill;", "si")
R(f"passed {dads_d} km downwind of the DADS landfill, {dads_mis}° off its downwind line, with {dads_n} gated samples:",
  f"passed downwind of the DADS landfill with {dads_n} gated samples, at a mean distance of {dads_d} km from the landfill and "
  f"a mean {dads_mis}° off its downwind line (the plume peak itself lies 3.2 km from the landfill, section S3):", "si")
R("Crossings whose hotspot cell fell outside the fetch (the leg was too far off the downwind line) are omitted.",
  "Crossings whose hotspot cell fell outside the fetch (the leg was too far off the downwind line) are omitted. Distance "
  "is the mean distance of the crossing's linked samples from the centre of the 4-km hotspot cell; the centre of the DADS "
  f"cell (site 1) lies {cell_km:.0f} km from the landfill, so this distance differs from the plume-peak distance (3.2 km, "
  f"section S3) and from the mean distance of the linked samples to the landfill itself ({dads_d} km, section S8).", "si")
R("the aircraft biogenic rate is 0.72 in the median (range 0.07 to 6.03); relative to the v2.0beta total in the fetch, "
  "the aircraft total is 0.69 in the median.",
  f"the aircraft biogenic rate is {st.median(rb):.2f} in the median (range {min(rb):.2f} to {max(rb):.2f}); relative to the "
  f"v2.0beta total in the fetch, the aircraft total is {st.median(rt):.2f} in the median.", "si")
R("(total 0.15 to 3.59 t/hr), 1.77 times the v2.0beta waste emissions in the upwind fetch.",
  f"(total 0.15 to 3.59 t/hr), {sm_ratio:.2f} times the v2.0beta waste emissions in the upwind fetch.", "si")
R("gives 1.10 t/hr and 2.82 times.", f"gives {ex_bio:.2f} t/hr and {ex_ratio:.2f} times.", "si")
R("ordered by cross-wind separation. A flight is flagged separable",
  "ordered by cross-wind separation. Wind from is the circular median direction of the near-complex plume samples. A flight is flagged separable", "si")

FIGSWAP("main", "image1.png", str(OUT / "figures" / "Fig4_source_combined.png"))
FIGSWAP("si", "image6.png", str(OUT / "figures" / "FigS4_mobile_plumes.png"))
FIGSWAP("si", "image10.png", str(OUT / "figures" / "FigS9_biogenic_sources.png"))
FIGSWAP("si", "image7.png", str(OUT / "figures" / "FigS14_v2_pointsource.png"))

tmp_m = Path(a.main_out).with_suffix(".t1.docx"); tmp_s = Path(a.si_out).with_suffix(".t1.docx")
L.apply(a.main_in, tmp_m, "main", "main text")
L.apply(a.si_in, tmp_s, "si", "SI")

# ---- units: t/h throughout ------------------------------------------------------------------------
UNITS = [
  (re.compile(r"(?<![A-Za-z])t CH4 per hour"), "t CH4/h"),
  (re.compile(r"(?<![A-Za-z])tonnes per hour"), "t/h"),
  (re.compile(r"(?<![A-Za-z])t CH4 ?(?:/ ?hr\b|h-1|h−1)"), "t CH4/h"),
  (re.compile(r"(?<![A-Za-z])t ?(?:/ ?hr\b|h-1|h−1)"), "t/h"),
  (re.compile(r"(?<![A-Za-z])kg CH4 ?(?:/ ?hr\b|h-1|h−1)"), "kg CH4/h"),
  (re.compile(r"(?<![A-Za-z])kg ?(?:/ ?hr\b|h-1|h−1)"), "kg/h"),
]
def fix_units(d):
    n = 0
    for p in all_paragraphs(d):
        for pat, new in UNITS:
            while True:
                m = pat.search(runs_text(p))
                if not m: break
                if replace_in_paragraph(p, m.group(0), new, m.start()) < 0: raise SystemExit(f"unit edit failed: {m.group(0)!r}")
                n += 1
    left = [m.group(0) for p in all_paragraphs(d) for pat, _ in UNITS for m in pat.finditer(p.text) if m.group(0) not in ("t/h", "t CH4/h")]
    assert not left, f"units left: {left[:5]}"
    return n

def set_cell(cell, text):
    cp = cell.paragraphs[0]
    for r_ in cp.runs[1:]: r_._r.getparent().remove(r_._r)
    if not cp.runs: cp.add_run("")
    cp.runs[0].text = text; cp.runs[0].font.color.rgb = L.RED

def table_by_header(d, prefix):
    hits = [t for t in d.tables if " | ".join(c.text for c in t.rows[0].cells).startswith(prefix)]
    assert len(hits) == 1, f"table {prefix!r}: {len(hits)}"
    return hits[0]

main = docx.Document(tmp_m); si = docx.Document(tmp_s)
nm, ns = fix_units(main), fix_units(si)
print(f"units: {nm} in the main text, {ns} in the SI")

# Table 1 footnote, directly below the table
t1 = table_by_header(main, "Flight (leg) | Urban legs")
cap = next(p for p in main.paragraphs if p.text.startswith("Table 1:"))
foot = copy.deepcopy(cap._p)
for child in list(foot):
    if child.tag != qn("w:pPr"): foot.remove(child)
t1._tbl.addnext(foot)
fp = docx.text.paragraph.Paragraph(foot, cap._parent)
r_ = fp.add_run(f"n/a: on 5 July (L1) none of the {na_legs} urban legs had the ten samples above the 20 ppb methane gate "
                "needed for a within-leg slope, so no fossil fraction could be estimated.")
r_.font.color.rgb = L.RED; r_.font.size = docx.shared.Pt(9)
print("Table 1 footnote added")

# Table S2: 29 June mixing height
t2 = table_by_header(si, "Flight | Wall | Legs / levels")
h2 = [c.text for c in t2.rows[0].cells]; ib = h2.index("Mixing ht (m)")
n2 = 0
for row in t2.rows[1:]:
    if row.cells[0].text.strip() == "2024-06-29 (L1)":
        assert row.cells[ib].text.strip() == "2000", row.cells[ib].text
        set_cell(row.cells[ib], f"{blh29} (PUMAS)"); n2 += 1
assert n2 >= 1
print(f"Table S2: {n2} row(s) of 29 June set to {blh29} m (PUMAS)")

# Table S10: distance header
t10 = table_by_header(si, "Flight | Leg | Site | Distance (km)")
set_cell(t10.rows[0].cells[3], "Distance to hotspot cell (km)")

# Table S11: rebuilt from facility_separation_scan.csv (circular-median wind)
t11 = table_by_header(si, "Flight | Plume pts near complex | Wind from (deg)")
rows = sorted(FS, key=lambda r: -float(r["cross_sep_m"]))
assert len(rows) == len(t11.rows) - 1, "Table S11 row count changed"
def fl_label(f):
    m = re.match(r"(\d{4})(\d{2})(\d{2})_R\d(?:_(L\d))?", f)
    return f"{m.group(1)}-{m.group(2)}-{m.group(3)}" + (f" ({m.group(4)})" if m.group(4) else "")
n11 = 0
for row, r in zip(t11.rows[1:], rows):
    vals = [fl_label(r["flight"]), r["n_plume_near"], r["wd_med"], r["cross_sep_m"], "yes" if r["separable"] == "TRUE" else "no"]
    for c, v in zip(row.cells, vals):
        if c.text.strip() != v: set_cell(c, v); n11 += 1
print(f"Table S11: {n11} cell(s) updated")

L.subscript_chem(all_paragraphs(main)); L.subscript_chem(all_paragraphs(si))
main.save(a.main_out); si.save(a.si_out); tmp_m.unlink(); tmp_s.unlink()
print("wrote", a.main_out, a.si_out)
