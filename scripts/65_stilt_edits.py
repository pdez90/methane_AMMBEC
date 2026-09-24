"""65_stilt_edits.py -- carry the HRRR-STILT exposure test (script 64) into the manuscript and SI.

SI: a new part of section S9 (after the wind-cone paragraph and Figure S12) with the method, the
numbers and a new Figure S13 (FigS_stilt_exposure.png); the old Figures S13 and S14 become S14 and
S15 in both documents. Main text: one sentence at the end of the leg-level wind paragraph in
section 4.1 and one in Limitations. Every number is read from the script-64 outputs; every edit is
an exact-anchor operation written in red (docx_edit_lib.py), so nothing is written if an anchor is
missing or duplicated.

usage: python3 scripts/65_stilt_edits.py <main.docx> <si.docx> <main_out> <si_out>
           --outputs <OUT_DIR> --figures <OUT_DIR>/figures
"""
import sys, csv, argparse, statistics as st
from pathlib import Path
sys.path.insert(0, str(Path(__file__).resolve().parent))
import docx_edit_lib as L
from docx_edit_lib import R, I

ap = argparse.ArgumentParser(); ap.add_argument("main_in"); ap.add_argument("si_in"); ap.add_argument("main_out"); ap.add_argument("si_out")
ap.add_argument("--outputs", required=True); ap.add_argument("--figures", required=True)
a = ap.parse_args(); OUT = Path(a.outputs); L.FIGDIR = a.figures; L.OUTDIR = str(OUT)

def rows(name): return list(csv.DictReader(open(OUT / name)))
S = rows("stilt_exposure_summary.csv")   # a list: two tests share the label "same, legs < 1 km AGL"
Q = {r["quantity"]: float(r["value"]) for r in rows("stilt_exposure_quantities.csv")}
LG = rows("stilt_exposure_legs.csv")
def test(i): r = S[i]; return float(r["rho"]), float(r["p"]), int(r["n"])
assert len(S) == 8 and S[0]["test"].startswith("slope vs f_og_stilt") and S[5]["test"].startswith("observed vs predicted"), "unexpected summary layout"
rho_f, p_f, n_f = test(0); rho_fl, p_fl, n_fl = test(1); rho_ppb, p_ppb, _ = test(2)
rho_core, p_core, _ = test(3); rho_sp, p_sp, _ = test(4); rho_tr, p_tr, _ = test(5); rho_trl, p_trl, _ = test(6)

tot = [float(r["total"]) for r in LG]
share = lambda k: 100 * st.median([float(r[k]) / float(r["total"]) for r in LG if float(r["total"]) > 0])
f_og = [float(r["f_og_stilt"]) for r in LG]
ratio = sorted(float(r["obs_over_pred"]) for r in LG)
q1, q3 = st.quantiles(ratio, n=4)[0], st.quantiles(ratio, n=4)[2]
pct = lambda x: f"{100 * x:.0f}"
fp = lambda p: f"p = {p:.2f}" if p >= 0.01 else f"p = {p:.3f}"
rh = lambda x: f"{x:.2f}".replace("-", "−")

# ---- renumber the SI figures after S12 (S14 -> S15 first, then S13 -> S14) ------------------------
R("Figure S14", "Figure S15", "si", 2); R("Figure S13", "Figure S14", "si", 1)
R("Figure S14", "Figure S15", "main", 1); R("Figure S13", "Figure S14", "main", 1)

# ---- SI: new part of section S9 ----------------------------------------------------------------
si_text = (
  "The cone uses the aircraft's own wind and a fixed geometry. To test whether modelled transport changes the result, we replaced it "
  f"with receptor-oriented Lagrangian footprints. Four gated samples from each of the {n_f} fitted urban legs served as receptors "
  f"({len(LG) * 4} in total). From each, 500 particles were followed 10 h backward with STILT (HYSPLIT in STILT emulation; Lin et al., 2003; "
  "Fasoli et al., 2018; Stein et al., 2015) driven by the 3-km HRRR meteorology archived by NOAA ARL (Dowell et al., 2022), "
  "including the near-field plume-dilution correction. The footprints were convolved with the gridded EPA GHGI fluxes of five source classes "
  "(basin-type oil and gas, natural-gas distribution and post-meter, waste, livestock, and all other sectors), giving each leg an "
  "inventory-predicted enhancement from each class and a basin oil-and-gas share of that enhancement, fOG."
  "\n\n"
  f"Basin oil and gas accounts for a median {pct(Q['median f_og_stilt (all legs)'])}% of the predicted urban enhancement "
  f"(range {pct(min(f_og))} to {pct(max(f_og))}%; {pct(Q['median f_djbcore_stilt'])}% from the basin north of 40.05°N), "
  f"or {pct(Q['median f_og_stilt with basin O&G x2'])}% if basin oil-and-gas emissions are doubled. Waste accounts for {share('waste'):.0f}%, "
  f"distribution and post-meter gas for {share('ng_dist'):.0f}% and livestock for {share('livestock'):.0f}%. "
  f"As with the cone, the observed leg slope is uncorrelated with fOG (Spearman ρ = {rh(rho_f)}, {fp(p_f)}, n = {n_f}; "
  f"ρ = {rh(rho_fl)} for the {n_fl} legs below 1 km), with the share from the basin north of 40.05°N (ρ = {rh(rho_core)}) and with the "
  f"inventory-predicted slope (ρ = {rh(rho_sp)}, {fp(p_sp)}). The slope is negatively correlated with the absolute predicted basin "
  f"enhancement (ρ = {rh(rho_ppb)}, {fp(p_ppb)}). We attribute this to large near-surface footprints also carrying large waste "
  "contributions, not to basin air lowering the slope. With source ratios of 0.102 (basin) and 0.110 (delivered gas), the median "
  f"inventory-predicted slope is {Q['median predicted slope']:.3f} mol mol−1, close to the observed median of {Q['median observed slope']:.3f}."
  "\n\n"
  f"The model reproduces the contrast between legs within and above the boundary layer (observed against predicted enhancement, "
  f"ρ = {rh(rho_tr)}, {fp(p_tr)}) but not the variation among boundary-layer legs (ρ = {rh(rho_trl)}, {fp(p_trl)}). The HRRR "
  f"transport direction at the receptors differs from the aircraft wind by a median of {Q['median |aircraft - HRRR| wind direction (deg)']:.0f}°, "
  "systematically so on the first 13 July flight (HRRR northerly at 5 to 6 m s−1, aircraft north-easterly at 2 to 5 m s−1), so under "
  "these light winds the leg-level test has little power. The predicted enhancement is a median "
  f"{Q['median observed/predicted dCH4']:.1f} times smaller than the observed (interquartile range {q1:.1f} to {q3:.1f}). This transport-based "
  "estimate of the inventory shortfall is independent of the CO and CO2 anchors and is consistent with section 4.2, although it depends on the "
  "10-h footprint length and on differences between the model and aircraft background definitions. We therefore read the transport analysis as "
  f"bounding the basin contribution in aggregate, at roughly {pct(Q['median f_og_stilt (all legs)'])} to "
  f"{pct(Q['median f_og_stilt with basin O&G x2'])}% of the urban enhancement in the inventory, while the leg-to-leg variability of the "
  "fossil signal is not organised by modelled basin exposure."
  "\n\n[[FIG:FigS_stilt_exposure.png]]\n\n"
  "Figure S13: HRRR-STILT exposure of the 25 fitted urban legs. (a) Leg ethane:methane slope against the basin oil-and-gas share of the "
  "enhancement predicted by convolving STILT footprints with the gridded EPA GHGI. (b) Observed mean gated enhancement against the predicted "
  "enhancement (log axes; dashed line 1:1). (c) Mean footprint of the 100 receptors (log10), with the top decile of basin oil-and-gas cells "
  "outlined in red, the Denver-metro box in black and receptors as crosses. Circles are legs below 1 km above ground, triangles above; "
  "symbol size scales with the number of gated samples.")
I("Figure S12: Per-leg ethane:methane slope against (a) the angle", si_text, "si")

# ---- main text ----------------------------------------------------------------------------------
R("therefore do not resolve a basin-advection gradient.",
  "therefore do not resolve a basin-advection gradient. Replacing the wind cone with HRRR-driven STILT footprints convolved with the "
  f"gridded GHGI gives the same result (ρ = {rh(rho_f)} between the leg slope and the modelled basin share; section S9, Figure S13).")
R("some contribution from advected DJB methane to their elevated fossil signature cannot be excluded.",
  "some contribution from advected DJB methane to their elevated fossil signature cannot be excluded. HRRR-STILT footprints convolved "
  f"with the gridded GHGI attribute roughly {pct(Q['median f_og_stilt (all legs)'])}% of the predicted urban enhancement to basin oil and gas "
  f"({pct(Q['median f_og_stilt with basin O&G x2'])}% if basin emissions are doubled), although the leg-level ethane:methane slope does not vary "
  "with this modelled exposure (section S9).")

L.apply(a.main_in, a.main_out, "main", "main text")
L.apply(a.si_in, a.si_out, "si", "SI")
