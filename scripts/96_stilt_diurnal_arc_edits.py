"""96_stilt_diurnal_arc_edits.py -- second round of edits on the output of script 93 (14 Oct 2026):

  * HRRR-STILT footprints re-run for the constant-altitude transects (script 64; 112 receptors on
    28 transects of seven flights with archived HRRR meteorology) and convolved with both the EPA
    gridded GHGI and GRA2PES v2.0beta: main 4.2, 4.3, Discussion, limitations; SI S9 and Figure S15.
  * Time-of-day sensitivity of the CO anchor (script 94): main 3.4 and limitations; SI S6.
  * ARL car: the archived files carry CH4 from the Aeris and the two Picarros; the ratios use the
    Aeris CH4 and C2H6 (main 2.3).
Numbers are read from the outputs and asserted. All inserted text is red.

usage: python3 scripts/96_stilt_diurnal_arc_edits.py <main93.docx> <si93.docx> <main_out> <si_out>
         --outputs <OUT_DIR> --figures <OUT_DIR>/figures
"""
import sys, csv, re, argparse, statistics as st, zipfile
from pathlib import Path
sys.path.insert(0, str(Path(__file__).resolve().parent))
import numpy as np, docx
import docx_edit_lib as L
import docx_ops as O

ap = argparse.ArgumentParser()
for k in ("main_in", "si_in", "main_out", "si_out"): ap.add_argument(k)
ap.add_argument("--outputs", required=True); ap.add_argument("--figures", required=True)
a = ap.parse_args(); OUT = Path(a.outputs); FIG = Path(a.figures)
rd = lambda f: list(csv.DictReader(open(OUT / f)))

# ---- STILT numbers -----------------------------------------------------------------------
SL = rd("stilt_exposure_legs.csv"); SS, _par = {}, ""
for r in rd("stilt_exposure_summary.csv"):        # "  same, legs < 1 km AGL" rows qualify the test above them
    k = r["test"].strip()
    if k.startswith("same,"): k = _par + " | " + k
    else: _par = k
    SS[k] = r
Q = {r["quantity"]: float(r["value"]) for r in rd("stilt_exposure_quantities.csv")}
nT = len(SL); nF = len({r["flight"] for r in SL}); nR = len(rd("stilt_exposure_receptors.csv"))
nAll = len(rd("wind_source_exposure_legs.csv"))
assert (nT, nF, nR, nAll) == (28, 7, 112, 36), (nT, nF, nR, nAll)
col = lambda k: [float(r[k]) for r in SL if r[k] not in ("NA", "")]
med = lambda k: st.median(col(k))
sh = lambda k, t: st.median(float(r[k]) / float(r[t]) for r in SL)
S = dict(
    fog=round(100 * med("f_og_stilt")), fog_lo=round(100 * min(col("f_og_stilt"))), fog_hi=round(100 * max(col("f_og_stilt"))),
    fog2=round(100 * med("f_og_stilt_x2")), core=round(100 * med("f_djbcore_stilt")),
    waste=round(100 * sh("waste", "total")), dist=round(100 * sh("ng_dist", "total")), live=round(100 * sh("livestock", "total")),
    v2og=round(100 * sh("v2_og", "v2_total")), v2waste=round(100 * sh("v2_waste", "v2_total")), v2ag=round(100 * sh("v2_ag", "v2_total")),
    pe=med("slope_pred"), pv=med("slope_pred_v2"), ob=med("slope"),
    oe=med("obs_over_pred"), ov=med("obs_over_pred_v2"),
    oe_q=np.percentile(col("obs_over_pred"), [25, 75]), ov_q=np.percentile(col("obs_over_pred_v2"), [25, 75]),
    v2e=Q["v2: median predicted/EPA-predicted dCH4"], wdd=Q["median |aircraft - HRRR| wind direction (deg)"])
r_ = lambda k: SS[k]
t_fog, t_fog_lo, t_core, t_pe, t_pv = r_("slope vs f_og_stilt (basin O&G share of predicted dCH4)"), r_("slope vs f_og_stilt (basin O&G share of predicted dCH4) | same, legs < 1 km AGL"), \
    r_("slope vs DJB-core share (basin O&G north of 40.05 N)"), r_("slope vs inventory-predicted slope"), r_("slope vs GRA2PES v2.0beta-predicted slope")
t_mag = r_("observed vs predicted dCH4 (transport check)")
assert (S["fog"], S["fog2"], round(S["pe"], 3), round(S["pv"], 3), round(S["ob"], 3)) == (26, 42, 0.045, 0.048, 0.025), S
print("STILT:", {k: (round(v, 3) if isinstance(v, float) else v) for k, v in S.items() if not hasattr(v, "shape")})

# ---- diurnal numbers -----------------------------------------------------------------------
DI = {r["flight"]: r for r in rd("co_diurnal_sensitivity.csv")}
order = ["20240703_R0_L1", "20240703_R0_L2", "20240709_R0", "20240713_R0_L1"]
fw = [float(DI[f]["factor_window"]) for f in order]; f3 = [float(DI[f]["factor_window3h"]) for f in order]
Ew = [float(DI[f]["E_window_t_hr"]) for f in order]; E3 = [float(DI[f]["E_window3h_t_hr"]) for f in order]
D = dict(fw_lo=min(fw), fw_hi=max(fw), f3_lo=min(f3), f3_hi=max(f3), ew_lo=min(Ew), ew_hi=max(Ew), ew_med=st.median(Ew),
         e3_lo=min(E3), e3_hi=max(E3), e3_med=st.median(E3),
         wk24=float(DI["20240703_R0_L1"]["co_24h_t_hr"]), sa24=float(DI["20240713_R0_L1"]["co_24h_t_hr"]))
print("diurnal:", D)
f1, f2 = (lambda x: f"{x:.1f}"), (lambda x: f"{x:.2f}")

OPS = {"main": [], "si": []}
def R(doc, old, new, count=1): OPS[doc].append(("R", old, new, count))
def RW(doc, anchor, text, drop_h=()): OPS[doc].append(("RW", anchor, text, drop_h))
M, SI = "main", "si"

# ================================ main ================================
R(M, "; the ethane-to-methane ratios here use the CH4 and C2H6 measured together by the Aeris.",
     "; the archived files report CH4 from the Aeris, the G2401 and a Picarro G2201i isotope analyzer, and the ethane-to-methane ratios here use the Aeris CH4 and C2H6, measured by the same analyzer.")
R(M, "Three of the four flights passing the reliability screen were on weekdays and 13 July L1 on a Saturday. ",
     "Three of the four flights passing the reliability screen were on weekdays and 13 July L1 on a Saturday. "
     f"GRA2PES CO also peaks in the afternoon; scaling each flight by the box CO of its sampling hours instead of the 24-hour mean would raise the estimates by factors of {f1(D['fw_lo'])} to {f1(D['fw_hi'])}, to {f1(D['ew_lo'])} to {f1(D['ew_hi'])} t CH4/h, "
     f"or to {f1(D['e3_lo'])} to {f1(D['e3_hi'])} t CH4/h if the 3 h before sampling are included (section S6). "
     "We keep the 24-hour mean, because the sampled boundary layer carries emissions accumulated over the preceding hours, and treat the difference as part of the anchor uncertainty. ")
R(M, "Replacing the wind cone with HRRR-driven STILT footprints convolved with the gridded GHGI, computed for the 25 urban level legs of the Vertical_Speed sensitivity definition (section S3), gives the same result (ρ = 0.02 between the leg slope and the modeled basin share; section S9, Figure S15).",
     f"Replacing the wind cone with HRRR-driven STILT footprints convolved with the gridded GHGI, for the {nT} urban transects on the {nF} flights with archived HRRR meteorology, gives the same result (ρ = {float(t_fog['rho']):.2f} between the transect slope and the modeled basin share; section S9, Figure S15).")
RW(M, "For the EPA inventory we made this footprint-based comparison.",
   "{{c33}}We made this footprint-based comparison for both inventories.{{/c33}} "
   f"HRRR-STILT footprints for {nT} urban transects on {nF} flights (section S9), convolved with each inventory, predict median transect ethane-to-methane slopes of {S['pe']:.3f} (EPA) and {S['pv']:.3f} mol mol−1 (GRA2PES v2.0beta), against {S['ob']:.3f} observed: "
   "once transport is accounted for, both inventories still imply a more fossil-rich mixture than the aircraft measured. "
   f"In the footprint-weighted v2.0beta, waste is a median {S['v2waste']}% of the predicted enhancement and oil and gas {S['v2og']}%, because the footprints also reach the oil-and-gas and agricultural emissions north and east of the city, whereas waste is three-quarters of its box total. "
   f"The transect-to-transect variation is not captured by either inventory (Spearman ρ = {float(t_pe['rho']):.2f} and {float(t_pv['rho']):.2f} between observed and predicted slopes).")
R(M, "HRRR-STILT footprints convolved with the EPA inventory attribute roughly 22 to 36% of the predicted urban enhancement to basin oil and gas (section S9).",
     f"HRRR-STILT footprints convolved with the EPA inventory attribute roughly {S['fog']} to {S['fog2']}% of the predicted urban enhancement to basin oil and gas (section S9).")
R(M, "which is not contemporaneous with the flights (GRA2PES, July 2023; EPA GHGI and NEI, 2020), and the bootstrap intervals",
     f"which is not contemporaneous with the flights (GRA2PES, July 2023; EPA GHGI and NEI, 2020) and whose time-of-day weighting is uncertain (the CO emitted during the sampling hours would raise the rates up to {f1(D['fw_hi'])}-fold; section S6), and the bootstrap intervals")
R(M, "A grid-resolved reconciliation of these top-down and bottom-up estimates is therefore a natural next step toward a calibrated Denver methane budget.",
     "Weighted by HRRR-STILT footprints, however, both inventories predict a more fossil-rich signature than observed (section 4.3), so the closer box-wide split of v2.0beta reflects where its waste emissions sit rather than the air the aircraft sampled. "
     "A grid-resolved reconciliation of these top-down and bottom-up estimates is therefore a natural next step toward a calibrated Denver methane budget.")

# ================================ SI ================================
RW(SI, "The cone uses the aircraft's own wind and a fixed geometry.",
   "The cone uses the aircraft's own wind and a fixed geometry. To test whether modeled transport changes the result, we replaced it with receptor-oriented Lagrangian footprints. "
   f"Four gated samples from each fitted urban transect served as receptors. From each, 500 particles were followed 10 h backward with STILT{{{{h0}}}} (HYSPLIT in STILT emulation{{{{h1}}}}) driven by the 3-km HRRR meteorology archived by NOAA ARL{{{{h2}}}}, including the near-field plume-dilution correction. "
   f"HRRR files covering the full 10 h were available for {nR} receptors on {nT} of the {nAll} fitted transects, on {nF} of the nine analysis flights; 5 July L1, 12 July L1 and one transect on each 13 July flight are not covered. "
   "The footprints were convolved with the gridded EPA GHGI fluxes of five source classes (basin-type oil and gas, natural-gas distribution and post-meter, waste, livestock, and all other sectors) and, separately, with the GRA2PES v2.0beta July 2023 weekday methane (oil and gas, post-meter, waste, agriculture and other sectors; 4-km cells mapped to a 0.02° grid). "
   "This gives each transect an inventory-predicted enhancement from each class, a basin oil-and-gas share of that enhancement, fOG, and an inventory-predicted ethane:methane slope, with source ratios of 0.102 for oil and gas and 0.110 for distribution and post-meter gas.")
RW(SI, "In the STILT results (Figure S15), basin oil and gas accounts for a median",
   f"In the STILT results with the EPA inventory (Figure S15), basin oil and gas accounts for a median {S['fog']}% of the predicted urban enhancement (range {S['fog_lo']} to {S['fog_hi']}%; {S['core']}% from the basin north of 40.05°N), or {S['fog2']}% if basin oil-and-gas emissions are doubled. "
   f"Waste accounts for {S['waste']}%, distribution and post-meter gas for {S['dist']}% and livestock for {S['live']}%. "
   f"As with the cone, the observed transect slope is uncorrelated with fOG (Spearman ρ = {float(t_fog['rho']):.2f}, p = {float(t_fog['p']):.2f}, n = {nT}; ρ = {float(t_fog_lo['rho']):.2f} for the {t_fog_lo['n']} transects below 1 km) and with the share from the basin north of 40.05°N (ρ = {float(t_core['rho']):.2f}, p = {float(t_core['p']):.2f}). "
   f"The median inventory-predicted slope is {S['pe']:.3f} mol mol−1 with the EPA inventory and {S['pv']:.3f} with GRA2PES v2.0beta, against an observed median of {S['ob']:.3f} for the same transects; "
   f"neither prediction tracks the observed transect-to-transect variation (ρ = {float(t_pe['rho']):.2f} and {float(t_pv['rho']):.2f}). "
   f"In the footprint-weighted v2.0beta, oil and gas is a median {S['v2og']}% of the predicted enhancement, waste {S['v2waste']}% and agriculture {S['v2ag']}%.")
RW(SI, "The model reproduces the contrast between legs below and above 1 km",
   f"The observed and EPA-predicted enhancements are weakly related across transects (ρ = {float(t_mag['rho']):.2f}, p = {float(t_mag['p']):.2f}). "
   f"The HRRR transport direction at the receptors differs from the aircraft wind by a median of {S['wdd']:.0f}°, so under the light winds of these flights the transect-level tests have little power. "
   f"The observed enhancement is a median {S['oe']:.1f} times the EPA-predicted enhancement (interquartile range {S['oe_q'][0]:.1f} to {S['oe_q'][1]:.1f}) and {S['ov']:.1f} times the v2.0beta-predicted enhancement ({S['ov_q'][0]:.1f} to {S['ov_q'][1]:.1f}); "
   f"v2.0beta predicts a median {S['v2e']:.1f} times the EPA enhancement at the same receptors. These ratios depend on the 10-h footprint length and on differences between the model and aircraft background definitions, so we do not read them as emission scalings. "
   f"We read the transport analysis as bounding the basin contribution in aggregate, at roughly {S['fog']} to {S['fog2']}% of the urban enhancement in the EPA inventory, and as showing that both inventories, weighted by transport, predict a more fossil-rich signature than the aircraft observed.")
RW(SI, "Figure S15: HRRR-STILT exposure of the 25 fitted urban level legs",
   f"**Figure S15:** HRRR-STILT footprints of the {nT} fitted urban transects with archived HRRR meteorology ({nR} receptors). "
   "(a) Transect ethane:methane slope against the basin oil-and-gas share of the enhancement predicted by convolving the footprints with the gridded EPA GHGI. "
   "(b) Observed mean gated enhancement against the enhancement predicted with the EPA GHGI (red) and GRA2PES v2.0beta (orange) (log axes; dashed line 1:1). "
   f"(c) Mean footprint of the {nR} receptors (log10), with the top decile of basin oil-and-gas cells outlined in red, the Denver-metro box in black and receptors as crosses. "
   "(d) Observed against inventory-predicted ethane:methane slope (dashed line 1:1; dotted line the observed median). "
   "Circles are transects below 1 km above ground, triangles above; symbol size scales with the number of gated samples; open symbols in (a) and points at the axis limits in (d) are weakly correlated transects whose slopes lie off scale.")
R(SI, "with the monthly mean for every flight the four estimates would be 4.4 to 10.3 t/h.",
     "with the monthly mean for every flight the four estimates would be 4.4 to 10.3 t/h. "
     f"GRA2PES CO also has a strong diurnal cycle: the box total falls to about 2 to 4 t/h overnight and rises to about 25 t/h in the early afternoon (local time), against 24-hour means of {f1(D['wk24'])} t/h on weekdays and {f1(D['sa24'])} t/h on Saturdays. "
     f"Scaling each flight instead by the box CO of its in-box sampling window would multiply the four estimates by {', '.join(f2(x) for x in fw[:3])} and {f2(fw[3])} (3 July L1 and L2, 9 July, 13 July L1), giving {f1(D['ew_lo'])} to {f1(D['ew_hi'])} t/h (median {f1(D['ew_med'])}); "
     f"including the 3 h before sampling gives factors of {f2(D['f3_lo'])} to {f2(D['f3_hi'])} and {f1(D['e3_lo'])} to {f1(D['e3_hi'])} t/h (median {f1(D['e3_med'])}). "
     "We keep the 24-hour mean as the primary anchor, because the sampled boundary layer carries emissions accumulated over the preceding hours and the CH4:CO enhancement ratio need not follow the instantaneous CO emission, but the absolute emission rates carry this additional time-of-day uncertainty.")

# ================================ apply ================================
def run_doc(src, dst, which, label):
    d = docx.Document(src); n = 0
    for op in OPS[which]:
        if op[0] == "R": O.do_replace(d, op[1], op[2], op[3], label)
        elif op[0] == "RW": O.do_rewrite(d, op[1], op[2], label, op[3])
        n += 1
    allp = L.all_paragraphs(d)
    m = L.subscript_chem(allp); e = L.exponent_fix(L.all_paragraphs(d)); c = L.cite_fix(L.all_paragraphs(d)); O.gra2pes_superscript(d)
    d.save(dst); print(f"{label}: {n} edit(s) -> {dst} ({m} subscripts, {e} exponents)")
run_doc(a.main_in, a.main_out, "main", "main text")
run_doc(a.si_in, a.si_out, "si", "SI")
O.swap_figures(a.si_out, {"image6.png": FIG / "FigS_stilt_exposure.png"}, {"image6.png"}, "SI")   # Figure S15: four panels now

# ---- checks ----------------------------------------------------------------------------------
dm = docx.Document(a.main_out); tm = "\n".join(O.vis_text(p) for p in L.all_paragraphs(dm))
ds = docx.Document(a.si_out); ts = "\n".join(O.vis_text(p) for p in L.all_paragraphs(ds))
for b in ("Vertical_Speed sensitivity definition (section S3), gives", "22 to 36%", "measured together by the Aeris", "For the EPA inventory we made"):
    assert b not in tm, f"main: {b!r} remains"
for b in ("25 fitted urban level legs", "25 urban level legs", "0.041 mol", "3.7 times", "22 to 36%"):
    assert b not in ts, f"SI: {b!r} remains"
x = zipfile.ZipFile(a.main_out).read("word/document.xml").decode(); xc = zipfile.ZipFile(a.main_out).read("word/comments.xml").decode()
assert set(re.findall(r'<w:commentReference w:id="(\d+)"', x)) == set(re.findall(r'<w:comment [^>]*?w:id="(\d+)"', xc))
print("checks passed")
