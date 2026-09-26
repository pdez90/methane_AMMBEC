"""70_lag_precision_edits.py -- carry the ethane timing-lag and precision audit (script 69) into the
manuscript and SI.

SI: new subsection S4.1 after Figure S6 (method, numbers, new Figure S7 = FigS_lag_sensitivity.png);
the old Figures S7-S15 become S8-S16 in both documents. Main text: three sentences at the end of the
section 3.2 paragraph on the 20 ppb gate. Every number is read from the script-69 outputs; every edit
is an exact-anchor operation written in red (docx_edit_lib.py), so nothing is written if an anchor is
missing or duplicated.

usage: python3 scripts/70_lag_precision_edits.py <main.docx> <si.docx> <main_out> <si_out>
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
num = lambda v: float(v) if v not in ("", "NA") else None

EST = rows("lag_estimates.csv"); SUM = {int(r["lag_s"]): r for r in rows("lag_sensitivity_summary.csv")}
FL = [r for r in rows("lag_sensitivity_flights.csv") if r["region"] == "urban" and num(r["fossil_pct"]) is not None]
PR = rows("ethane_precision_sensitivity.csv")

wd = [r for r in EST if r["well_defined_all"] == "TRUE"]; wb = [r for r in EST if r["well_defined_basin"] == "TRUE"]
lags = [int(float(r["lag_all"])) for r in wd]; LAG = int(st.median(lags)); LAGB = st.median([float(r["lag_basin"]) for r in wb])
assert LAG == LAGB, "basin-only and all-leg lags differ; revise the text"
med = {k: float(SUM[k]["median_fossil_pct"]) for k in SUM}
assert all(int(SUM[k]["n_majority_fossil"]) == 0 for k in SUM), "a flight is majority fossil at some shift; revise the text"
assert all(int(SUM[k]["n_flights"]) == 7 for k in SUM)
f0 = {r["flight"]: num(r["fossil_pct"]) for r in FL if int(r["lag_s"]) == 0}
fL = {r["flight"]: num(r["fossil_pct"]) for r in FL if int(r["lag_s"]) == LAG}
dl = {f: round(fL[f]) - round(f0[f]) for f in f0}
big = max(dl, key=lambda f: abs(dl[f])); others = max(abs(v) for f, v in dl.items() if f != big)
assert big == "20240703_R0_L1", "largest per-flight change is not 3 July L1; revise the text"
bas = {k: float(SUM[k]["median_basin_ratio"]) for k in SUM}
core = [bas[k] for k in range(-3, 3)]; tails = [bas[k] for k in SUM if k < -3 or k > 2]
sig = [float(r["sigma_C2H6_empirical"]) for r in EST]
pe = st.median(float(r["fossil_pct_sy_empirical"]) for r in PR); p5 = st.median(float(r["fossil_pct_sy0.5"]) for r in PR)
dpe = max(abs(float(r["fossil_pct_sy_empirical"]) - float(r["fossil_pct_sy0.2"])) for r in PR)
p = lambda x: f"{x:.0f}"; dmed = abs(round(med[LAG]) - round(med[0]))
WORDS = {n: w for n, w in enumerate("Zero One Two Three Four Five Six Seven Eight Nine Ten Eleven Twelve Thirteen Fourteen Fifteen Sixteen Seventeen Eighteen Nineteen Twenty".split())}
sgn = lambda k: f"{k:+d}".replace("-", "−")
rng = lambda lo, hi: f"{lo:.3f} to {hi:.3f}"
print(f"lag {LAG} s from {len(wd)} flights ({min(lags)}..{max(lags)}), basin {LAGB} ({len(wb)}); medians {med}")

# ---- renumber the SI figures after S6 (descending, so no number is renamed twice) ------------------
for n, (si_c, main_c) in zip(range(15, 6, -1), [(2, 1), (1, 1), (1, 1), (1, 1), (1, 1), (1, 1), (1, 1), (2, 0), (2, 0)]):
    R(f"Figure S{n}", f"Figure S{n + 1}", "si", si_c)
    if main_c: R(f"Figure S{n}", f"Figure S{n + 1}", "main", main_c)

# ---- SI: new subsection S4.1 after Figure S6 --------------------------------------------------------
si_text = (
  "[[H2]] S4.1 Instrument timing and ethane precision"
  "\n\n"
  "CH4 is measured by the Picarro G2401-m and C2H6 by the Aeris analyzer, on separate sample lines, and our processing applies "
  "no relative lag correction between them. Because the fits pair samples second by second, a relative offset "
  "attenuates the slope in narrow plumes (at the aircraft's ~50 m s-1, 2 s corresponds to ~100 m of track). We estimated the "
  "offset per flight by cross-correlating the 61 s high-passed CH4 and C2H6 records over trial lags of −15 to +15 s, using only "
  "the 60 s windows of level flight in the top 15% of methane variance, so that the lag is set by large, unambiguous "
  f"co-enhancements rather than fitted plume by plume. {WORDS[len(wd)]} of the {len(EST)} flights give a well-defined peak (r ≥ 0.3 and "
  f"at least 0.05 above lags 3 to 6 s away); their median is {sgn(LAG)} s, meaning ethane is recorded {abs(LAG)} s before methane "
  f"(range {sgn(min(lags))} to {sgn(max(lags))} s). Restricting the windows to DJB basin legs, where the ethane signal is strongest, "
  f"gives the same {sgn(LAG)} s ({WORDS[len(wb)].lower()} flights)."
  "\n\n"
  "We then shifted the ethane record by each integer lag from −5 to +5 s, before leg selection, background estimation and gating, "
  "and repeated the within-leg York fit of section 3.2 unchanged; at zero shift the procedure reproduces Table 1 on all seven flights. "
  f"The campaign median is {p(med[0])}% at 0 s, {p(med[LAG])}% at the estimated {sgn(LAG)} s and {p(med[-2])}% at −2 s. Per flight, "
  f"the correction changes the fossil fraction by {abs(dl[big])} percentage points on 3 July L1 ({p(f0[big])} to {p(fL[big])}%) and "
  f"by at most {others} points on the other six flights. Shifts beyond the estimated lag attenuate the slope, as expected when the "
  f"two records are misaligned: the median falls to {p(med[1])}% at +1 s and to {p(min(med[-5], med[5]))} to {p(max(med[-5], med[5]))}% "
  "at ±5 s, and no flight exceeds 50% fossil at any shift. The median DJB basin ratio behaves the same way, holding at "
  f"{rng(min(core), max(core))} mol mol-1 from −3 to +2 s and falling to {rng(min(tails), max(tails))} beyond. The uncorrected "
  f"alignment therefore lies within {dmed} percentage point{'' if dmed == 1 else 's'} of the best-aligned result, and the adopted analysis "
  "is left uncorrected."
  "\n\n"
  "The York fit assigns ethane a 1σ precision of 0.2 ppb, the value in the data files. The in-flight 1 Hz scatter, estimated per "
  "flight as the median absolute deviation of the 61 s high-passed ethane record at quiet level-leg samples (|high-passed ΔCH4| < 5 ppb), "
  f"is {st.median(sig):.2f} ppb ({min(sig):.2f} to {max(sig):.2f} ppb across {len(sig)} flights); because it includes sub-minute "
  "atmospheric variability, it is an upper bound on instrument noise. Refitting with each flight's empirical value leaves the "
  f"campaign median at {p(pe)}% (largest per-flight change {dpe:.1f} points), and a stress value of 0.5 ppb gives {p(p5)}%."
  "\n\n[[FIG:FigS_lag_sensitivity.png]]\n\n"
  "Figure S7: Sensitivity of the urban fossil fraction to the relative timing of the ethane and methane records. Left: fossil fraction "
  "against the time shift applied to the ethane record (negative values read ethane earlier), for each flight (thin gray) and the "
  f"campaign median (orange); the shaded band marks the estimated {sgn(LAG)} s offset and the dotted line the even fossil-biogenic split. "
  "Right: median DJB basin ethane-to-methane ratio over all basin flights against the same shift. The fossil fraction peaks at the "
  "estimated offset, and both quantities decline as the records are misaligned further.")
I("Figure S6: Sensitivity of the urban fossil fraction to the methane plume threshold/gate.", si_text, "si")

# ---- main text, section 3.2 ----------------------------------------------------------------------
R("(sections S3-S4).",
  "(sections S3-S4). Methane and ethane are measured by separate analyzers, so we also "
  f"tested the time alignment of the two records: ethane leads methane by about {abs(LAG)} s, correcting for this changes the "
  f"campaign median fossil fraction from {p(med[0])} to {p(med[LAG])}%, and no flight becomes majority fossil for any shift between "
  f"−5 and +5 s (section S4.1, Figure S7). The in-flight 1 Hz ethane scatter ({st.median(sig):.2f} ppb) is consistent with the "
  "0.2 ppb precision assumed in the fit.")

L.apply(a.main_in, a.main_out, "main", "main text")
L.apply(a.si_in, a.si_out, "si", "SI")
