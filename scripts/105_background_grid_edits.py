"""105_background_grid_edits.py -- adds the background-definition sweep (script 104) to the
manuscript (section 3.2) and the SI (section S4, new figure after Figure S7), and a pointer to
the interactive explorer in the SI and the Data and Code Availability statement. Works on the
output of script 103. Numbers are read from background_grid_sensitivity.csv and asserted. New
text is red. SI figures after the new one are renumbered (same routine as script 75).

usage: python3 scripts/105_background_grid_edits.py <main.docx> <si.docx> <main_out> <si_out>
           --outputs <OUT_DIR> [--figures <OUT_DIR>/figures] [--app-url URL]
"""
import sys, re, csv, argparse, statistics as st
from pathlib import Path
sys.path.insert(0, str(Path(__file__).resolve().parent))
import docx, docx.oxml.ns
import docx_edit_lib as L
from docx_edit_lib import I, R, runs_text, all_paragraphs, replace_in_paragraph
import docx_ops as O

ap = argparse.ArgumentParser()
for k in ("main_in", "si_in", "main_out", "si_out"): ap.add_argument(k)
ap.add_argument("--outputs", required=True); ap.add_argument("--figures", default=None)
ap.add_argument("--png", default="FigS_background_grid.png")
ap.add_argument("--app-url", default="[URL of the interactive explorer]")
a = ap.parse_args(); OUT = Path(a.outputs); L.FIGDIR = a.figures or str(OUT / "figures"); L.OUTDIR = str(OUT)

G = list(csv.DictReader(open(OUT / "background_grid_sensitivity.csv")))
S = list(csv.DictReader(open(OUT / "background_grid_summary.csv")))
M = list(csv.DictReader(open(OUT / "background_mode_sensitivity.csv")))
num = lambda v: float(v) if v not in ("", "NA") else float("nan")
med = [int(float(r["median_fossil_pct"])) for r in S]; n_set = len(S)
assert n_set == 45 and (min(med), max(med)) == (18, 33), (n_set, min(med), max(med))
n_le26 = sum(m <= 26 for m in med); assert n_le26 == 27, n_le26
const = [int(float(r["median_fossil_pct"])) for r in S if r["window_samples"] == "Inf"]; assert (min(const), max(const)) == (22, 26)
wide = [int(float(r["median_fossil_pct"])) for r in S if r["window_samples"] not in ("60", "120", "180", "300", "Inf") and r["percentile"] != "25"]
assert (min(wide), max(wide)) == (18, 29), (min(wide), max(wide))
s180 = [int(float(r["median_fossil_pct"])) for r in S if r["window_samples"] in ("120", "180") and r["percentile"] != "25"]; assert (min(s180), max(s180)) == (27, 33)
rng = {}
for f in sorted(set(r["flight"] for r in G)):
    v = [num(r["fossil_pct"]) for r in G if r["flight"] == f]; v = [x for x in v if x == x]
    rng[f] = (int(min(v)), int(max(v)), len(v))
assert rng["20240703_R0_L2"] == (0, 3, 45) and rng["20240709_R0"] == (29, 38, 45) and rng["20240703_R0_L1"] == (17, 31, 45)
assert rng["20240713_R0_L1"] == (31, 48, 45) and rng["20240713_R0_L2"] == (21, 46, 45) and rng["20240712_R0_L1"] == (10, 35, 45)
assert rng["20240708_R0_L1"] == (0, 100, 45) and rng["20240710_R0_L1"] == (0, 25, 45) and rng["20240705_R0_L1"] == (0, 26, 39)
hi = [r for r in G if r["fossil_pct"] not in ("", "NA") and float(r["fossil_pct"]) >= 50]
assert hi and all(r["flight"] == "20240708_R0_L1" for r in hi), set(r["flight"] for r in hi)
hg = [int(r["n_gated"]) for r in hi]; hl = sorted(set(int(r["n_legs"]) for r in hi))
assert (min(hg), max(hg)) == (50, 186) and hl == [3, 4], (hg, hl)
paper8 = [int(r["n_gated"]) for r in G if r["flight"] == "20240708_R0_L1" and r["window_samples"] == "300" and r["percentile"] == "5"][0]; assert paper8 == 273
no60 = [r["flight"] for r in G if r["window_samples"] == "60" and r["fossil_pct"] in ("", "NA")]; assert no60 == ["20240705_R0_L1"] * 5, no60
T1 = ["20240703_R0_L1", "20240703_R0_L2", "20240705_R0_L1", "20240708_R0_L1", "20240709_R0", "20240710_R0_L1", "20240712_R0_L1", "20240713_R0_L1", "20240713_R0_L2"]
track = [int(float(r["fossil_pct_track"])) for r in M if r["flight"] in T1]; assert len(track) == 9 and st.median(track) == 22

MAIN_ADD = (" Because each transect has its own intercept in the within-transect fit (section 3.3), the background enters the fossil fraction only through "
            "which samples pass the 20 ppb gate and through along-transect structure on scales longer than the window: running the window along the "
            f"continuous flight record instead gives a campaign median of {st.median(track):.0f}%, and sweeping the window from 60 to 1,800 samples and the "
            f"percentile from 1 to 25%, or taking one percentile of all of a flight's urban-transect samples, keeps it between {min(med)} and {max(med)}% "
            "(section S4, Figure S99), with the largest changes on the three flights whose intervals in Table 1 are widest.")
SI_PARA = (
    "To see how far the two parameters of the rolling background matter, we recomputed each flight's fossil fraction over a grid of window widths "
    "(60 to 1,800 samples, 1 to 30 min of flight) and percentiles (1 to 25%), and with one constant background per flight (the same percentiles of all of "
    "the flight's urban-transect samples), keeping the 20 ppb gate and the within-transect fit unchanged (Figure S99; the fit at every setting, and the "
    "background it implies on each flight, can be inspected in the interactive explorer, see Data and Code Availability). "
    f"The campaign median stays between {min(med)} and {max(med)}% over the {n_set} settings ({min(const)} to {max(const)}% with a constant background) and is "
    f"at or below 26% in {n_le26} of them; windows wider than the paper's with percentiles up to 10% give {min(wide)} to {max(wide)}%, whereas windows of 120 to "
    f"180 samples with the same percentiles give {min(s180)} to {max(s180)}%, because a short window follows the signal more closely, removes the broad enhancement and leaves the fit to "
    "the narrow plumes, whose ethane content is higher on several flights. Four flights keep their character at every setting (3 July L2 "
    f"{rng['20240703_R0_L2'][0]} to {rng['20240703_R0_L2'][1]}%, 3 July L1 {rng['20240703_R0_L1'][0]} to {rng['20240703_R0_L1'][1]}%, 9 July {rng['20240709_R0'][0]} to "
    f"{rng['20240709_R0'][1]}%, 13 July L1 {rng['20240713_R0_L1'][0]} to {rng['20240713_R0_L1'][1]}%). The three flights that move most are the three with the widest "
    f"intervals in Table 1: 8 July L1 spans {rng['20240708_R0_L1'][0]} to {rng['20240708_R0_L1'][1]}%, 5 July L1 {rng['20240705_R0_L1'][0]} to "
    f"{rng['20240705_R0_L1'][1]}% (no fit at the 60-sample window) and 10 July L1 {rng['20240710_R0_L1'][0]} to {rng['20240710_R0_L1'][1]}%; 12 July L1 spans "
    f"{rng['20240712_R0_L1'][0]} to {rng['20240712_R0_L1'][1]}% and 13 July L2 {rng['20240713_R0_L2'][0]} to {rng['20240713_R0_L2'][1]}%. "
    f"Every single-flight value above 50% occurs on 8 July L1, at settings that leave it {min(hg)} to {max(hg)} gated samples ({paper8} in Table 1) on three or "
    "four transects, none of which shows a meaningful ethane-methane relationship at the paper's setting (r = -0.35 to 0.23; section S3). The sweep therefore supports the same reading as the two alternative constructions above: "
    "the background definition moves individual flights, above all those already flagged as poorly constrained, but not the conclusion that the campaign "
    "is biogenic-dominated at the adopted endmember.")
CAPTION = (
    "Figure S99: Sensitivity of the urban fossil fraction to the background definition. Left: campaign median over the nine flights of Table 1 for each "
    "combination of background window (number of 1 Hz samples; const: one percentile of all of the flight's urban-transect samples) and percentile; the "
    "black box marks the paper's setting (300 samples, 5th percentile). Right: each flight's fossil fraction against the window at the 5th percentile, "
    "with the campaign median (thick black line); 5 July L1 has no fit at 60 samples. Dotted line: even fossil-biogenic split. All settings use the 20 ppb "
    "gate and the within-transect York fit of Table 1 at the adopted endmember of 0.102 mol mol-1.")
AVAIL = (f" An interactive explorer of the fits under alternative endmembers and background definitions, built from the same outputs "
         f"(the shiny folder of the repository), is published at {a.app_url}.")

if any("we recomputed each flight's fossil fraction over a grid of window widths" in p.text for p in docx.Document(a.si_in).paragraphs):
    sys.exit("the background sweep is already in this SI; run on the output of script 103")
R("and no flight reaches 50% (section S4).", "and no flight reaches 50% (section S4)." + MAIN_ADD, "main")
R("as it does with the assumed endmember (sections S3-S4).",
  f"as it does with the assumed endmember (sections S3-S4), and the campaign median stays between {min(med)} and {max(med)}% over the background definitions of section 3.2.", "main")
R("We therefore treat the two backgrounds as bracketing the campaign median (21 to 29%) rather than adopting the aloft one.",
  f"We therefore treat the two backgrounds as bracketing the campaign median (21 to 29%; the sweep below widens this to {min(med)} to {max(med)}%) rather than adopting the aloft one.", "si")
R("S4. Robustness of the fossil fraction to the plume threshold/gate", "S4. Robustness of the fossil fraction to the plume threshold/gate and the background", "si")
R("archived as PDF copies and stored as psco_gas_quality.csv.", "archived as PDF copies and stored as psco_gas_quality.csv." + AVAIL, "main")
I("although 13 July L1 then falls just below the 0.7 correlation criterion (r = 0.68).", SI_PARA, "si")
# the figure goes after Figure S7 (threshold sensitivity), whose caption closes section S4's figures
I("which is the same composition effect seen in the left panel.", f"[[FIG:{a.png}]]\n\n{CAPTION}", "si")
L.apply(a.main_in, a.main_out, "main", "main text")
L.apply(a.si_in, a.si_out, "si", "SI")

# ---- renumber figures by physical order, in both documents (as script 75) ------------------
main = docx.Document(a.main_out); si = docx.Document(a.si_out)
CAP_FIG = re.compile(r"^\s*Figure\s+S(\d+)([a-z]?)\s*[:.]")
fig_map = {}
for p in si.paragraphs:
    m = CAP_FIG.match(p.text)
    if m:
        key = m.group(1) + m.group(2)
        if key not in fig_map: fig_map[key] = str(len(fig_map) + 1)
print("figure map:", ", ".join(f"S{o}->S{n}" for o, n in fig_map.items() if o != n) or "identity")
TOK = r"S\d+[a-z]?"
LIST = r"(?:%s(?:\s*(?:,\s*and|,|and|to|-|–)\s*%s)*)" % (TOK, TOK)
REF_RE = re.compile(r"(Figures?)(\s+)(%s)" % LIST)
CAPF_RE = re.compile(r"^(Figure\s+S)(\d+[a-z]?)(\s*[:.])")
def map_tok(tok):
    m = re.match(r"S(\d+)([a-z]?)", tok); return "S" + fig_map.get(m.group(1) + m.group(2), m.group(1) + m.group(2))
def map_list(lst):
    parts = re.split(r"(\s*(?:,\s*and|,|and|to|-|–)\s*)", lst)
    out = "".join(map_tok(t) if re.fullmatch(TOK, t) else t for t in parts)
    if not re.search(r"[-–]|\bto\b", lst):
        nums = re.findall(TOK, out); key = lambda t: (int(re.sub(r"[a-z]", "", t[1:])), t)
        if nums != sorted(nums, key=key):
            it = iter(sorted(nums, key=key)); out = re.sub(TOK, lambda m: next(it), out)
    return out
def rewrite(p):
    text = runs_text(p); edits = {}
    m = CAPF_RE.match(text)
    if m: edits[0] = (m.group(0), m.group(1) + fig_map.get(m.group(2), m.group(2)) + m.group(3))
    for m in REF_RE.finditer(text):
        if m.start() in edits: continue
        edits[m.start()] = (m.group(0), m.group(1) + m.group(2) + map_list(m.group(3)))
    n = 0; pos = 0
    for _, (old, new) in sorted(edits.items()):
        if old == new: pos = runs_text(p).find(old, pos) + len(old); continue
        pos2 = replace_in_paragraph(p, old, new, pos)
        if pos2 < 0: raise SystemExit("could not place %r in: %s" % (old, text[:80]))
        pos = pos2; n += 1
    return n
n_main = sum(rewrite(p) for p in all_paragraphs(main)); n_si = sum(rewrite(p) for p in all_paragraphs(si))
print(f"rewrote {n_main} figure reference(s) in the main text and {n_si} in the SI")
figs_now = ["S" + m.group(1) + m.group(2) for p in si.paragraphs for m in [CAP_FIG.match(p.text)] if m]
print("SI figures now:", figs_now)
missing = set()
for p in all_paragraphs(main) + all_paragraphs(si):
    for m in REF_RE.finditer(p.text):
        for t in re.findall(TOK, m.group(3)):
            if t not in figs_now: missing.add(t)
if missing: print("WARNING: references to figures that do not exist:", sorted(missing))
# caption formatting as the other SI captions: italic, "Figure S8:" bold
for p in si.paragraphs:
    if p.text.startswith("Figure S") and "background definition" in p.text:
        for r in p.runs: r.italic = True
        lead = p.runs[0] if p.runs[0].text.strip() else p.runs[1]
        if lead.text.startswith("Figure S") and ":" in lead.text:
            head, tail = lead.text.split(":", 1); lead.text = head + ":"; lead.bold = True
            nr = p.add_run(tail); nr.italic = True; nr.font.color.rgb = lead.font.color.rgb
            lead._r.addnext(nr._r)
# the SI separates blocks with empty paragraphs: put one between Figure S7's caption and the new figure
import copy as _copy
def _spacer_before(par):
    sp = _copy.deepcopy(par._p)
    for ch in list(sp):
        if ch.tag != docx.oxml.ns.qn("w:pPr"): sp.remove(ch)
    par._p.addprevious(sp)
_ps = si.paragraphs
for _i, _p in enumerate(_ps):
    if _p.text.startswith("Figure S") and "background definition" in _p.text and _ps[_i - 1]._p.xpath(".//w:drawing"):
        _spacer_before(_ps[_i - 1]); break
L.subscript_chem(all_paragraphs(main)); L.subscript_chem(all_paragraphs(si))
main.save(a.main_out); si.save(a.si_out)
tm = "\n".join(O.vis_text(p) for p in all_paragraphs(docx.Document(a.main_out)))
ts = "\n".join(O.vis_text(p) for p in all_paragraphs(docx.Document(a.si_out)))
assert tm.count("sweeping the window from 60 to 1,800 samples") == 1 and ts.count("over a grid of window widths") == 1 and "S99" not in tm + ts
print("background sweep added: main 3.2, Data and Code Availability, SI S4 + figure")
