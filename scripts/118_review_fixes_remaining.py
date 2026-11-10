#!/usr/bin/env python3
"""118_review_fixes_remaining.py -- the review fixes of script 117 that had not been carried into
the working copies, applied only where the old text is still present (safe to run again).

Main: author affiliation numbers, headings 2.2 / 3.1 / 3.2, "the G2401", "of CH4", "sections 3.3
and S5", minus signs in exponents, Figure 1. SI: "20-min", "50-m", Table S9 distance (1.6 km),
Figures S2, S11 and S12 (wastewater plant at its EPA facility position).

Run: python3 scripts/118_review_fixes_remaining.py MAIN_IN SI_IN MAIN_OUT SI_OUT --outputs OUT_DIR
"""
import sys, re, argparse, zipfile
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent))
import docx
from docx.oxml.ns import qn
import docx_edit_lib as L, docx_ops as O
from docx_tc import tc_replace, tc_find, tc_text, tc_insert_at
RED = L.RED

ap = argparse.ArgumentParser()
for k in ("main_in", "si_in", "main_out", "si_out"): ap.add_argument(k)
ap.add_argument("--outputs", required=True)
a = ap.parse_args(); FIG = Path(a.outputs) / "figures"
main = docx.Document(a.main_in); si = docx.Document(a.si_in); done = []

def ensure(d, name, old, new, label):
    """replace old by new if old is still there; accept the document if new is already there"""
    txt = "\n".join(tc_text(p) for p in L.all_paragraphs(d))
    if old in txt: tc_replace(d, old, new, f"{name} {label}"); done.append(f"{name}: {label}")
    elif new not in txt: sys.exit(f"{name} {label}: neither the old nor the corrected text was found")

# ---- main ---------------------------------------------------------------------------------------
want = {"Sunil Baidar": "8", "Xinrong Ren": "4", "Phillip Stratton": "5", "Joost de Gouw": "6,7"}
p = tc_find(main, "Priyanka deSouza", "authors"); runs = p.runs
for i, r in enumerate(runs[:-1]):
    for nm, sup in want.items():
        if r.text.strip(" ,").endswith(nm) and runs[i + 1].font.superscript and runs[i + 1].text != sup:
            runs[i + 1].text = sup; runs[i + 1].font.color.rgb = RED; done.append(f"main: affiliation of {nm} -> {sup}")
ensure(main, "main", "2.2.CDPHE facility surveys", "2.2. CDPHE facility surveys", "heading 2.2")
for h in ("3.1 Constant-altitude flight transects", "3.2 Background and enhancement"):
    hp = [q for q in main.paragraphs if tc_text(q).strip() == h]
    if hp: tc_replace(main, h, h[:3] + "." + h[3:], "main heading " + h[:3]); done.append("main: heading " + h[:3])
ensure(main, "main", "from G2401 CH4, which has", "from the G2401 CH4, which has", "the G2401")
if any("concentrations ofCH4" in tc_text(q) for q in main.paragraphs):
    tc_insert_at(main, "CH4, C2H6, CO, and CO2 for the aircraft, CH4 and C2H6 for the ARL mobile laboratory", " ", "main ofCH4"); done.append("main: of CH4")
ensure(main, "main", "; sections 3.3, section S5)", "; sections 3.3 and S5)", "sections 3.3 and S5")

# ---- SI -----------------------------------------------------------------------------------------
ensure(si, "SI", "For each 20- min profile", "For each 20-min profile", "20-min")
ensure(si, "SI", "spaced at 50- m intervals", "spaced at 50-m intervals", "50-m")
t9 = si.tables[8]; assert t9.rows[3].cells[0].text.strip() == "3", [c.text for c in t9.rows[3].cells]
if t9.rows[3].cells[2].text.strip() == "2.5": O.set_cell(t9.rows[3].cells[2], "1.6"); done.append("SI: Table S9 distance 2.5 -> 1.6 km")
assert t9.rows[3].cells[2].text.strip() == "1.6"

# ---- exponents typed with a hyphen ------------------------------------------------------------------
nexp = 0
for d in (main, si):
    for q in L.all_paragraphs(d):
        for r in q._p.iter(qn("w:r")):
            rp = r.find(qn("w:rPr")); va = rp.find(qn("w:vertAlign")) if rp is not None else None
            if va is None or va.get(qn("w:val")) != "superscript": continue
            for t in r.iter(qn("w:t")):
                if t.text and re.fullmatch(r"-[123]", t.text): t.text = "−" + t.text[1:]; nexp += 1
for d in (main, si):
    ps = L.all_paragraphs(d); L.subscript_chem(ps); L.exponent_fix(ps)
main.save(a.main_out); si.save(a.si_out)

# ---- figures ------------------------------------------------------------------------------------
def media_in_order(path):
    z = zipfile.ZipFile(path); x = z.read("word/document.xml").decode(); rels = z.read("word/_rels/document.xml.rels").decode(); out = []
    for m in re.finditer(r'r:embed="(rId\d+)"', x):
        t = re.search(r'Id="%s"[^>]*Target="([^"]+)"' % m.group(1), rels) or re.search(r'Target="([^"]+)"[^>]*Id="%s"' % m.group(1), rels)
        out.append(t.group(1).split("/")[-1])
    return out
mm = media_in_order(a.main_out); sm = media_in_order(a.si_out); assert len(mm) == 4 and len(sm) == 19, (len(mm), len(sm))
O.swap_figures(a.main_out, {mm[0]: FIG / "Fig1_study_map_sources.png"}, set(), "main Figure 1")
O.swap_figures(a.si_out, {sm[1]: FIG / "FigS4_mobile_plumes.png", sm[10]: FIG / "FigS9_biogenic_sources.png", sm[11]: FIG / "FigS13_spatial_match.png"}, set(), "SI Figures S2, S11, S12")
print("\n".join(done)); print(f"{nexp} exponent(s) given a minus sign; wrote {a.main_out} and {a.si_out}")
