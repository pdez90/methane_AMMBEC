#!/usr/bin/env python3
"""116_abstract_200_words.py -- the abstract shortened to the 150-200 words ES&T asks for.

Replaces the abstract paragraph (in red; co-authors' comment markers stay on the paragraph, their
tracked insertions and deletions inside it go, because the paragraph is rewritten whole) with
a 198-word version that keeps every number of the longer one and adds the upper bound of
section 3.2 / SI S13.1 (39 to 44% when all methane and ethane above each flight's cleanest air
are attributed to the city). The numbers are checked against the outputs.

Run: python3 scripts/116_abstract_200_words.py MAIN_IN MAIN_OUT --outputs OUT_DIR
"""
import sys, csv, argparse, statistics as st
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent))
import docx
import docx_edit_lib as L
from docx_tc import tc_text, _replace_once

ap = argparse.ArgumentParser(); ap.add_argument("main_in"); ap.add_argument("main_out"); ap.add_argument("--outputs", required=True)
a = ap.parse_args(); OUT = Path(a.outputs)
rd = lambda f: list(csv.DictReader(open(OUT / f)))

ABSTRACT = ("Urban methane comes from natural-gas systems and biogenic sources such as landfills and wastewater. Fossil methane co-emits ethane and biogenic methane does not, "
  "so ethane-to-methane ratios separate them. We report, to our knowledge, the first airborne estimate of this split for the Denver metropolitan area, separate from the adjacent "
  "Denver-Julesburg oil-and-gas basin. We used 2024 Airborne Methane Mass Balance Emissions in Colorado (AMMBEC) flights designed around the basin and a method needing no closed flight path. "
  "At a fossil endmember of 0.102 mol mol−1, the median fossil fraction over nine flights was 21% (0 to 35%), or 19 to 26% across reference endmembers, below the EPA inventory's ~60%. "
  "Attributing all methane and ethane above each flight's cleanest air to Denver, including air carried in from the basin, gives at most 39 to 44%. "
  "Scaling CH4:CO enhancement ratios by a CO inventory gave 4.6 to 9.6 t CH4/h (median 6.7) on four flights, about 2.4 times the EPA inventory (2.8 t CH4/h) but below GRA2PES v2.0beta (16.5 t CH4/h). "
  "These rates are less certain than the split. Biogenic sources, not the natural-gas system, dominated summertime methane over Denver; mitigation focused only on gas delivery would miss most of it.")
assert 150 <= len(ABSTRACT.split()) <= 200, len(ABSTRACT.split())

# the numbers
T1 = [r for r in rd("table1.csv") if r["Fossil_pct_CI"] != "NA"]; ff = [float(r["Fossil_pct_CI"].split()[0]) for r in T1]
assert len(ff) == 9 and (st.median(ff), min(ff), max(ff)) == (21, 0, 35), ff
em = [float(r["CH4CO_GRA2PES_t_hr"].split()[0]) for r in T1 if r["CH4CO_GRA2PES_t_hr"] != "not usable"]
assert len(em) == 4 and (min(em), max(em)) == (4.6, 9.6) and abs(st.median(em) - 6.7) < 0.051 and round(6.7 / 2.8, 1) == 2.4, em
E = rd("fixed_background_estimators.csv")
rt = [st.median([float(r[f"fossil_ratio_{b}"]) for r in E if r[f"fossil_ratio_{b}"] not in ("", "NA")]) for b in ("p01flight", "p05urban", "aloft")]
assert (min(rt), max(rt)) == (39, 44), rt

d = docx.Document(a.main_in)
ps = d.paragraphs; i = next(k for k, p in enumerate(ps) if tc_text(p).strip() == "Abstract")
p = ps[i + 1]; old = tc_text(p)
assert old.startswith("A city's methane emissions arise") or old.startswith("Urban methane comes from"), old[:60]
_replace_once(p, old, ABSTRACT, "abstract")
# the paragraph is rewritten whole, so co-authors' tracked deletions inside it no longer have a context: drop them
from docx.oxml.ns import qn
ndel = 0
for e in list(p._p.iter(qn("w:del"))) + list(p._p.iter(qn("w:moveFrom"))):
    if e.getparent() is not None and e.getparent().tag != qn("w:rPr"): e.getparent().remove(e); ndel += 1
L.subscript_chem([p]); L.exponent_fix([p])
assert tc_text(p) == ABSTRACT, "abstract text differs after the replacement"
d.save(a.main_out); print(f"abstract replaced: {len(old.split())} -> {len(ABSTRACT.split())} words; {ndel} tracked deletion(s) inside it dropped; wrote {a.main_out}")
