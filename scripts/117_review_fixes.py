#!/usr/bin/env python3
"""117_review_fixes.py -- fixes from the review of 11 November 2026 (main text and SI), in red.

1. The Metro Water Recovery (Robert W. Hite) wastewater plant was placed at 39.792 N, 104.938 W
   in biogenic_sources.csv and script 24, a point 2.4 km south-east of the plant. Its position
   in the EPA Risk Management Plan facility record (6450 York Street) is 39.8111 N, 104.9536 W.
   Consequences corrected here:
     * the GRA2PES v2.0beta waste cell with 0.39 t CH4/h CONTAINS the plant (1.6 km from the cell
       centre); main 4.3 and SI S11 said it lay north of the plant and that the plant's own cell
       held no waste methane (Table S9 distance 2.5 -> 1.6 km);
     * SI S12 and Table S11 (facility separation) are rewritten from the rerun of script 24;
     * the plant's share of the allocated wastewater methane is stated from the EPA cell that
       contains it (37% of the in-box domestic wastewater methane);
     * Figures 1, S2, S11 and S12 are redrawn with the plant in the right place.
2. Author affiliation numbers that no longer matched the renumbered affiliation list.
3. Slips: "aircraft windows", headings 2.2 / 3.1 / 3.2, "ofCH4", "from G2401", "20- min",
   "50- m", a hyphen used as a minus sign in exponents.
4. SI S1.1 still carried the "strong westerly flow" sentence that S. Baidar corrected in the main
   text; SI S2.1 still placed all three CDPHE facilities in the metropolitan area.

Run: python3 scripts/117_review_fixes.py MAIN_IN SI_IN MAIN_OUT SI_OUT --outputs OUT_DIR
"""
import sys, csv, re, argparse
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
a = ap.parse_args(); OUT = Path(a.outputs); FIG = OUT / "figures"
rd = lambda f: list(csv.DictReader(open(OUT / f)))

# ---- numbers ------------------------------------------------------------------------------------
SC, SR = rd("facility_separation_scan.csv"), rd("facility_separation_scan_rolling.csv")
sep = lambda rows: sum(r["separable"] == "TRUE" for r in rows)
assert (len(SC), sep(SC), len(SR), sep(SR)) == (8, 8, 8, 6), (len(SC), sep(SC), len(SR), sep(SR))
cs = [int(r["cross_sep_m"]) for r in SC]; npl = [int(r["n_plume_near"]) for r in SR]
assert (min(cs), max(cs)) == (694, 1586) and (min(npl), max(npl)) == (10, 127)
FT, FR = rd("facility_sector_fits.csv"), rd("facility_sector_fits_rolling.csv")
ref = [int(r["n"]) for r in FT if r["sector"].startswith("Suncor")]; ww = {r["flight"]: r for r in FT if r["sector"].startswith("RobertHite")}
assert max(ref) == 17 and all(r["slope"] == "NA" for r in FT if r["sector"].startswith("Suncor"))
fitted = [f for f, r in ww.items() if r["slope"] != "NA"]; w9 = ww["20240709_R0"]
assert fitted == ["20240709_R0"] and (w9["n"], w9["n_intercepts"], w9["slope"], w9["r"], w9["fossil_pct"]) == ("24", "2", "0.032", "0.88", "32"), w9
r9 = next(r for r in FR if r["flight"] == "20240709_R0" and r["sector"].startswith("RobertHite"))
assert r9["n"] == "17" and all(r["slope"] == "NA" for r in FR)
HS = {r["hotspot_id"]: r for r in rd("v2_pointsource_hotspots.csv")}
assert HS["3"]["facility_km"] == "1.6" and round(float(HS["3"]["waste_t_hr"]), 2) == 0.39
lab = lambda f: f"{f[:4]}-{f[4:6]}-{f[6:8]}" + (f" ({f[-2:]})" if re.search(r"_L[12]$", f) else "")
T11 = [[lab(r["flight"]), r["n_plume_near"], r["wd_med"], r["cross_sep_m"], "yes" if r["separable"] == "TRUE" else "no"] for r in SC]

main = docx.Document(a.main_in); si = docx.Document(a.si_in)
M = lambda old, new, label, count=1: tc_replace(main, old, new, "main " + label, count)
S = lambda old, new, label, count=1: tc_replace(si, old, new, "SI " + label, count)

# ---- main: author affiliation numbers --------------------------------------------------------------
want = {"Sunil Baidar": "8", "Xinrong Ren": "4", "Phillip Stratton": "5", "Joost de Gouw": "6,7"}
p = tc_find(main, "Priyanka deSouza", "authors"); runs = p.runs; nfix = 0
for i, r in enumerate(runs[:-1]):
    for name, sup in want.items():
        if r.text.strip(" ,").endswith(name) and runs[i + 1].font.superscript and runs[i + 1].text != sup:
            runs[i + 1].text = sup; runs[i + 1].font.color.rgb = RED; nfix += 1
assert nfix == 4, f"authors: {nfix} superscripts corrected, expected 4"

# ---- main: slips -------------------------------------------------------------------------------------
M("the aircraft windows (section 2.4)", "the aircraft winds (section 2.4)", "windows")
M("2.2.CDPHE facility surveys", "2.2. CDPHE facility surveys", "heading 2.2")
M("3.1 Constant-altitude flight transects", "3.1. Constant-altitude flight transects", "heading 3.1")
M("3.2 Background and enhancement", "3.2. Background and enhancement", "heading 3.2")
M("from G2401 CH4, which has", "from the G2401 CH4, which has", "the G2401")
tc_insert_at(main, "CH4, C2H6, CO, and CO2 for the aircraft, CH4 and C2H6 for the ARL mobile laboratory", " ", "main ofCH4")
M("; sections 3.3, section S5)", "; sections 3.3 and S5)", "sections")

# ---- main: the wastewater plant ----------------------------------------------------------------------
M("Twelve crossings downwind of the waste cell immediately north of the Suncor/Metro Water Recovery complex (seven flights)",
  "Twelve crossings downwind of the waste cell that contains the Metro Water Recovery plant, beside the Suncor refinery (seven flights)", "4.3 crossings")
M("The source represented by the cell north of Metro Water Recovery is uncertain. That cell contains 0.39 t CH4/h of waste emissions, while the cell containing the treatment plant has none. "
  "This could reflect a misplaced representation of the plant or a different nearby waste source. The aircraft crossings nevertheless suggest less methane downwind of DADS than the inventory predicts, "
  "but more biogenic methane near the wastewater complex than the inventory assigns to the plant cell and the adjacent waste cell combined.",
  "That cell holds 0.39 t CH4/h of waste emissions and contains the treatment plant, so v2.0beta places the plant where it is. The aircraft crossings suggest less methane downwind of DADS than the inventory predicts, "
  "and somewhat more biogenic methane near the wastewater plant than the inventory assigns to it.", "4.3 cell")
M("the implied contribution from Metro Water Recovery’s Robert W. Hite facility is on the order of 1 t CH4/h.",
  "the implied contribution from Metro Water Recovery’s Robert W. Hite facility is about 0.6 t CH4/h, because the grid cell that contains the plant holds 37% of the inventory's in-box domestic wastewater methane.", "4.3 hite")

# ---- SI: slips and leftovers -----------------------------------------------------------------------------
S("For each 20- min profile", "For each 20-min profile", "20-min"); S("spaced at 50- m intervals", "spaced at 50-m intervals", "50-m")
S("Under strong westerly flow over the mountains, the ground-based variance method can detect turbulence aloft that is disconnected from the surface.",
  "A mixing height derived from vertical-velocity variance should be interpreted with caution, because turbulence aloft is not necessarily connected to the surface, "
  "and the boundary layer is generally deeper toward the foothills, where the ground-based lidar is located, than over the city.", "S1.1 westerly")
S("These facilities are within the metropolitan area, so their emissions contribute to the urban methane that the aircraft measures.",
  "The refinery and the terminal are within the metropolitan area, so their emissions contribute to the urban methane that the aircraft measures; the Collins Aerospace site is in Pueblo, about 160 km south of Denver.", "S2.1 Collins")
S("(Metro Water Recovery, Commerce City)", "(Metro Water Recovery, 6450 York Street, Denver)", "S2.1 address")

# ---- SI S11 and Table S9 ---------------------------------------------------------------------------------------
S("the third lies immediately north of the Metro Water Recovery plant, whose own cell carries no waste methane in v2.0beta, so whether it represents the plant or another waste facility is not determinable from the gridded product.",
  "the third contains the Metro Water Recovery wastewater plant, 1.6 km from the cell center.", "S11 third cell")
S("The twelve crossings downwind of the waste cell immediately north of the Suncor/Metro Water Recovery complex (v2.0beta waste 0.39 t/h in that cell; seven flights)",
  "The twelve crossings downwind of the waste cell that contains the Metro Water Recovery plant, beside the Suncor refinery (v2.0beta waste 0.39 t/h in that cell; seven flights)", "S11 crossings")
S("Which facility the third cell represents cannot be determined from the gridded product: it lies immediately north of the Metro Water Recovery plant, and the cell that contains the plant itself carries no waste methane in v2.0beta, "
  "so the cell may be the plant with a displaced location or another waste facility. Either way, the aircraft find far less methane than v2.0beta downwind of the landfill they could test, "
  "and more biogenic methane around the wastewater plant than v2.0beta places in that cell and the plant's own cell together:",
  "The third cell contains the Metro Water Recovery plant and so represents it. The aircraft find far less methane than v2.0beta downwind of the landfill they could test, "
  "and more biogenic methane around the wastewater plant than v2.0beta places in its cell:", "S11 which facility")
t9 = si.tables[8]; assert t9.rows[3].cells[0].text.strip() == "3" and t9.rows[3].cells[2].text.strip() == "2.5", [c.text for c in t9.rows[3].cells]
O.set_cell(t9.rows[3].cells[2], "1.6")

# ---- SI S12 and Table S11 ----------------------------------------------------------------------------------------
S("The Suncor refinery and Metro Water Recovery wastewater plant are separated by only about 1 km, placing",
  "The Metro Water Recovery wastewater plant lies about 1.6 km north-west of the reference point used here for the Suncor refinery, placing", "S12 distance")
S("Under easterly or westerly flow, the facilities lie approximately side by side across the wind and can, in principle, generate distinct plumes. Under northerly or southerly flow, they are aligned along the flow and their plumes overlap.",
  "Under north-easterly or south-westerly flow, the facilities lie approximately side by side across the wind and can, in principle, generate distinct plumes. "
  "Under north-westerly or south-easterly flow, they are aligned along the flow and their plumes overlap.", "S12 winds")
S("Of the flights that sampled the complex, 6 had a separating cross-wind. With a 5-min rolling background along the flight record in place of the flight-wide percentile, fewer near-complex samples count as plume (7 to 68 per flight) "
  "and 3 flights meet the separability criteria; the conclusion below holds under either background.",
  "All 8 flights that sampled the complex had a separating cross-wind (694 to 1,586 m). With a 5-min rolling background along the flight record in place of the flight-wide percentile, fewer near-complex samples count as plume (10 to 127 per flight) "
  "and 6 flights meet the separability criteria; the conclusion below holds under either background.", "S12 counts")
S("none of those flights placed enough samples in the refinery-only sector to fit a defensible ethane slope. On none of the flights did the aircraft place more than 5 samples in the wastewater-only sector, "
  "so an independent slope for the plant could not be fit either.",
  "none of those flights placed enough samples in the refinery-only sector to fit a defensible ethane slope (at most 17). In the wastewater-only sector one flight met the criteria: on 9 July, 24 samples in two intercepts "
  "give a slope of 0.032 (r = 0.88; 32% fossil), indistinguishable from the 34% of that flight's urban transects as a whole, so the sector did not isolate an ethane-free plume; with the rolling background the same sector holds 17 samples and no fit is possible.", "S12 sectors")
S("(winds within about 50° of east or west)", "(any wind more than about 22° off the north-west to south-east line joining them)", "Table S11 caption")
t11 = si.tables[10]; assert t11.rows[0].cells[0].text.strip() == "Flight" and len(t11.rows) == 8
O.do_table(si, 10, T11, None, "SI Table S11")

# ---- exponents typed with a hyphen ------------------------------------------------------------------------------------
nexp = 0
for d in (main, si):
    for p in L.all_paragraphs(d):
        for r in p._p.iter(qn("w:r")):
            rp = r.find(qn("w:rPr")); va = rp.find(qn("w:vertAlign")) if rp is not None else None
            if va is None or va.get(qn("w:val")) != "superscript": continue
            for t in r.iter(qn("w:t")):
                if t.text and re.fullmatch(r"-[123]", t.text): t.text = "−" + t.text[1:]; nexp += 1
for d in (main, si):
    ps = L.all_paragraphs(d); L.subscript_chem(ps); L.exponent_fix(ps)
# the product name is set with a superscript 2 everywhere else in the main text
import copy
from docx.text.run import Run
ng = 0
for p in main.paragraphs:
    if not (tc_text(p).startswith("Urban methane comes from") or tc_text(p).startswith("Figure 1:")): continue
    for r in list(p.runs):
        if "GRA2PES" not in r.text: continue
        parts = r.text.split("GRA2PES"); pieces = [(parts[0], False)]
        for part in parts[1:]: pieces += [("GRA", False), ("2", True), ("PES", False), (part, False)]
        prev = r._r
        for txt, sup in pieces:
            if not txt: continue
            el = copy.deepcopy(r._r); prev.addnext(el); prev = el; nr = Run(el, p); nr.text = txt
            if sup: nr.font.superscript = True
        r._r.getparent().remove(r._r); ng += 1
assert ng == 2, f"GRA2PES superscript applied to {ng} run(s), expected 2"
main.save(a.main_out); si.save(a.si_out)

# ---- figures with the plant marker ---------------------------------------------------------------------------------------
def media_in_order(path):
    import zipfile
    z = zipfile.ZipFile(path); x = z.read("word/document.xml").decode(); rels = z.read("word/_rels/document.xml.rels").decode(); out = []
    for m in re.finditer(r'r:embed="(rId\d+)"', x):
        t = re.search(r'Id="%s"[^>]*Target="([^"]+)"' % m.group(1), rels) or re.search(r'Target="([^"]+)"[^>]*Id="%s"' % m.group(1), rels)
        out.append(t.group(1).split("/")[-1])
    return out
mm = media_in_order(a.main_out); sm = media_in_order(a.si_out); assert len(mm) == 4 and len(sm) == 19, (len(mm), len(sm))
O.swap_figures(a.main_out, {mm[0]: FIG / "Fig1_study_map_sources.png"}, set(), "main Figure 1")
O.swap_figures(a.si_out, {sm[1]: FIG / "FigS4_mobile_plumes.png", sm[10]: FIG / "FigS9_biogenic_sources.png", sm[11]: FIG / "FigS13_spatial_match.png"}, set(), "SI Figures S2, S11, S12")
print(f"{nexp} exponent(s) given a minus sign; wrote {a.main_out} and {a.si_out}")
