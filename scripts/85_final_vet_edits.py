"""85_final_vet_edits.py -- fixes from the final vet of 4 Oct 2026 (main text, SI, layout).

Substance
  * Table S8 / section 4.3: the "sampled cells" over which the inventories' emission-weighted
    fossil fractions and box shares are evaluated are every inventory cell that contains any
    of the 106 aircraft-sampled 0.02-degree cells (18 EPA cells, 49 GRA2PES cells), not the
    26/24 cells with a local slope (which hold only 2% of the GRA2PES box methane). Script 58.
  * Section 3.1 / SI S4: the gridded signature map (Figure 4B, section S7.1) bins every
    boundary-layer sample and computes its background along the continuous flight record
    (script 25), unlike the leg-based fits.
  * SI S4 and S12 sensitivities are now produced by the pipeline (script 85 R, script 24 with
    METHANE_FACSCAN_BG=rolling) and are read from background_mode_sensitivity.csv and
    facility_separation_scan_rolling.csv rather than typed in.
  * Section 3.2 / SI S5: the 0.063-0.073 implied source ratio assumes an oil-and-gas share
    of one half, slightly above the 45% that the 0.0813 lower bound itself implies; stated.
  * SI S3: the 0.24 mol/mol leg (13 July L2, 28 samples, r = 0.70) is excluded from the floor
    constraint only by the 30-sample minimum; stated. SI S2.2: ethane-free plume definition
    includes the slope condition. SI S5: which flights enter the basin table. SI S8: the
    ambiguous 10 July L1 encounter (crosses in Figure S12). The 0.0813 value is cited to the
    AMMBEC report at first use (main and SI); SI S10 calls out Figure S16.
Wording and formatting
  * Grammar in 3.1 and 3.2, "it" -> "the production-area value" in the Limitations, typos,
    spaced hyphens used as dashes -> en dashes, GRA2PES spelled one way, Equation 3 subscript
    "source", Fig S4 and S6 caption subscripts, Table S10 header.
Layout
  * Table rows may no longer split across pages (cantSplit was present with val="0"), header
    rows repeat, captions keep their lines together, caption labels are bold, two bold runs in
    Limitation 1 and the Figure 1 caption are reset, five underlined citations are not.
Main and SI references are renumbered in order of first citation (the AMMBEC report is now cited in
3.2 and in S2.2). Figures 4 and S11 are scaled to 86% and 85% so that figure and caption share a page.

usage: python3 scripts/85_final_vet_edits.py <main.docx> <si.docx> <main_out> <si_out> --outputs <OUT_DIR>
(inputs: the _2026-10-03 documents written by script 84)
"""
import sys, csv, re, argparse, copy
from pathlib import Path
sys.path.insert(0, str(Path(__file__).resolve().parent))
import docx
from docx.oxml.ns import qn
from docx.text.run import Run
import docx_edit_lib as L
from docx_edit_lib import R, I, T, runs_text, all_paragraphs

ap = argparse.ArgumentParser(); ap.add_argument("main_in"); ap.add_argument("si_in"); ap.add_argument("main_out"); ap.add_argument("si_out")
ap.add_argument("--outputs", required=True)
a = ap.parse_args(); OUT = Path(a.outputs); L.OUTDIR = str(OUT)
rd = lambda f: list(csv.DictReader(open(OUT / f)))

# ---- numbers from the outputs ------------------------------------------------------------------
BM = rd("background_mode_sensitivity.csv")
order = ["20240703_R0_L1", "20240703_R0_L2", "20240708_R0_L1", "20240709_R0", "20240710_R0_L1", "20240713_R0_L1", "20240713_R0_L2"]
bm = {r["flight"]: r for r in BM}
ff_track = [int(bm[f]["fossil_pct_track"]) for f in order]
import statistics as st
med_j = round(st.median(int(bm[f]["fossil_pct_joined"]) for f in order)); med_t = round(st.median(ff_track))
assert med_j == 29 and med_t == 25, (med_j, med_t)
n0713 = (bm["20240713_R0_L2"]["n_gated_joined"], bm["20240713_R0_L2"]["n_gated_track"])
n0708 = (bm["20240708_R0_L1"]["n_gated_joined"], bm["20240708_R0_L1"]["n_gated_track"])
r0708 = (bm["20240708_R0_L1"]["r_co_joined"], bm["20240708_R0_L1"]["r_co_track"])
r0703 = (bm["20240703_R0_L1"]["r_co_joined"], bm["20240703_R0_L1"]["r_co_track"])
assert max(ff_track) < 50
FS = rd("facility_separation_scan_rolling.csv"); FS0 = rd("facility_separation_scan.csv")
n_sep_roll = sum(r["separable"] == "TRUE" for r in FS); n_sep = sum(r["separable"] == "TRUE" for r in FS0)
pts = [int(r["n_plume_near"]) for r in FS]
assert (n_sep, n_sep_roll) == (6, 3), (n_sep, n_sep_roll)
BS = rd("basin_summary.csv")[0]
share_arc = round(100 * float(BS["implied_og_share_using_ARC"]))          # 45 (44.6)
assert share_arc == 45
PL = rd("perleg_fossil_fraction.csv")
big = [r for r in PL if float(r["leg_slope"]) > 0.2]
assert len(big) == 1 and big[0]["flight"] == "20240713_R0_L2", big
big = big[0]; assert int(big["n_gated"]) == 28 and abs(float(big["leg_r"]) - 0.70) < 0.006

# ---- main text -----------------------------------------------------------------------------------
# sampled cells (Table S8) are the 4-km cells the aircraft crossed, not the 24 slope cells
R("compared with 0.12 when the sampled-cell emissions are aggregated. The 24 sampled cells inside the GRA2PES domain "
  "contain 45% of v2.0beta’s box-wide methane emissions but only 13% of those in v1.1, indicating",
  "compared with 0.12 when emissions are aggregated over every 4-km cell the aircraft crossed. Those 49 cells, which "
  "contain the 106 aircraft-sampled 0.02° cells, hold 45% of v2.0beta’s box-wide methane emissions but only 13% of "
  "those in v1.1 (the 15 cells holding the 24 cells with a local slope hold only about 2%), indicating")
# background: the gridded signature map runs along the flight record
R("which retains the urban-scale enhancement (section S4).",
  "which retains the urban-scale enhancement (section S4). The gridded signature map of Figure 4B and section S7.1, "
  "which bins every boundary-layer sample rather than level legs, computes the same 300-sample background along the "
  "continuous flight record.")
# 0.063-0.073 vs the 0.0813 lower bound
R("these imply an oil-and-gas source ratio of approximately 0.063-0.073 mol mol-1 (section S5).",
  f"these imply an oil-and-gas source ratio of approximately 0.063-0.073 mol mol-1 (section S5); that share is slightly "
  f"above the {share_arc}% the 0.0813 production-area ratio itself implies, so the range illustrates the sensitivity to "
  "the assumed share rather than adding an independent constraint.")
# 0.0813 cited to the AMMBEC report at first use
R("the ground-level ratio of 0.0813 measured in production areas during this campaign (an ambient ratio,",
  "the ground-level ratio of 0.0813 measured in production areas during this campaign[[^22]] (an ambient ratio,")
# wording
R("Using it as the endmember raises the campaign median to 36% fossil",
  "Using the production-area value as the endmember raises the campaign median to 36% fossil")
R("about 5 minutes (300 samples) for the aircraft and about 2 minutes for the CDPHE survey vehicle, and 5 minutes for the ARL car,",
  "about 5 minutes (300 samples) for the aircraft, about 2 minutes for the CDPHE survey vehicle, and 5 minutes for the ARL car,")
R("ethane leads methane by about 1 s, correcting for this changes", "ethane leads methane by about 1 s; correcting for this changes")
R("from fossil-sources and", "from fossil sources and")
R("a fossil share of ~ 26%", "a fossil share of ~26%")
R("compiled in  Table 2 of Kille et al.", "compiled in Table 2 of Kille et al.")
R("0.41 t CH4/h - about 7%", "0.41 t CH4/h, about 7%")
R("two landfills- Tower Road (18 plumes on 15 dates) and the Denver Arapahoe Disposal Site (DADS; 9 plumes on 6 dates)- which",
  "two landfills, Tower Road (18 plumes on 15 dates) and the Denver Arapahoe Disposal Site (DADS; 9 plumes on 6 dates), which")
R("about 11,700 km2- roughly four times the analysis box- it", "about 11,700 km2 (roughly four times the analysis box), it")
R("the aggregate comparison- a top-down fossil fraction of about 26% versus an EPA bottom-up fraction of about 60%.",
  "the aggregate comparison: a top-down fossil fraction of about 26% versus an EPA bottom-up fraction of about 60%.")
R("would miss a substantial- and during this campaign apparently majority- component",
  "would miss a substantial, and during this campaign apparently majority, component")
R("presented here- that Denver’s", "presented here, that Denver’s")
R("during this campaign- is consistent with", "during this campaign, is consistent with")

# ---- SI ------------------------------------------------------------------------------------------
R("one whose methane rises with no ethane response (|r| below 0.3) is called ethane-free",
  "one whose methane rises with no ethane response (|r| below 0.3 and slope below 0.01 mol mol-1) is called ethane-free", "si")
R("the 0.0813 mol mol−1 production-area ratio this work takes from the AMMBEC report to CDPHE,",
  "the 0.0813 mol mol-1 production-area ratio this work takes from the AMMBEC report to CDPHE,[[^13]]", "si")
R("Across 151 basin legs on 15 flights, 7 pass the screen.",
  "A flight enters the basin analysis only with at least three basin legs and at least ten gated samples; seven of "
  "the 22 flights fail this (four with one or two basin legs, three with fewer than ten samples above the gate). "
  "Across 151 basin legs on the 15 remaining flights, 7 pass the screen.", "si")
R("Taking the ground-level ratio of 0.0813 mol mol−1 measured by the NOAA ARL Air Resources Car during this campaign "
  "(section S2.2) as the endmember,",
  "Taking the ground-level ratio of 0.0813 mol mol-1 measured in production areas during this campaign[[^13]] (which "
  "section S2.2 reproduces from the car's own files) as the endmember,", "si")
R("Our 2024 implied source ratio of 0.063 to 0.073 is lower; it rests on the assumed oil-and-gas share near one half,",
  f"Our 2024 implied source ratio of 0.063 to 0.073 is lower; it rests on an assumed oil-and-gas share near one half, "
  f"slightly above the {share_arc}% that the 0.0813 lower bound itself implies,", "si")
R("0.054 (13 July L1) and 0.047 (13 July L2). For the assumed lean basin ratio of 0.061,",
  "0.054 (13 July L1) and 0.047 (13 July L2). A 28-sample leg at 2.3 km on 13 July L2 (r = 0.70) has a slope of 0.24 "
  "mol mol-1 and is excluded only by the sample minimum; it drives the wide upper interval of that flight in Table 1 "
  "and is discussed in section S9, and under this model it would indicate air aloft richer in ethane than any "
  "reference value, which the sample is too small to test. For the assumed lean basin ratio of 0.061,", "si")
R("analysis of the same flights ", "analysis of the same flights", "si")
R("and the inventory's emission-weighted fossil fraction over the sampled cells against the whole box, which says "
  "whether the aircraft sampled the fossil-rich or the biogenic-rich part of each inventory.",
  "and the inventory's emission-weighted fossil fraction over the sampled cells against the whole box, which says "
  "whether the aircraft sampled the fossil-rich or the biogenic-rich part of each inventory. Sampled cells here are "
  "every inventory cell that contains any of the 106 aircraft-sampled 0.02-degree cells (18 EPA cells, 49 GRA2PES "
  "cells); the 15 GRA2PES cells holding the 24 cells with a local slope hold only about 2% of the box methane.", "si")
R("Table S8: Cell-matched comparison of the aircraft fossil fraction (0.102 endmember) with three gridded inventories "
  "at the aircraft-sampled cells. Aircraft median is the median over the cells; inventory medians and emission-weighted "
  "fractions are fossil over fossil plus biogenic.",
  "Table S8: Cell-matched comparison of the aircraft fossil fraction (0.102 endmember) with three gridded inventories "
  "at the aircraft-sampled cells. Aircraft median is the median over the cells with a local slope; inventory medians "
  "and emission-weighted fractions are fossil over fossil plus biogenic, the sampled-cell columns over every inventory "
  "cell containing any of the 106 aircraft-sampled cells.", "si")
R("(slopes 0.031 to 0.039, r = 0.70 to 0.73; crosses in Figure S12);",
  "(slopes 0.031 to 0.039, r = 0.70 to 0.73), and likewise a 10 July (L1) leg 16 to 18 km downwind of the same landfill and "
  "one of the sites (slopes -0.003 and 0.049 on 11 and 12 samples; both legs are the crosses in Figure S12);", "si")
# SI S4: the along-track sensitivity, now from background_mode_sensitivity.csv
R("the number of gated samples falls on every flight (from 234 to 95 on 13 July L2 and from 121 to 24 on 8 July), and "
  "the fossil fractions become 25, 32, 6, 28, 2, 26 and 0% (3 July L1, 3 July L2, 8 July, 9 July, 10 July L1, 13 July "
  "L1 and 13 July L2), a median of 25% against 29%, with no flight above 50%. The CH4:CO correlation also weakens (r "
  "from 0.85 to 0.17 on 8 July and from 0.91 to 0.58 on 3 July L1),",
  f"the number of gated samples falls on every flight (from {n0713[0]} to {n0713[1]} on 13 July L2 and from {n0708[0]} "
  f"to {n0708[1]} on 8 July), and the fossil fractions become {', '.join(str(v) for v in ff_track[:-1])} and "
  f"{ff_track[-1]}% (3 July L1, 3 July L2, 8 July, 9 July, 10 July L1, 13 July L1 and 13 July L2), a median of {med_t}% "
  f"against {med_j}%, with no flight above 50%. The CH4:CO correlation also weakens (r from {r0708[0]} to {r0708[1]} on "
  f"8 July and from {r0703[0]} to {r0703[1]} on 3 July L1),", "si")
R("fewer near-complex samples count as plume (7 to 68 per flight) and 3 flights meet the separability criteria;",
  f"fewer near-complex samples count as plume ({min(pts)} to {max(pts)} per flight) and {n_sep_roll} flights meet the "
  "separability criteria;", "si")
R("Of the flights that sampled the complex, 6 had a separating cross-wind.",
  f"Of the flights that sampled the complex, {n_sep} had a separating cross-wind.", "si")
I("S10. Per-flight urban sampling and winds",
  "Figure S16 shows, for each of the eight Denver-metro flights, the urban legs flown, the in-plume samples and the "
  "mean boundary-layer wind, which together set how much of the city each flight-level fossil fraction represents.", "si")
T("si", 9, 0, 3, "Dist. to cell (km)", expect="Distance to hotspot cell (km)")

# ---- anchors must not straddle a hyperlinked citation --------------------------------------------
def sentinel_text(p):
    out = []
    for ch in p._p:
        if ch.tag == qn("w:r"): out.append("".join(t.text or "" for t in ch.iter(qn("w:t"))))
        elif ch.tag == qn("w:hyperlink"): out.append("\x00")
    return "".join(out)
for which, path in (("main", a.main_in), ("si", a.si_in)):
    d = docx.Document(path); txt = "\n".join(sentinel_text(p) for p in all_paragraphs(d))
    for anc, _, doc_, kind, _ in L.E:
        if doc_ == which and kind == "replace" and anc not in txt:
            sys.exit(f"{which}: anchor crosses a hyperlinked citation: {anc[:80]!r}")

L.apply(a.main_in, a.main_out, "main", "main text")
L.apply(a.si_in, a.si_out, "si", "SI")

# ---- post-passes --------------------------------------------------------------------------------
def all_runs(d):
    for p in all_paragraphs(d):
        for r in p.runs: yield p, r

def split_run(p, r, pieces):
    """replace run r by consecutive copies holding `pieces` = [(text, fn)] where fn adjusts the new Run"""
    prev = r._r
    for txt, fn in pieces:
        if not txt: continue
        el = copy.deepcopy(r._r); prev.addnext(el); prev = el
        nr = Run(el, p); nr.text = txt
        if fn: fn(nr)
    r._r.getparent().remove(r._r)

def unify_gra2pes(d):
    """GRA^2PES (superscript 2 in its own run) -> GRA2PES, the form used in the SI and in the inventory's own name"""
    n = 0
    for p in all_paragraphs(d):
        rs = p.runs
        for i, r in enumerate(rs):
            if r.text == "2" and r.font.superscript and i > 0 and rs[i - 1].text.endswith("GRA") and i + 1 < len(rs) and rs[i + 1].text.startswith("PES"):
                r.font.superscript = None; n += 1
    return n

def chem_subscripts_anywhere(d, pattern):
    """subscript the digits of CH4 / CO2 in runs matching `pattern`, whatever their colour"""
    n = 0
    for p, r in list(all_runs(d)):
        if r.font.subscript or r.font.superscript or not re.search(pattern, r.text): continue
        pieces = []
        for tok in re.split(r"(CH4|CO2)", r.text):
            if tok in ("CH4", "CO2"): pieces += [(tok[:-1], None), (tok[-1], lambda nr: setattr(nr.font, "subscript", True))]
            elif tok: pieces.append((tok, None))
        split_run(p, r, pieces); n += 1
    return n

def beta_subscripts(d):
    n = 0
    for p, r in list(all_runs(d)):
        if "βsource" not in r.text: continue
        pieces = []
        for tok in re.split(r"(βsource)", r.text):
            if tok == "βsource": pieces += [("β", None), ("source", lambda nr: setattr(nr.font, "subscript", True))]
            elif tok: pieces.append((tok, None))
        split_run(p, r, pieces); n += 1
    return n

def strip_trailing_space_in_superscripts(d):
    """'^{14 }provides' -> '^{14} provides'"""
    n = 0
    for p, r in list(all_runs(d)):
        if r.font.superscript and r.text != r.text.rstrip() and r.text.strip():
            tail = r.text[len(r.text.rstrip()):]; r.text = r.text.rstrip()
            el = copy.deepcopy(r._r); r._r.addnext(el); nr = Run(el, p); nr.text = tail; nr.font.superscript = None; n += 1
        elif r.font.superscript and not r.text.strip() and r.text:
            r.font.superscript = None; n += 1
    return n

def caption_labels_bold(d):
    """'Figure Sn:' / 'Table n:' labels bold, and nothing after the colon bold by inheritance from the label run"""
    n = 0
    for p in d.paragraphs:
        m = re.match(r"((?:Figure|Table) S?\d+:)", p.text)
        if not m: continue
        label = m.group(1); pos = 0
        for r in p.runs:
            s, e = pos, pos + len(r.text); pos = e
            if e <= len(label):
                if not r.bold: r.bold = True; n += 1
            elif s < len(label) < e:
                cut = len(label) - s; head, tail = r.text[:cut], r.text[cut:]
                def mk(b): return lambda nr: setattr(nr, "bold", b)
                split_run(p, r, [(head, mk(True)), (tail, mk(False))]); n += 1; break
            else: break
    return n

def scale_picture(d, caption_prefix, factor):
    """shrink the picture whose caption follows it, so figure and caption fit one page"""
    ps = d.paragraphs
    for i, p in enumerate(ps):
        if p.text.startswith(caption_prefix):
            for j in range(i - 1, max(i - 4, -1), -1):
                dr = ps[j]._p.findall(".//" + qn("w:drawing"))
                if dr:
                    for ext in dr[0].iter(qn("wp:extent")):
                        ext.set("cx", str(int(int(ext.get("cx")) * factor))); ext.set("cy", str(int(int(ext.get("cy")) * factor)))
                    for ext in dr[0].iter(qn("a:ext")):
                        ext.set("cx", str(int(int(ext.get("cx")) * factor))); ext.set("cy", str(int(int(ext.get("cy")) * factor)))
                    return True
    return False

def tidy_layout(d, label):
    nk = 0
    for p in d.paragraphs:
        if p._p.findall(".//" + qn("w:drawing")): p.paragraph_format.keep_with_next = True; nk += 1
        if re.match(r"(Figure|Table) S?\d+:", p.text): p.paragraph_format.keep_together = True
    nr = 0
    for tb in d.tables:
        for i, tr in enumerate(tb._tbl.iter(qn("w:tr"))):
            trpr = tr.find(qn("w:trPr"))
            if trpr is None: trpr = tr.makeelement(qn("w:trPr"), {}); tr.insert(0, trpr)
            cs = trpr.find(qn("w:cantSplit"))
            if cs is None: cs = trpr.makeelement(qn("w:cantSplit"), {}); trpr.append(cs)
            cs.set(qn("w:val"), "1"); nr += 1
            if i == 0:
                th = trpr.find(qn("w:tblHeader"))
                if th is None: th = trpr.makeelement(qn("w:tblHeader"), {}); trpr.append(th)
                th.set(qn("w:val"), "1")
        # short tables stay on one page: every row but the last keeps with the next
        rows = list(tb._tbl.iter(qn("w:tr")))
        if len(rows) <= 6:
            for tr in rows[:-1]:
                for pp in tr.iter(qn("w:p")):
                    ppr = pp.find(qn("w:pPr"))
                    if ppr is None: ppr = pp.makeelement(qn("w:pPr"), {}); pp.insert(0, ppr)
                    if ppr.find(qn("w:keepNext")) is None: ppr.append(ppr.makeelement(qn("w:keepNext"), {}))
    print(f"{label}: {nk} figures kept with captions, captions kept together, {nr} table rows set not to split, header rows repeat")

m = docx.Document(a.main_out)
n = unify_gra2pes(m); print(f"main text: {n} GRA2PES spellings unified")
n = strip_trailing_space_in_superscripts(m); print(f"main text: {n} superscript spacing fixes")
# Equation 3: subscript "Source" -> "source"
ne = 0
for t in m.element.body.iter(qn("m:t")):
    if t.text == "Source": t.text = "source"; ne += 1
assert ne == 1, ne
# underlined hyperlinked citations
nu = 0
for h in m.element.body.iter(qn("w:hyperlink")):
    for r in h.iter(qn("w:r")):
        rp = r.find(qn("w:rPr"))
        if rp is not None and rp.find(qn("w:vertAlign")) is not None and rp.find(qn("w:u")) is not None:
            rp.remove(rp.find(qn("w:u"))); nu += 1
print(f"main text: {nu} underlined citations fixed")
# bold runs that should not be bold
nb = 0
for p in m.paragraphs:
    if p.text.startswith("Figure 1:"):
        for r in p.runs[1:]:
            if r.bold: r.bold = False; nb += 1
    if p.text.startswith("The ethane endmember is the dominant"):
        # the long red runs inherited bold from the "Table 1, Figure 2" cross-reference they replaced;
        # the short bold runs are Word cross-references, bold throughout the document, and are left
        for r in p.runs:
            if r.bold and len(r.text) > 12: r.bold = False; nb += 1
print(f"main text: {nb} bold runs reset")
n = caption_labels_bold(m); print(f"main text: {n} caption labels made bold")
L.renumber_refs(m, "main text")
assert scale_picture(m, "Figure 4:", 0.86)
tidy_layout(m, "main text")
m.save(a.main_out)

s = docx.Document(a.si_out)
n = chem_subscripts_anywhere(s, r"OLS for the CH4:CO emission slope"); print(f"SI: {n} CH4 subscript fixes")
n = beta_subscripts(s); print(f"SI: {n} beta_source subscript fixes")
n = caption_labels_bold(s); print(f"SI: {n} caption labels made bold")
L.renumber_refs(s, "SI", skip_after=r"(GRA|km|δ|σw|\bm|\bs)$")
assert scale_picture(s, "Figure S11:", 0.85)
tidy_layout(s, "SI")
s.save(a.si_out)

# ---- regenerated figure (Figure S11 legend background) --------------------------------------------
L.swap_pictures(a.si_out, [("image3.png", str(OUT / "figures" / "FigS13_spatial_match.png"))], "SI")

# ---- final checks --------------------------------------------------------------------------------
for path, label, bads in ((a.main_out, "main text", ("The 24 sampled cells", "fossil-sources", "~ 26%", "GRA2PES", "km2- ", "here- that")),
                          (a.si_out, "SI", ("flights ^{13}", "Distance to hotspot cell"))):
    d = docx.Document(path); t = "\n".join(runs_text(p) for p in all_paragraphs(d))
    for bad in bads:
        if bad == "GRA2PES":
            assert not any(r.text == "2" and r.font.superscript and p.runs[i - 1].text.endswith("GRA")
                           for p in all_paragraphs(d) for i, r in enumerate(p.runs) if i), f"{label}: GRA^2PES remains"
        else:
            assert bad not in t, f"{label}: {bad!r} remains"
print("checks passed")
