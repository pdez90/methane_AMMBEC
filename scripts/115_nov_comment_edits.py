#!/usr/bin/env python3
"""115_nov_comment_edits.py -- co-author comments on the November draft (main text and SI).

The draft carries co-authors' tracked changes and 26 Word comments. This script leaves every
tracked change and comment thread in place (docx_tc works on the text with the changes
accepted) and makes its own edits in red:

  * the comments that ask for a change in the text (abstract novelty, "source mix", waste
    sources, roads, aircraft exhaust, precision, B = 2000, winds, ground-lidar caveat,
    Carbon Mapper coverage, slope/intercept wording, where the GRA2PES description sits);
  * typing slips left by the last round of edits (affiliation numbers, "ad rhodes", missing
    spaces and full stops);
  * S. Baidar's point that, with sampling inside the city, the per-transect intercept holds
    information the slope ignores: fixed-background estimators (script 112) and the 9 July
    transect east of the city (script 113), as a paragraph in section 3.2, sentences in 4.1
    and the limitations, and a new SI section S13 with Table S12 and Figure S19;
  * Figure 1 gains panel (b), the analysis box with the main sources (script 114).

Numbers are read from the outputs of scripts 112 and 113 and checked against the text.

Run: python3 scripts/115_nov_comment_edits.py MAIN_IN SI_IN MAIN_OUT SI_OUT --outputs OUT_DIR
"""
import sys, csv, copy, argparse, statistics as st
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent))
import docx
from docx.shared import Pt, Inches
from docx.oxml.ns import qn
from docx.text.paragraph import Paragraph
from docx.text.run import Run
import docx_edit_lib as L, docx_ops as O
from docx_tc import tc_replace, tc_find, tc_text, tc_insert_after, tc_insert_at
RED = L.RED

ap = argparse.ArgumentParser()
for k in ("main_in", "si_in", "main_out", "si_out"): ap.add_argument(k)
ap.add_argument("--outputs", required=True)
a = ap.parse_args(); OUT = Path(a.outputs); L.OUTDIR = str(OUT); L.FIGDIR = str(OUT / "figures")
rd = lambda f: list(csv.DictReader(open(OUT / f)))
num = lambda rows, k: [float(r[k]) for r in rows if r[k] not in ("", "NA")]

# ---- numbers ------------------------------------------------------------------------------
E = rd("fixed_background_estimators.csv")
BG = ("p01flight", "p05urban", "aloft")
med = lambda k: [st.median(num(E, f"{k}_{b}")) for b in BG]
pooled, ratio, basin = med("fossil_pooled"), med("fossil_ratio"), med("fossil_ratio_basin")
n50 = [sum(v >= 50 for v in num(E, f"fossil_ratio_{b}")) for b in BG]
rr = [v for b in BG for v in num(E, f"fossil_ratio_{b}")]
dd = num(E, "dch4_downwind_minus_upwind"); nneg = sum(v < 0 for v in dd)
assert st.median(num(E, "fossil_within_rolling")) == 21 and pooled == [27, 27, 21.5] and ratio == [44, 39, 39.5] and basin == [42, 35, 31.5], (pooled, ratio, basin)
assert n50 == [2, 0, 0] and nneg == 7 and len(E) == 9 and (min(rr), max(rr)) == (20, 55), (n50, nneg, min(rr), max(rr))
pos = sorted((r["flight"], float(r["dch4_downwind_minus_upwind"])) for r in E if float(r["dch4_downwind_minus_upwind"]) > 0)
assert [f for f, _ in pos] == ["20240705_R0_L1", "20240709_R0"] and [round(v) for _, v in pos] == [20, 16]
assert (round(-max(v for v in dd if v < 0)), round(-min(dd))) == (5, 84)
r9 = {r["flight"]: r for r in E}
assert r9["20240709_R0"]["fossil_ratio_p01flight"] == "53" and r9["20240710_R0_L1"]["fossil_ratio_p01flight"] == "55"

F = {(r["level"], r["band"][:3]): r for r in rd("downwind_leg_0709_fits.csv")}
lo, hi, lon_, mid, los, lono = F[("low", "box")], F[("high", "box")], F[("low", "bas")], F[("low", "39.")], F[("low", "sou")], F[("low", "nor")]
chk = lambda r, k, v: abs(float(r[k]) - v) < 1e-9 or sys.exit(f"downwind fits: {k} is {r[k]}, text says {v}")
for r, k, v in ((lo, "slope_york", 0.0381), (lo, "r", 0.85), (lo, "fossil_pct_slope", 37), (lo, "ratio_zero_intercept", 0.05196), (lo, "fossil_pct_ratio", 51), (lo, "n", 735),
                (lo, "r_ch4_co", -0.2), (lo, "wind_from_deg", 357), (lo, "wind_vector_ms", 3.6), (lo, "intercept_ppb", 0.71),
                (hi, "slope_york", 0.04311), (hi, "r", 0.89), (hi, "fossil_pct_slope", 42), (hi, "ratio_zero_intercept", 0.05217),
                (lon_, "slope_york", 0.02064), (lon_, "ratio_zero_intercept", 0.05473), (lon_, "fossil_pct_ratio", 54), (mid, "slope_york", 0.02016), (mid, "ratio_zero_intercept", 0.03199),
                (los, "slope_york", 0.03294), (los, "fossil_pct_slope", 32), (los, "ratio_zero_intercept", 0.05685), (los, "fossil_pct_ratio", 56), (los, "r_ch4_co", 0.65), (los, "dch4_median_ppb", 34.6),
                (lono, "slope_york", 0.03275), (lono, "fossil_pct_slope", 32), (lono, "r_ch4_co", -0.29)): chk(r, k, v)
assert round(100 * 0.5 / float(los["dch4_median_ppb"]) / 0.102) == 14
PSS = {r["level"]: r for r in rd("downwind_leg_0709_passes.csv")}
assert [PSS["low"][k] for k in ("start_utc", "end_utc", "lon_median", "agl_min_m", "agl_max_m", "blh_airborne_m", "km_east_of_box")] == ["17:51", "18:17", "-104.39", "223", "712", "1685", "14.1"], PSS["low"]
assert [PSS["high"][k] for k in ("start_utc", "end_utc", "agl_min_m", "agl_max_m", "blh_airborne_m")] == ["18:30", "18:52", "2048", "2580", "2702"], PSS["high"]
S = rd("downwind_leg_0709_stilt.csv"); g = lambda grp, k: [float(r[k]) for r in S if r["group"] == grp]
pc = lambda v: round(100 * v)
assert len(S) == 16 and len(g("low_box", "share_box")) == 8
assert (pc(st.median(g("low_box", "share_box"))), pc(min(g("low_box", "share_box"))), pc(max(g("low_box", "share_box")))) == (12, 1, 42)
assert (pc(st.median(g("low_box", "share_north"))), pc(st.median(g("low_box", "share_og_basin")))) == (43, 41)
assert pc(st.median(g("low_north", "share_north"))) == 97 and pc(st.median(g("high_box", "share_box"))) == 6
fb = g("low_box", "foot_share_box"); assert (pc(st.median(fb)), pc(min(fb)), pc(max(fb)), pc(st.median(g("high_box", "foot_share_box")))) == (27, 3, 52, 13), fb
assert [pc(v) for v in fb[:4]] == [52, 34, 36, 33] and r9["20240710_R0_L1"]["fossil_ratio_aloft"] == "39"
T1 = {r["Flight"]: r for r in rd("table1.csv")}
assert T1["20240709_R0"]["Fossil_pct_CI"] == "34 [25-39]", "Table 1: 9 July is not 34 [25-39]"

main = docx.Document(a.main_in); si = docx.Document(a.si_in)
M = lambda old, new, label, count=1: tc_replace(main, old, new, "main " + label, count)
Sx = lambda old, new, label, count=1: tc_replace(si, old, new, "SI " + label, count)

# ---- title block: two affiliations were numbered 3 ------------------------------------------
p = tc_find(main, "Priyanka deSouza", "authors"); seen_durant = False; nfix = 0
for r in p.runs:
    if "John L Durant" in r.text: seen_durant = True; continue
    if seen_durant and r.font.superscript and r.text.strip() and r.text.strip() != "3":
        r.text = ",".join(str(int(x) + 1) for x in r.text.split(",")); r.font.color.rgb = RED; nfix += 1
    elif seen_durant and r.font.superscript and r.text.strip() == "3": seen_durant = "done"   # Durant keeps 3 (Tufts)
assert nfix == 10, f"authors: {nfix} superscripts renumbered, expected 10"
M("3: Department of Chemistry, University of Colorado Boulder", "4: Department of Chemistry, University of Colorado Boulder", "affil 4")
M("4: Cooperative Institute for Research in Environmental Sciences", "5: Cooperative Institute for Research in Environmental Sciences", "affil 5")
M("5: Chemical Sciences Laboratory, NOAA, Boulder", "6: Chemical Sciences Laboratory, NOAA, Boulder", "affil 6")

# ---- abstract (comments 1 to 4) -------------------------------------------------------------
M("We evaluate this split using the 2024 Airborne Methane Mass Balance Emissions in Colorado (AMMBEC) campaign, flown over Denver and the adjacent Denver-Julesburg Basin, an oil and natural-gas producing area.",
  "We report, to our knowledge, the first airborne estimate of this split for the Denver metropolitan area itself, as distinct from the adjacent Denver-Julesburg Basin, an oil and natural-gas producing area. "
  "It uses flights of the 2024 Airborne Methane Mass Balance Emissions in Colorado (AMMBEC) campaign, which were designed around the basin, and an approach that needs no closed flight path around the city.", "abstract novelty")
M("the median was 19 to 26%,; the median exceeds 50% fossil only for an effective endmember below 0.042 mol mol−1.", "the median was 19 to 26%.", "abstract 0.042")
M("These anchor-dependent rates are less certain than the source mix and exceed", "These rates scale with the CO inventory and are therefore less certain than the fossil-biogenic split; they exceed", "abstract source mix")
M("which adds substantial waste methane.", "which adds substantial waste methane from landfills and wastewater treatment.", "abstract waste")

# ---- introduction and section 2 ---------------------------------------------------------------
M("stacked flight transects(typically", "stacked flight transects (typically", "space")
hd = [p for p in main.paragraphs if tc_text(p).strip() == "2. Data"]; assert len(hd) == 1
L.insert_after(hd[0], "The analysis draws on six data streams. The airborne in situ measurements (section 2.1) are the basis of every quantitative result. Two ground-based data sets, the CDPHE facility surveys (section 2.2) and the "
  "NOAA ARL mobile laboratory (section 2.3), give context on source types and a street-level check on the airborne ratios. Doppler-lidar mixing heights and the aircraft winds (section 2.4) enter the sensitivity tests and the transect-level analyses. "
  "Carbon Mapper's plume catalogue (section 2.5) locates independently detected point sources, and four gridded inventories (section 2.6) supply the anchors for the emission rates and the bottom-up comparison.", main)
M("calibration results ad rhodes of the other 14 flights", "calibration results, and that those of the other 14 flights", "ad rhodes")
M("pre-campaign laboratory specification.The estimated 1σ measurement uncertainties were", "pre-campaign laboratory calibration. The estimated 1σ measurement uncertainties were", "calibration")
M("These uncertainties were used as the instrument precisions in the York fits (sections 3.3-3.4).",
  "These uncertainties, which describe the repeatability of a 1-s reading and are not detection limits, were used as the instrument precisions in the York fits (sections 3.3-3.4).", "precision 2.1")
M("2.2.CDPHE facility surveys", "2.2. CDPHE facility surveys", "heading 2.2")
M("in the three mile radius around three Front Range industrial facilities", "within about three miles (5 km) of three Front Range industrial facilities", "three mile")
M("These facilities lie within the metropolitan area, so their emissions are part of the urban source field sampled by the aircraft.",
  "The refinery and the terminal lie within the metropolitan area, so their emissions are part of the urban source field sampled by the aircraft; the Collins Aerospace site is in Pueblo, about 160 km south of Denver, and serves only as a comparison.", "Collins")
M("from G240 CH4, which has", "from the G2401 CH4, which has", "G2401")
M("are the aircraft's own measurements (wind roses", "are the in situ winds measured at flight level by the aircraft's AIMMS probe (section 2.1), not the wind profiles of the Doppler lidar (wind roses", "winds")
M("under strong westerly flow over the mountains its vertical-velocity variance can register turbulence aloft that is not connected to the surface, and its mixing height exceeds",
  "a mixing height defined from vertical-velocity variance has to be read with caution, because turbulence aloft need not be connected to the surface, and the boundary layer is generally deeper towards the foothills in the west than over the city. Its mixing height exceeds", "ground lidar")
M("none of these plumes were observed during the AMMBEC campaign.",
  "none of these plumes were observed during the AMMBEC campaign, because the catalogue lists no acquisition over these sources between 28 June and 13 July 2024; the nearest methane detections were on 15 April and 17 August 2024.", "carbon mapper dates")
M("Other sources such as the refinery in north Denver did not appear in the catalogue.",
  "No methane plume is catalogued at the Metro Water Recovery wastewater plant or at the adjacent refinery in north Denver; the one plume there is CO2 from a power station about 1 km from the plant. "
  "Diffuse emissions from a treatment plant may not exceed the detection thresholds of these imagers, so their absence from the catalogue does not show that the plant emits no methane.", "carbon mapper wwtp")
M("whereas the methods for combustion sources of CO and CO2 are largely unchanged.",
  "whereas the methods for combustion sources of CO and CO2 are largely unchanged. For municipal landfills, methane removal efficiencies are assigned from land-cover type and the status of gas collection and control systems.", "gra2pes 2.6")
M("provides the CO2 anchor. The EPA 2020 National Emissions Inventory (NEI) county-level CO totals provide a sensitivity anchor (section 3.4).", "provides the CO2 anchor.", "duplicate NEI")

# ---- section 3 ---------------------------------------------------------------------------------
M("3.1 Constant-altitude flight transects", "3.1. Constant-altitude flight transects", "heading 3.1")
M("3.2 Background and enhancement", "3.2. Background and enhancement", "heading 3.2")
M("gas leaks, and roads can spread vertically", "gas leaks, and road traffic (vehicle exhaust, the main urban source of CO) can spread vertically", "roads")
tc_insert_at(main, "CH4, C2H6, CO, and CO2 for the aircraft, CH4 and C2H6 for the ARL mobile laboratory", " ", "main space CH4")
M("(note that  the facility-separation scan", "(note that the facility-separation scan", "double space")
M("which retains the urban-scale enhancement (section S4).",
  "which retains the urban-scale enhancement (section S4). Exhaust from the aircraft itself does not enter these enhancements: in level flight the aircraft leaves its exhaust behind, "
  "and the turns in which it could cross its own wake are excluded from the transects (section 3.1).", "aircraft exhaust")
M("within a few hundred metres shifts", "within a few hundred meters shifts", "meters")
M("even though the regional background varies slowly", "even though the regional background varies slowly.", "full stop")
q = tc_insert_after(main, "These tests indicate that the predominance of non-fossil methane in the campaign-median estimate does not depend on the chosen background definition.",
  "The separate intercepts also have a cost. Because the aircraft sampled inside the urban area and not downwind of it, a methane enhancement that is uniform over a whole transect, with the ethane that accompanies it, "
  "is taken up by that transect's intercept and does not inform the slope; the within-transect estimate therefore describes the sources that produce structure within the city on each pass. "
  "We tested estimators that keep this information, using one fixed background per flight (the 1st percentile of the flight record, the 5th percentile of the flight's urban-transect samples, or the air above the boundary layer). "
  "A single York slope through all gated urban samples, with one intercept per flight, gives campaign medians of 22 to 27%. Attributing the entire enhancement above the fixed background to the sampled sources, "
  "as the ratio of the summed ethane and methane enhancements with no intercept, gives 39 to 44%, with at most two of the nine flights at or above 50%. We treat this as an upper bound and not as an alternative estimate, "
  "because the same ratio over the basin transects of the same flights is 32 to 42%, and on seven of the nine flights the samples at the upwind edge of the box carried more methane than those in its downwind half: "
  "most of the enhancement above a clean-air background is regional air, part of it advected from the basin, and not an increment added by the city (section S13).", "main 3.2 intercept")
q.paragraph_format.space_before = Pt(12); q.paragraph_format.space_after = Pt(0)
M("we fit a fixed-effects York regression with a separate intercept for each transect and a common slope across transects.",
  "we estimated one ethane-to-methane slope per flight, which is the quantity of interest, while allowing each transect its own intercept (a fixed-effects York regression). "
  "The intercepts are nuisance terms that absorb transect-to-transect differences in the ethane level and are not interpreted; what they leave out is examined in section 3.2.", "slope wording")
M("a threshold well above both the 1 ppb instrument precision and the typical scatter", "a threshold well above both the 1 ppb instrument precision (the 1σ repeatability of a 1-s methane reading, not a detection limit) and the typical scatter", "precision 3.3")
M("with replacement (B = 2000), repeated the within-transect regression", "with replacement (2,000 bootstrap resamples), repeated the within-transect regression", "B 3.3")
M("by resampling whole transects with replacement (B = 2000).", "by resampling whole transects with replacement (2,000 resamples).", "B 3.4")

# ---- section 4 ---------------------------------------------------------------------------------
M("Of the 22flights, 21 crossed", "Of the 22 flights, 21 crossed", "space 22")
M("would give 35% (sections 3.3, S3).",
  "would give 35% (sections 3.3, S3). Attributing the whole enhancement above a fixed clean-air background to the city, an upper bound that includes regional air advected into it, gives 39 to 44% (section 3.2).", "4.1 upper bound")
q = tc_insert_after(main, "although the car sampled mainly the northern edge of the box (section S2.2).",
  "The one transect flown just outside the box, east of the city on 9 July, supports the in-box value for one of the most fossil-influenced flights. A north-south pass along 104.39°W, about 14 km east of the box, "
  "gives a fitted slope of 0.038 mol mol−1 at box latitudes (r = 0.85; 37% fossil; 0.033, or 32%, on each half of that stretch), against 34% (25 to 39%) inside the box on that flight. "
  "The ratio of its total ethane and methane enhancements above a clean-air background is higher (0.052 mol mol−1, 51%), but the same ratio is found on that pass at basin latitudes and again near the top of the boundary layer, "
  "and HRRR-STILT places a median of only 27% of the pass's footprint inside the box. That ratio therefore largely characterizes the regional air arriving from the north, and not the city (section S13.2, Figure S19).", "4.1 downwind")
q.paragraph_format.space_before = Pt(12); q.paragraph_format.space_after = Pt(0)
M("GRA2PES v2.0beta explicitly developed facility-level methane emissions for landfill and wastewater sectors. For municipal landfills, methane removal efficiencies are determined based on land cover types, the status of gas collection and control systems. "
  "With the current parameterization, the v2.0beta waste emissions should be treated as an upper estimate. Future GRA2PES releases will include uncertainty ranges.",
  "With the current parameterization of landfill gas removal (section 2.6), the v2.0beta waste emissions should be treated as an upper estimate; future GRA2PES releases will include uncertainty ranges.", "gra2pes 4.3")
M("More broadly, both the aggregate allocation indicates", "More broadly, the aggregate allocation indicates", "both")
tc_insert_at(main, "landfills and wastewater treatment plants, as complementary priorities", " ", "main space such as")

# ---- Figure 1 caption (comment 22) and limitations ---------------------------------------------------
M("Figure 1: AMMBEC flight tracks (thin red lines; all 22 flights)", "Figure 1: (a) AMMBEC flight tracks (thin red lines; all 22 flights)", "fig1 a")
M("is traced as a dark-blue line with a white outline", "is traced as a dark-blue line with a white outline. (b) The analysis box enlarged, with the main point and area methane sources referred to in the text: "
  "the two landfills and three oil-and-gas sites of the Carbon Mapper catalogue (section 2.5), the Metro Water Recovery wastewater plant with the adjacent Suncor refinery and Cherokee gas-fired power station (sections 4.2 and 5), "
  "and the three GRA2PES v2.0beta waste cells tested in section 4.3 (green squares). Circles are the urban transect segments of panel (a), with the same color scale; the street map is © OpenStreetMap contributors", "fig1 b")
q = tc_insert_after(main, "HRRR-STILT footprints convolved with the EPA inventory attribute roughly 26 to 42% of the predicted urban enhancement to basin oil and gas (section S9).",
  "The aircraft sampled within the city and not downwind of it. The within-transect fit therefore characterizes the sources that vary across the city on each pass; an enhancement common to a whole transect does not enter it. "
  "Attributing the entire enhancement above a clean-air background to the city would raise the campaign median to 39 to 44%, an upper bound that includes methane advected from the basin, because the same calculation gives 32 to 42% over the basin itself "
  "(sections 3.2, S13). Transects upwind and downwind of the city on the same flight would be needed to separate the city's increment from the inflow directly.", "limitation")
q.paragraph_format.space_after = None

# ---- SI ----------------------------------------------------------------------------------------------
Sx("an enhancement that raises a whole transect does not change the slope under either background;",
   "an enhancement that raises a whole transect does not change the slope under either background (what this leaves out is examined in section S13);", "S4 pointer")

ref = [p for p in si.paragraphs if tc_text(p).strip() == "References"]; assert len(ref) == 1
body = next(p for p in si.paragraphs if tc_text(p).startswith("The Suncor refinery and Metro Water Recovery wastewater plant are separated"))
h1 = next(p for p in si.paragraphs if tc_text(p).startswith("S12. Facility-scale source separation"))
h2 = next(p for p in si.paragraphs if tc_text(p).startswith("S4.1 Instrument timing"))
cap = next(p for p in si.paragraphs if tc_text(p).startswith("Table S11:"))
def before_refs(text, src, bold=False):
    newp = copy.deepcopy(src._p)
    for ch in list(newp):
        if ch.tag != qn("w:pPr"): newp.remove(ch)
    ref[0]._p.addprevious(newp); q = Paragraph(newp, ref[0]._parent)
    if text is not None and src is cap:                      # caption: bold italic label, italic text, as the others
        lab, rest = text.split(":", 1)
        r = q.add_run(lab + ":"); r.font.color.rgb = RED; r.font.bold = True; r.font.italic = True
        r = q.add_run(rest); r.font.color.rgb = RED; r.font.italic = True
    elif text is not None:
        r = q.add_run(text); r.font.color.rgb = RED
        if src is body: before_refs(None, body)                # the SI separates body paragraphs with an empty one
    return q

before_refs("S13. Sampling inside the city: what the per-transect intercepts leave out", h1)
before_refs("The fossil fractions of Table 1 come from a fit with a separate intercept for every urban transect (section 3.3). Because the aircraft sampled within the urban area and not downwind of it, this has a consequence beyond the choice of "
  "background examined in section S4: a methane enhancement that is uniform over a whole transect, together with the ethane that accompanies it, is taken up by that transect's intercept and does not inform the slope. The within-transect estimate "
  "therefore describes the sources that produce structure within the city on each pass. An enhancement shared by a whole transect, whether a city-wide accumulation or air advected into the city, does not enter it, and its fossil fraction could differ. "
  "This section quantifies how much that matters (S13.1) and examines the one transect flown just outside the box on 9 July (S13.2).", body)
before_refs("S13.1 Estimators with one fixed background per flight", h2)
before_refs("For each flight of Table 1 we computed two estimators that keep the information the intercepts absorb, with one fixed background per flight and species in place of the rolling one. The first is a York slope through all gated "
  "urban-transect samples with a single intercept for the flight (pooled). The second is the ratio of the summed ethane enhancement to the summed methane enhancement over the same samples, which has no intercept and so attributes the entire "
  "enhancement above the background to the sampled sources. Each was computed for three fixed backgrounds: the 1st percentile of the whole flight record, the 5th percentile of the flight's urban-transect samples, and the median of the air above "
  "the boundary layer (section S4; not available for 12 July L1). The gate is ΔCH4 > 20 ppb above the same background.", body)
before_refs("The pooled slope gives campaign medians of 27, 27 and 21.5% for the three backgrounds, against 21% for the within-transect fit (Table S12). The zero-intercept ratio gives 44, 39 and 39.5%, with flight values of 20 to 55%; "
  "two flights are at or above 50% with the lowest background (9 July, 53%, and 10 July L1, 55%) and none with the other two.", body)
before_refs("The zero-intercept ratio is not, however, a measure of the city's emissions, for two reasons. First, the same ratio computed with the same backgrounds over the basin transects of these flights has medians of 42, 35 and 31.5%, "
  "close to the urban values: a quantity that is nearly the same over the gas field and over the city describes the regional air mass above the chosen background, and not the sources beneath the aircraft. Second, on most flights the city did not "
  "add a measurable increment to that air. For the in-box transect samples below 1 km above ground, we compared the methane at the upwind edge of the box (the 15% of samples farthest upwind along the flight's mean wind) with the mean of the "
  "downwind half. On seven of the nine flights the downwind half carried less methane than the upwind edge, by 5 to 84 ppb; only 5 July L1 (+20 ppb) and 9 July (+16 ppb) show an increase. Under the northerly and north-easterly flow of most of "
  "these flights, the air entering the box had already crossed the basin, and what an aircraft inside the box measures above a clean-air background is largely that inflow. This comparison mixes transects flown at different times and heights and is "
  "indicative only. The ratio also depends directly on the two fixed backgrounds, which a slope does not: on 10 July L1 it moves from 39 to 55% across the three backgrounds of Table S12.", body)
before_refs("We therefore keep the within-transect estimate as the measure of the urban source mix and report the zero-intercept ratio as an upper bound: if the whole enhancement above a clean-air background were attributed to the city, the campaign "
  "median would be 39 to 44%, still below an even split. The pooled slope with a fixed background (21.5 to 27%) lies close to the within-transect value. Separating the city's increment from the inflow directly would need transects upwind and "
  "downwind of the city on the same flight, which flight plans centred on the basin did not provide.", body)
before_refs("Table S12: Fossil fraction (%, at the adopted endmember of 0.102 mol mol−1) from estimators with one fixed background per flight, for the nine flights of Table 1. Each triplet gives the result for the three backgrounds in the order "
  "1st percentile of the flight record / 5th percentile of the flight's urban-transect samples / median of the air above the boundary layer (-: not available). Pooled: York slope through all gated urban-transect samples with one intercept. "
  "Ratio: summed ethane enhancement over summed methane enhancement, no intercept, for the urban and for the basin transects of the same flight. Wind: vector-mean wind of the in-box transect samples below 1 km above ground. "
  "Downwind minus upwind: mean methane of the downwind half of those samples minus the median of the 15% farthest upwind.", cap)
anchor_tbl = before_refs(None, body)
L._table_from_csv(anchor_tbl, si, "fixed_background_estimators_table.csv|Flight=Flight,within=Within-transect (Table 1),pooled=Pooled York, fixed background,ratio=Zero-intercept ratio, urban,"
                  "ratio_basin=Zero-intercept ratio, basin,wind=Wind (from, speed),dch4=Downwind minus upwind CH4 (ppb)".replace("Pooled York, fixed", "Pooled York; fixed").replace("ratio, urban", "ratio; urban").replace("ratio, basin", "ratio; basin").replace("(from, speed)", "(from; speed)"))
anchor_tbl._p.getparent().remove(anchor_tbl._p)
before_refs("S13.2 The 9 July transect east of the city", h2)
before_refs("On 9 July the aircraft flew a north-south transect along 104.39°W, about 14 km east of the eastern edge of the analysis box, once at 0.22 to 0.71 km above ground (17:51 to 18:17 UTC; boundary-layer height from the airborne lidar 1.7 km) "
  "and once at 2.05 to 2.58 km (18:30 to 18:52 UTC), near the top of the boundary layer measured then (2.7 km). It is the wall listed in Table S2 as N-S, 42 km. It was flown as a crossing of the urban plume, but the winds were becoming light (2 to 4 m/s on the low pass), "
  "which rules out a mass balance; a ratio analysis does not need the wind. The transect lies outside the box and so is not part of Table 1.", body)
before_refs("With one fixed background (the 1st percentile of the flight record), the low pass at box latitudes (39.50 to 39.95°N, 735 samples) gives a York slope of 0.038 mol mol−1 (r = 0.85), or 37% fossil at the adopted endmember, with an "
  "intercept of 0.7 ppb of ethane (Figure S19); the southern and northern halves of that stretch each give 0.033 (32%). That agrees with the 34% (25 to 39%) obtained inside the box on the same flight. The ratio of the summed enhancements, which keeps the intercept, is 0.052 mol mol−1 (51%). "
  "The same ratio is found on that pass north of 40.05°N (0.055; fitted slope 0.021) and on the high pass at box latitudes (0.052; fitted slope 0.043, r = 0.89), "
  "so it is a property of the air mass over the whole transect and not of the part of it east of the city.", body)
before_refs("HRRR-STILT footprints (section S9) for 16 receptors on the transect show where that air came from. The measured wind on the low pass was from 357° at 3.6 m/s (vector mean), and the 10-hour particle clouds reach the transect from the "
  "north-north-west, over the basin and the north-eastern corner of the box. For the eight receptors on the low pass at box latitudes, cells inside the box hold a median of 27% of the footprint itself, which involves no inventory (3% at the northern end to 52% at the southern end), "
  "and contribute 12% of the enhancement predicted from the EPA gridded inventory (1 to 42%); cells north of 40.05°N contribute 43% of that enhancement, and basin-type oil and gas accounts for 41% of it. "
  "For the four receptors on the high pass the box holds 13% of the footprint. Consistent with this, methane and CO are correlated on the southern half of the low pass at box latitudes (r = 0.65), "
  "where the box holds a third to a half of the footprint, and not on its northern half (r = -0.29); inside the box on this flight r = 0.85 (Table S6).", body)
before_refs("The transect is therefore only partly downwind of the city. Its fitted slopes support the in-box result for one of the campaign's most fossil-influenced flights, and the ratio of about 0.05 between its total ethane and "
  "methane enhancements, which recurs at basin latitudes and near the top of the boundary layer, is largely the signature of regional air arriving from the basin under northerly flow (S13.1). On the southern half of the low pass, "
  "where the influence of the city is largest, that ratio is 0.057 (56%). It rests there on enhancements of about 35 ppb of methane and 2 ppb of ethane above backgrounds taken from elsewhere on the flight, "
  "so that a change of 0.5 ppb in the ethane background moves it by about 14 percentage points; the fitted slope does not depend on those backgrounds.", body)
before_refs(None, body); fp = before_refs(None, body); fp.alignment = 1
fp.add_run().add_picture(str(OUT / "figures" / "FigS_downwind_leg_0709.png"), width=Inches(6.5))
before_refs("Figure S19: The 9 July transect along 104.39°W, about 14 km east of the analysis box. Left and middle: ethane against methane enhancement above a fixed background (the 1st percentile of the flight record) on the low and high passes, "
  "colored by latitude. Solid line: York fit to the samples at box latitudes (39.50 to 39.95°N); dashed line: ratio of the summed enhancements at those latitudes (no intercept). Right: centroids of the HRRR-STILT particle clouds (500 particles, "
  "10 h back; dots every 2 h) released from 16 receptors on the transect (crosses), with the analysis box, the 40.05°N basin boundary and the top decile of basin-type oil-and-gas cells in the EPA gridded inventory (red contour).", cap)

for d in (main, si):
    ps = L.all_paragraphs(d); L.subscript_chem(ps); L.exponent_fix(ps)
main.save(a.main_out); si.save(a.si_out)
O.swap_figures(a.main_out, {"image4.png": OUT / "figures" / "Fig1_study_map_sources.png"}, {"image4.png"}, "main")
print("wrote", a.main_out, "and", a.si_out)
