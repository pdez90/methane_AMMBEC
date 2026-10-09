"""89_transect_and_comment_edits.py -- constant-altitude transects and reviewer comments (9 Oct 2026),
applied to the author's current manuscript and SI (the versions carrying the co-author comments).

1. Every transect-dependent number, table and figure is brought to the constant-altitude transect
   definition (R/transects.R; LEG_METHOD = "transect"), read from the outputs of run_all.R and
   scripts 88 and 90. The vertical-speed level legs are reported as a sensitivity (SI S3).
2. Section 3.3 (leg detection) becomes section 3.1, "Constant-altitude flight transects"; the old 3.1 and
   3.2 become 3.2 and 3.3, and every "section 3.2" reference in the original text follows.
3. The co-author comments are answered in the text (red), and the comment anchors are kept so each
   comment can be replied to and resolved in Word. A response table is written next to the outputs.
4. Tables 1, S1-S3, S5-S7 and S10 are rebuilt from the CSVs; Table S9 carries the persistence-weighted
   Carbon Mapper rates (102 and 420 kg/h); the figures are swapped, with the frame resized where the
   aspect ratio changed (Figures S1, S13 and S16).
All inserted text is red (C00000). Nothing refers to earlier versions of the analysis.

usage: python3 scripts/89_transect_and_comment_edits.py <main.docx> <si.docx> <main_out> <si_out>
         --outputs <OUT_DIR> --figures <OUT_DIR>/figures --response <response.docx>
"""
import sys, csv, copy, re, argparse, zipfile, shutil, struct, statistics as st
from pathlib import Path
sys.path.insert(0, str(Path(__file__).resolve().parent))
import docx
from docx.oxml.ns import qn
from docx.text.run import Run
from docx.text.paragraph import Paragraph
from docx.shared import Pt
import docx_edit_lib as L
RED = L.RED

ap = argparse.ArgumentParser()
for k in ("main_in", "si_in", "main_out", "si_out"): ap.add_argument(k)
ap.add_argument("--outputs", required=True); ap.add_argument("--figures", required=True); ap.add_argument("--response", required=True)
a = ap.parse_args(); OUT = Path(a.outputs); FIG = Path(a.figures); L.OUTDIR = str(OUT); L.FIGDIR = str(FIG)
rd = lambda f: list(csv.DictReader(open(OUT / f)))
num = lambda v: float(v) if v not in ("", "NA", None) else None

# ================================================================================================
# numbers from the outputs (each value used in the text is asserted, so a rerun that changes it stops here)
# ================================================================================================
T1 = rd("table1.csv"); T1F = {r["flight"]: r for r in rd("table1_full.csv")}
FL = [r["Flight"] for r in T1 if r["Fossil_pct_CI"] != "NA"]                       # the nine flights
assert len(T1) == 16 and len(FL) == 9, (len(T1), len(FL))
fos = {f: int(T1F[f]["fossil_pct"]) for f in FL}
assert st.median(fos.values()) == 21 and min(fos.values()) == 0 and max(fos.values()) == 35
slopes = sorted(float(T1F[f]["ethane_slope"]) for f in FL)
assert round(st.median(slopes), 4) == 0.0211 and round(slopes[-1], 3) == 0.036
TOT = {r["scope"]: r for r in rd("campaign_sampling_totals.csv")}
assert TOT["all flights"]["n_transects"] == "398" and TOT["all flights"]["n_box_transects"] == "109"
assert TOT["flights with a fossil fraction"]["inbox_transect_s"] == "24158" and TOT["flights with a fossil fraction"]["n_gated"] == "4475"
assert TOT["flights with a fossil fraction"]["n_box_transects"] == "58" and TOT["all flights"]["inbox_transect_s"] == "29790"
RF = {r["flight"]: r for r in rd("ratio_method_flux.csv")}
co_fl = [f for f in FL if RF[f]["usable_co"] == "TRUE"]
assert co_fl == ["20240703_R0_L1", "20240703_R0_L2", "20240709_R0", "20240713_R0_L1"], co_fl
assert not any(RF[f]["usable_co2"] == "TRUE" for f in RF)
rco2 = [float(RF[f]["r_co2"]) for f in co_fl]
assert (min(rco2), max(rco2)) == (0.45, 0.63), rco2
rco2_9 = max(float(RF[f]["r_co2"]) for f in FL)
E = sorted(float(RF[f]["E_CH4_from_CO_t_hr"]) for f in co_fl)
assert (E[0], E[-1]) == (4.44, 10.27) and round(st.median(E), 3) == 6.455
qc = {r["flight"]: r for r in rd("qc_robustness.csv")}
assert [int(qc[f]["delta"]) for f in FL] == [0, 0, 0, 17, 0, 1, 0, 8, 9]
bs = rd("basin_summary.csv")[0]; assert bs["screened_median"] == "0.0316" and bs["n_flights_screened"] == "4"
be = float(rd("beta_breakeven.csv")[0]["beta_breakeven"]); assert round(be, 3) == 0.042
cs = rd("denver_curtain_screen.csv"); assert len(cs) == 48 and len({r["flight"] for r in cs}) == 20
cnt = {k: sum(r[k] == "TRUE" for r in cs) for k in ("city_upwind", "width_ok", "vertical_ok", "wind_ok")}
assert cnt == {"city_upwind": 11, "width_ok": 9, "vertical_ok": 2, "wind_ok": 14}, cnt
assert max(int(r["conditions_met"]) for r in cs) == 3
wf = {r["flight"]: r for r in rd("wind_fossil.csv")}
assert (wf["20240713_R0_L1"]["pct_from_NE"], wf["20240713_R0_L2"]["pct_from_NE"]) == ("77", "68")
ws = {r["test"][:30]: r for r in rd("wind_source_exposure_summary.csv")}
lag = {r["lag_s"]: r for r in rd("lag_sensitivity_summary.csv")}
assert (lag["0"]["median_fossil_pct"], lag["-1"]["median_fossil_pct"]) == ("20.7", "22")
eps = rd("ethane_precision_sensitivity.csv")
assert round(st.median(float(r["fossil_pct_sy_empirical"]) for r in eps)) == 24 and round(st.median(float(r["fossil_pct_sy0.5"]) for r in eps)) == 20
bg = rd("background_scatter.csv"); bgv = [float(r["bg_scatter_ppb"]) for r in bg]
assert len(bg) == 19 and round(st.median(bgv), 1) == 3.0 and max(bgv) == 40.62
bm = {r["flight"]: r for r in rd("background_mode_sensitivity.csv")}
assert [int(bm[f]["fossil_pct_track"]) for f in FL] == [29, 0, 8, 0, 32, 16, 22, 41, 43]
hs = {r["nearest_facility"]: r for r in rd("v2_pointsource_hotspots.csv")}
assert round(float(hs["Denver Arapahoe Disposal Site (DADS)"]["carbonmapper_kg_hr"])) == 102 and round(float(hs["Tower Road landfill"]["carbonmapper_kg_hr"])) == 420
enc = [r for r in rd("v2_pointsource_encounters.csv") if r["hotspot_in_fetch"] == "TRUE"]
seen = set(); ENC = []
for r in enc:                                    # the CSV repeats one crossing; keep each (flight, transect, site) once
    k = (r["flight"], r["leg_id"], r["hotspot_id"])
    if k not in seen: seen.add(k); ENC.append(r)
assert len(ENC) == 15 and len({r["flight"] for r in ENC}) == 7
rb = sorted(float(r["ratio_bio_over_v2waste"]) for r in ENC if r["ratio_bio_over_v2waste"] not in ("NA", "Inf"))
rt = sorted(float(r["ratio_total_over_v2total"]) for r in ENC)
ENC_RB, ENC_RT = st.median(rb), st.median(rt)
qmin = min(float(r["Q_t_hr"]) for r in ENC); qmax = max(float(r["Q_t_hr"]) for r in ENC)
bmax = max(float(r["Q_bio_t_hr"]) for r in ENC if r["Q_bio_t_hr"] != "NA")
assert (qmin, qmax, bmax) == (0.11, 7.73, 5.54), (qmin, qmax, bmax)
FS = {r["flight"]: r for r in rd("flight_summary.csv")}
assert len(FS) == 16

def lab(f):              # 20240703_R0_L1 -> 2024-07-03 (L1)
    m = re.match(r"(\d{4})(\d\d)(\d\d)_R0_?(L\d)?", f)
    return f"{m.group(1)}-{m.group(2)}-{m.group(3)}" + (f" ({m.group(4)})" if m.group(4) else "")
def lab2(f):             # 20240703_R0_L1 -> 20240703 (L1)
    m = re.match(r"(\d{8})_R0_?(L\d)?", f)
    return m.group(1) + (f" ({m.group(2)})" if m.group(2) else "")
def ci(s):               # "24 [23-26]" -> "24 [23 to 26]"
    return re.sub(r"\[(-?[\d.]+)-(-?[\d.]+)\]", r"[\1 to \2]", s)
def rnd(x): return int(abs(x) + 0.5) * (1 if x >= 0 else -1)

# ================================================================================================
# edit registry
# ================================================================================================
OPS = {"main": [], "si": []}
def R(doc, old, new, count=1): OPS[doc].append(("R", old, new, count))
def RW(doc, anchor, text): OPS[doc].append(("RW", anchor, text))
def INS(doc, anchor, text): OPS[doc].append(("INS", anchor, text))
def TAB(doc, ti, rows, header=None, widths=None): OPS[doc].append(("TAB", ti, rows, header, widths))

# ================================================================================================
# MAIN TEXT
# ================================================================================================
M = "main"
# ---- abstract (comment 1 stays on the tracked insertion; comment 2 asks for a closing sentence) ----
RW(M, "A city's methane emissions arise from its natural-gas system",
   "A city's methane emissions arise from its natural-gas system and biogenic sources such as landfills, wastewater treatment plants, and agriculture. "
   "Fossil methane co-emits ethane, while biogenic methane does not. Airborne ethane-to-methane ratios can distinguish the two. "
   "We evaluate this split using the 2024 Airborne Methane Mass Balance Emissions in Colorado (AMMBEC) campaign, flown over Denver and the adjacent Denver-Julesburg Basin. "
   "Nine of the 16 flights{{c1}}{{x0}}{{/c1}} that crossed the Denver analysis box on at least three constant-altitude transects yielded estimable urban ethane-methane slopes. "
   "At an effective fossil endmember of 0.102 mol mol−1, the campaign-median fossil fraction was 21% (0 to 35% across flights), toward the lower end of fractions reported in previous airborne urban studies. "
   "Across regional reference endmembers (0.081-0.110 mol mol−1) the median was 19 to 26%, indicating a more biogenic source mix over the sampled urban area than reported for the adjacent producing basin; "
   "the median exceeds 50% fossil only for an effective endmember below 0.042 mol mol−1. "
   "Scaling the CH4:CO enhancement ratio by a gridded CO inventory yielded 4.4 to 10.3 t CH4/h (median 6.5) on four flights passing a reliability screen; no flight passed the corresponding CH4:CO2 screen. "
   "These anchor-dependent rates are less certain than the source mix and exceed the two published box-consistent bottom-up inventories (1.7 and 2.8 t CH4/h) by about 2.3 to 3.8-fold at the CH4:CO median, "
   "but fall below the 16.5 t CH4/h of the GRA2PES v2.0beta release, which adds substantial waste methane. "
   "The airborne fossil fraction is below the EPA gridded inventory's ~60%{{c2}}.{{/c2}} "
   "These results indicate that in summer, landfills and wastewater rather than the natural-gas system dominated the methane sampled over Denver, "
   "so monitoring and mitigation focused only on the gas system would miss most of the city's observed methane.")

# ---- introduction ----
R(M, ". Their emissions arise from a mix", ".[[^2,3,5]] Their emissions arise from a mix")       # comment 3
R(M, ". The ethane-to-methane ratio of urban plumes", ".[[^3]] The ethane-to-methane ratio of urban plumes")  # comment 4
RW(M, "The Colorado Front Range is a setting where the methane source mix",                                             # comments 5, 6
   "The Denver metropolitan area is home to three million people. It abuts the Denver-Julesburg oil-and-gas basin (DJB) to the northeast, one of the most productive tight-gas regions in the United States. "
   "Two decades of research have quantified methane from the DJB gas field using flux towers, mobile laboratories, aircraft mass balance, and satellites.{{h0}} "
   "These studies have reported that oil-and-gas emissions exceed inventory estimates, with a declining emission intensity over {{c6}}time (for example, across 2010 to 2019 in the satellite record){{/c6}}. "
   "{{c5}}Despite these two decades of basin research, the methane source mix of the Colorado Front Range's urban core has been understudied: {{/c5}}far less attention has been paid to methane emissions from the Denver metropolitan area itself, as distinct from the adjacent gas field. "
   "Information about the source mix is important for informing municipal and state greenhouse-gas mitigation strategies. "
   "However, the proximity of the DJB to the Denver metropolitan area makes this work challenging as, without measurements of a fossil tracer, urban biogenic methane and advected basin gas are difficult to separate.")
R(M, "on the downwind side, and it captures the plume", "(typically three or more level legs at different altitudes spanning the boundary layer) on the downwind side, and it captures the plume")  # comment 7
R(M, "direct evaluation of 24 candidate screens", "direct evaluation of 48 candidate screens")
R(M, "We analyze methane enhancements within geographically defined urban flight legs,",
     "We analyze methane enhancements on constant-altitude flight transects inside a geographically defined urban analysis box,")

# ---- 2. Data (comment 8: a summary of the data streams) ----
INS(M, "2. Data",
    "The analysis draws on six data streams, each with a distinct role. The aircraft in situ record (section 2.1; 1 Hz, 22 flights between 28 June and 13 July 2024) is the primary dataset and supplies every fossil fraction and emission ratio. "
    "Two ground platforms provide context only: the CDPHE facility surveys (section 2.2; methane only, 2023 to 2025) characterize the facility source types, and the NOAA ARL mobile laboratory (section 2.3; methane and ethane, 24 June to 15 July 2024) checks the airborne enhancement ratio and the ethane endmember. "
    "Doppler-lidar mixing heights and the aircraft's own winds (section 2.4) document the boundary layer and the transport. Carbon Mapper's plume catalogue (section 2.5) locates discrete point sources, "
    "and gridded emission inventories (section 2.6) supply the CO anchor of the emission estimates and the bottom-up comparison.")
RW(M, "Airborne measurements were made at 1 s resolution from the NOAA Twin Otter",                                     # comments 9, 10, 11
   "Airborne measurements were made at 1 s resolution from the NOAA Twin Otter (N46RF) on 12 days between 28 June and 13 July 2024, over the DJB basin and the Denver metropolitan area (Figure 1). "
   "A Picarro G2401-m and an Aeris mid-IR ethane analyzer measured {{c9}}CH4, C2H6, CO2, CO, and H2O{{/c9}}. "
   "The archived files (revision R0) are preliminary field data based on the pre-campaign laboratory calibration of the analyzers; they give 1σ uncertainties of 1 ppb for CH4, 0.2 ppb for C2H6, 0.1 ppm for CO2 and 5 ppb for CO, which we use as the instrument precisions in the York fits (sections 3.3 and 3.4), "
   "and the Picarro record has a native resolution of about 2.5 s on the 1 s grid. "
   "[Authors to complete with the instrument PIs: calibration standards and their traceability, in-flight zero and span checks, and data-flagging criteria.] "
   "Additional sensors measured O3, NO, NO2, and NOy; however, none of these four channels is used in this analysis. Differential GPS and an AIMMS probe recorded aircraft position, altitude, and winds. "
   "The archive holds {{c10}}22 flight files{{/c10}} containing CH4 and C2H6, because 10 of the 12 flight days were archived as two separate files, denoted L1 and L2 (a morning and an afternoon flight); we treat each file as an independent flight. "
   "Of the 22, 20 crossed the Denver-metro analysis box and 16 crossed it on at least three constant-altitude transects (section 3.1), the flights screened for the source-mix and emission analyses; "
   "9 of those, on 7 days, yielded an estimable urban ethane-methane slope. These 9 are the flights in {{c11}}Table 1{{/c11}}, which is why some dates appear twice.")
R(M, "2.2. Ground-based mobile surveys", "2.2. CDPHE facility surveys")                                               # comment 12
RW(M, "conducted repeated vehicle-based methane",                                                                        # comments 13, 14, 15
   "The Colorado Department of Public Health and Environment (CDPHE) conducted repeated vehicle-based methane {{c13}}surveys{{/c13}} at three Front Range industrial facilities: a refinery (Suncor P66), a gas terminal (HEP), and a Collins Aerospace site. "
   "Unlike the NOAA ARL mobile laboratory (section 2.3), which drove regionally during the flight campaign and measured ethane, these are methane-only surveys that repeatedly circle the same three facilities over 2023 to 2025, so they describe persistent facility plumes rather than the city-scale source mix. "
   "The {{c14}}surveys measured CH4{{/c14}} in ppmv with a Picarro G2204 cavity ring-down analyzer and differential GPS, delivered on a 1 s grid (the analyzer acquires about every 5 s); "
   "the inlet-delay correction, the averaging to the analyzer's native cadence and the removal of samples at the vehicle depot are described in section S2.1. "
   "{{c15}}The surveys span 801 to 925 days per site.{{/c15}} These facilities lie within the metropolitan area, so their emissions are part of the urban source field sampled by the aircraft.")
R(M, "2.3. Ground-based ethane measurements", "2.3. NOAA ARL mobile laboratory")
R(M, "with an Aeris MIRA Ultra measuring CH4 and C2H6. Its coverage",                                                    # comment 16
     "with an Aeris MIRA Ultra measuring CH4 and C2H6 among a larger instrument suite (section S2.2; the plumes it crossed are mapped in Figure S3). Its coverage")
R(M, "own measurements. The High-Resolution", "own measurements (wind roses for the flights and for June and July 2024 are in Figure S13). The High-Resolution")  # comment 17
R(M, "spanning 0.35 to 3.35 km across the eight urban flights.", "spanning 0.35 to 3.35 km across the nine flights with estimable urban slopes (Table 1).")  # comment 18
R(M, "as the median over the urban legs' time window", "as the median over the urban transects' time window")
R(M, " methane plumes and one CO2 plume detected between", " methane plumes and one CO2 plume (32 plumes in all) detected between")   # comment 19

# ---- 3.2 (was 3.1) background and enhancement ----
R(M, "3.1. Background and enhancement", "3.2. Background and enhancement")
R(M, "(t), we estimated a background as a rolling ",                                                                     # comment 20
     "(t) (CH4, C2H6, CO and CO2 for the aircraft; CH4 and C2H6 for the ARL mobile laboratory; CH4 for the CDPHE surveys), we estimated a background as a rolling ")
R(M, "quantile within a moving time window", "5th percentile within a moving time window")                              # comment 21
R(M, "quiet residuals", "quiet-period enhancements")                                                                   # comment 22
RW(M, "With the aircraft data, this procedure produced enhancements",                                                    # comment 24
   "With the aircraft data, this procedure produced enhancements in CH4, C2H6, CO, and CO2 that feed the source-apportionment and flux analyses. "
   "There the series is each flight's urban transects joined end to end (basin transects for the basin ratio, section S5), so near a transect boundary the window spans the adjacent transect and the background is the cleanest air sampled on neighboring urban transects, which retains the urban-scale enhancement (section S4). "
   "The gridded signature map of Figure 4B and section S7.1, which bins every boundary-layer sample rather than transects, computes the same {{c24}}300-sample{{/c24}} background (5 minutes, about 20 km of flight; the choice of window is explained below) along the continuous flight record.")
R(M, "the background changes over much shorter distances.",                                                             # comment 25
     "the background changes over much shorter distances. Here the background is the local baseline beside the road rather than the regional background: "
     "at street level a survey vehicle is often inside the diffuse plume of the same facility or of nearby roads and sources, so the cleanest air within a few hundred metres shifts as the vehicle turns corners and as the wind shifts, even though the regional background varies slowly.")

# ---- 3.3 (was 3.2) source apportionment ----
R(M, "3.2. Source apportionment from the ethane-to-methane ratio", "3.3. Source apportionment from the ethane-to-methane ratio")
RW(M, "Because individual urban legs sample distinct air masses",
   "Because individual urban transects sample distinct air masses with different ethane backgrounds, we fit a fixed-effects York regression with a separate intercept for each transect and a common slope across transects. "
   "Before fitting, enhancements were centered on each transect's mean, and transects contributed according to their methane variance. "
   "Pooling all transects in a single regression would have combined the within-transect relationship with differences among transect means and might even have reversed the estimated slope when different transects sample different source mixtures (section S3). "
   "We excluded transects with fewer than ten in-plume samples; every flight in Table 1 retained at least three qualifying transects, so each has a transect-block bootstrap interval.")
R(M, "We used the within-leg York fit as the primary estimator.", "We used the within-transect York fit as the primary estimator.")
R(M, "we also fitted the pooled leg data", "we also fitted the pooled transect data")
RW(M, "The fit only included points with ΔCH4 > 20 ppb",
   "The fit only included points with ΔCH4 > 20 ppb, a threshold well above both the 1 ppb instrument precision and the typical scatter of the rolling background (median 3.0 ppb across the 19 flights with urban transects; section S4), "
   "so the estimated slope is driven by plume enhancements rather than background variability. "
   "The biogenic-dominated conclusion is robust to considering thresholds from 5 to 50 ppb for the flights evaluable at every threshold, although the inferred fossil fraction shifts somewhat with the threshold, as it does with the assumed endmember (sections S3-S4). "
   "Methane and ethane are measured by separate analyzers, so we also tested the time alignment of the two records. Specifically, ethane leads methane by about 1 s; correcting for this changes the campaign median fossil fraction from 21 to 22%, "
   "and no flight becomes majority fossil for any shift between -5 and +5 s (section S4.1, Figure S8). The in-flight 1 Hz ethane scatter (0.19 ppb) is consistent with the 0.2 ppb precision assumed in the fit.")
R(M, "we quantified uncertainty using a leg-block bootstrap", "we quantified uncertainty using a transect-block bootstrap")
R(M, "We resampled the level legs entering each fit", "We resampled the transects entering each fit")
R(M, "only when βsource falls below 0.059 mol mol−1.", "only when βsource falls below 0.042 mol mol−1.")
RW(M, "Urban fossil methane may, however, combine advected DJB emissions",
   "Urban fossil methane may, however, combine advected DJB emissions with leakage from the distribution system. We therefore also evaluated an effective endmember that blends these two signatures across the full range of mixture proportions (section S3, Figure S5). "
   "With the measured Denver delivered-gas composition, the campaign median ranged from 19 to 26% fossil for the regional basin values and reached at most 35% under an assumed lean basin ratio of 0.061 mol mol−1; it never reached 50%. "
   "Among the scenarios considered, only the much more ethane-poor gas measured in East Coast cities (0.019-0.037 mol mol−1) could have raised the median above 50%, with the required distribution-gas contribution ranging from approximately 62% to 91% for basin ratios of 0.0813 and 0.102 (44% to 77% under the assumed 0.061), depending on the distribution-gas ratio. "
   "However, this composition is inconsistent with Denver's measured delivered gas.")
RW(M, "The campaign observations provided two conditional checks",
   "The campaign observations provided two conditional checks on the effective endmember. First, under the two-component, fixed-composition model of Equation 3, an atmospheric slope cannot exceed the effective fossil endmember. "
   "Among transects with strong ethane-methane correlations (r ≥ 0.7; at least 30 samples), the steepest slopes were 0.024-0.044 mol mol−1 across four flights. "
   "Even the strongest of these transect-level floors (0.043-0.044 mol mol−1, on both 13 July flights) lies below every endmember considered, including the assumed lean basin ratio of 0.061 mol mol−1, and with East Coast distribution gas it would still allow 41 to 72% of that transect's fossil methane to be distribution gas, so the floors do not exclude an ethane-poor fossil source (section S3). "
   "Second, the DJB basin transects yielded ambient C2H6:CH4 enhancement ratios of 0.020 to 0.035 mol mol−1 on the four flights passing the screen (median 0.032). "
   "If oil and gas account for approximately half of the basin methane enhancement,{{h0}} these imply an oil-and-gas source ratio of approximately 0.063 mol mol−1 (section S5); "
   "that share is above the 39% the 0.0813 production-area ratio itself implies, so the value illustrates the sensitivity to the assumed share rather than adding an independent constraint.")
RW(M, "This range is below the adopted 0.102 mol mol-1",
   "This value is below the adopted 0.102 mol mol−1,{{h0}} consistent with the decline reported since 2015,{{h1}} although it rests on the assumed oil-and-gas share and cannot be compared reliably with the ratio of about 0.10 implied by the 2021 basin emissions (section S5); "
   "it remains above the 0.042 threshold at which the campaign median becomes majority fossil. Using this lower, conditional value would increase the campaign median fossil fraction to approximately 34% without changing the overall biogenic-dominated conclusion. "
   "Individual flights near 50% are more sensitive to endmember choice. We thus emphasize the campaign median and the full sensitivity range rather than per-flight majority classifications as key results.")
R(M, "changes any flight by at most 3 percentage points and preserves the ranking (section S3).",
     "leaves the campaign median unchanged at 21%, although it shifts three flights by 8 to 17 percentage points (section S3).")

R(M, "repeated the within-leg regression for each resample", "repeated the within-transect regression for each resample")
# ---- 3.1 (was 3.3): constant-altitude transects (comment 27; Sunil's SI comment 0) ----
RW(M, "3.3. Level-leg detection", "{{c27}}3.1. Constant-altitude flight transects{{/c27}}")
RW(M, "Straight, level legs, continuous flight segments",
   "The sampling units of the analysis are constant-altitude transects: continuous flight segments in which the aircraft holds an approximately constant altitude and a straight ground track. "
   "Each samples one air mass at a single level, so transects serve as the blocks of the bootstrap and, for the ethane fit, carry their own intercepts (section 3.3). "
   "Excluding turns, climbs and descents avoids banking and altitude changes that mix sampling heights and blur plume structure. Their role in a mass-balance flux integral, which this campaign's geometry does not support, is described in section S1. "
   "No list of planned flight legs was available, so transects were identified from the aircraft's navigation record alone (GPS altitude, ground track and position), with a rule fixed before any trace-gas variable was examined.")
RW(M, "We note that a change in heading does not inherently invalidate a transect",
   "A change in heading does not inherently invalidate a transect, and a transect containing a turn may remain usable under favorable wind conditions. Our criterion is therefore practical: each transect should represent a well-defined sampling level. "
   "The data files carry two vertical-rate variables, the avionics Vertical_Speed and the differential-GPS Heave, but both run at about six times the rate of change of the GPS altitude (r ≈ 0.99 when smoothed over 30 s), "
   "so a threshold on either does not correspond to a physical climb rate. We therefore define transects directly from the GPS altitude and the ground track (section S3).")
RW(M, "Specifically, a level leg is a run where the 20-second-smoothed vertical speed",
   "Specifically, a transect is grown sample by sample while each new 1 Hz sample lies within ±50 m of the running median GPS altitude of the transect and the 20 s running circular mean of the ground track stays within 30° of the transect's initial heading (its circular mean over the first 10 s). "
   "Runs of up to 20 samples failing either test are bridged only when a passing sample follows, so a transect never begins or ends in a turn, climb or descent. "
   "Samples at either end lying more than 25 m from the transect's median altitude are trimmed, and a candidate is rejected if the least-squares trend of its altitude exceeds 0.25 m/s, since a slow approach or descent is not level flight. "
   "A transect must last at least 60 s and cover at least 2 km. Consecutive transects separated by at most 30 s whose mean tracks differ by at most 15° and median altitudes by at most 50 m are merged as one pass split by a gentle course correction. "
   "Applied to the 22 flight files, the rule gives 398 transects, 109 of which cross the Denver-metro box (39.50 to 39.95°N, 105.20 to 104.55°W; Figure 1, Figure S1). "
   "Transects at every altitude are used, within and above the mixed layer; an in-boundary-layer restriction is applied only as a sensitivity test (section S3). "
   "Region is assigned sample by sample, so a transect crossing the box contributes only its in-box samples: those form an 'urban' transect, samples north of 40.05°N, over the adjacent gas field, form a 'basin' transect, and samples in the gap between the two regions are excluded. "
   "A flight entered the per-flight source-mix and emission analysis only if it had at least three urban transects and at least 50 urban-transect samples. "
   "Repeating the analysis with level legs defined by a threshold on the avionics Vertical_Speed field gives a campaign-median fossil fraction of 29% (section S3).")
INS(M, "Applied to the 22 flight files, the rule gives 398 transects",                                                   # comment 23
    "Conceptually, we treat the air in the analysis box at each transect altitude as a mixture of a regional background and the overlapping plumes of many urban sources, not as uniformly mixed air. "
    "In the convective daytime boundary layer, plumes from individual landfills, wastewater plants, gas leaks and roads spread vertically within a few kilometres and merge downwind into a broad urban enhancement, on which narrower plume peaks from nearby sources are superimposed. "
    "Each transect records a sequence of such mixtures, and the ethane-to-methane slope fitted across a flight's transects measures the methane-weighted mix of the sources upwind during that flight. "
    "That mix changes with the wind: north-easterly flow crosses the Wattenberg/DJB field and the industrial corridor of north Denver before reaching the box, whereas westerly flow arrives from the mountains with little upwind oil and gas (section 4.2). "
    "Figure 1 shows the box relative to the DJB, Figure S10 and Table S9 locate the landfills, wastewater plants and mapped point sources, and the per-flight winds are listed in Table S1 and summarized as wind roses in Figure S13.")

# ---- 3.4, 3.5 ----
R(M, "We therefore fit all urban-leg samples; with the filters applied, no flight's fossil fraction changes by more than 3 percentage points (Table S3).",
     "We therefore fit all urban-transect samples; with the filters applied, the campaign median is unchanged (21%), although three flights change by 8 to 17 percentage points (Table S3).")
R(M, "Each fit uses the urban-leg samples in which", "Each fit uses the urban-transect samples in which")
R(M, "95% leg-block bootstrap interval, obtained by resampling whole-level legs with replacement",
     "95% transect-block bootstrap interval, obtained by resampling whole transects with replacement")
R(M, "characterized by the leg-block bootstrap interval", "characterized by the transect-block bootstrap interval")
R(M, "leave-one-leg-out", "leave-one-transect-out", 3)
R(M, "2) at least two contributing level legs,", "2) at least two contributing transects,")

# ---- 4.1 ----
RW(M, "Of the 22 in situ flights, 11 included legs",
   "Of the 22 in situ flights, 20 crossed the Denver-metro box on at least one constant-altitude transect, with tracks reaching as far south as 39.28°N (3 July L1), and 16 crossed it on at least three transects (Figure 1, Figure S16). "
   "Together the 22 flights flew 398 transects, 109 of them crossing the box, for 8.3 h (29,790 s) of transect flight inside it. "
   "We applied the within-transect instrument-weighted York regression described in section 3.3 to each flight's urban ethane and methane enhancements; nine flights on seven days, with 58 urban transects, "
   "about 6.7 h (24,158 s) of in-box transect flight and 4,475 samples above the 20 ppb gate, yielded an estimable slope (Table 1, Table S1). "
   "At the adopted endmember, inferred fossil fractions range from 0 to 35%, with a median of 21% (Figure 2, Table 1). "
   "Across the regional reference endmembers (0.081-0.110 mol mol−1), the campaign median ranges from 19 to 26%; an assumed lean basin ratio of 0.061 mol mol−1 would give 35% (sections 3.3, S3).")
R(M, "against 0.029 mol mol−1 (0.002 to 0.048) for the flights", "against 0.021 mol mol−1 (-0.002 to 0.036) for the flights")
RW(M, "The campaign-level source-mix inference is therefore based on the distribution",
   "The campaign-level source-mix inference is therefore based on the distribution of flight-level slopes, their median, and the robustness of that median to endmember choice (section 3.3, Figure S4). "
   "Transect-block bootstrap intervals quantify uncertainty for individual flights: six of the nine lie entirely below 50%. "
   "The three that reach it, 5, 8 and 10 July L1, belong to low-correlation flights whose transects disagree (section S3).")
RW(M, "The most fossil-influenced flights are 13 July L2",
   "The most fossil-influenced flights are 13 July L1 (35%; 95% interval 12-42%), 9 July (34%; 25-39%) and 13 July L2 (34%; 0-42%). "
   "At the adopted calibration, no flight-level point estimate reaches an even fossil-biogenic split. We therefore describe the campaign as biogenic-dominated both in the median and across all flight-level estimates, "
   "while noting that the uncertainty intervals of the three low-correlation flights include an even or fossil-dominated mixture. "
   "Our campaign-median fossil fraction of 21% falls within the range of previous airborne urban estimates. "
   "Floerchinger et al. reported natural-gas contributions of 20 to 80% in summer and 32 to 100% in winter across seven U.S. urban areas,{{h0}} and studies of Los Angeles, Baltimore-Washington, New York and East Coast cities reported 40 to 88%.{{h1}} "
   "Denver therefore lies toward the lower end of published estimates, although differences in season, sampling footprint, and assumed ethane-to-methane endmembers limit direct comparisons.")
RW(M, "The lowest fossil fractions occur on 8 and 10 July",
   "The lowest fossil fractions occur on 3 July L2 (0%), under strong north-westerly flow from the mountains, and on 5 and 10 July L1 (8% each), when the urban ethane-methane slopes are nearly flat (10 July L1 in Figure 3). "
   "On 10 July, the transect structure makes the source contrast especially clear. The longest urban transect, a 59 km north-west-to-south-east pass at about 350 m above ground, contributes 297 of the flight's 564 gated samples and shows essentially no ethane relationship (slope = 0.003, r = 0.05). "
   "It passed downwind of the DADS landfill: its 97 gated samples within 25° of the landfill's downwind line, a mean 5.9 km from the landfill with winds from about 326° at 2.5 m/s and methane enhancements of up to 75 ppb, show no ethane relationship (slope = -0.0008, r = -0.04; section S8, Figure S12). "
   "This is the clearest aircraft encounter with an independently mapped point source. Carbon Mapper detected plumes from the DADS landfill nine times on six dates, and the encounter provides campaign-specific support for an effectively ethane-free biogenic endmember.")
RW(M, "By contrast, two legs near 430 m over the Commerce City industrial corridor",
   "By contrast, a southbound transect at about 450 m above ground along 105.0°W through central Denver has higher ethane and a modest positive slope (0.062, r = 0.39), which alone would correspond to about 60% fossil (section S3). "
   "The flight-level transect-block bootstrap interval of 3-60% captures this between-transect variability. "
   "On 8 July, none of the four contributing transects shows a meaningful ethane relationship (r = -0.35 to 0.23), so its 17% point estimate carries an interval of 0 to 100%.")
RW(M, "Figure 1: AMMBEC flight tracks (red lines)",                                                                     # comments 28, 29, 30
   "**Figure 1:** AMMBEC flight tracks (thin red lines; all 22 flights) and constant-altitude transect segments ({{c28}}circles{{/c28}}: each circle marks the mean position of one transect, or of the part of a transect inside one region, "
   "and is colored by that segment's mean CH4 enhancement above the 5th percentile of the flight's transect methane; all 570 segments of the 398 transects are shown) over the Denver Front Range, "
   "with the Denver-metro analysis box (green; 39.50 to 39.95°N, 105.20 to 104.55°W, about 50 by 56 km, roughly 2,800 km2) and the DJB basin {{c29}}boundary{{/c29}} at 40.05°N (dashed line; samples north of it are treated as basin). "
   "The seven-county metropolitan footprint used for the EPA-NEI sensitivity anchor, about 11,700 km2, is roughly four times the area of the analysis box, shown for scale as a locator inset, so the county-scale inventory domain is far larger than the sampled region. "
   "The gray background is Vulcan v4.0 fossil-fuel CO2 (2022),{{h0}} which traces the urban road network and built-up area. Major cities and the direction of the Wattenberg/DJB gas field are marked. "
   "{{c30}}The first 13 July flight, which shows the largest enhancements of the campaign, is traced as a dark-blue line with a white outline{{/c30}}.")
RW(M, "Figure 2: Fossil fraction of urban methane by flight",
   "**Figure 2:** Fossil fraction of urban methane by flight (points), ordered low to high and colored from biogenic to fossil, with 95% transect-block bootstrap intervals. "
   "The median across the campaign is 21%; no flight-level point estimate reaches an even split at this calibration, and the highest, the first 13 July flight, is 35%. "
   "The intervals of 5, 8 and 10 July L1 extend to or above 50% because their transects disagree (section S3); the 3 July L1 interval (23 to 26%) is mostly hidden by its marker.")
RW(M, "Figure 3: Urban ethane versus methane enhancement",
   "**Figure 3:** Urban ethane versus methane enhancement for a biogenic-dominated flight (10 July L1, within-transect slope 0.008) and the most fossil-influenced flight (13 July L1, slope 0.036). "
   "The points in the left panel come from three transects with different ethane levels, whose individual slopes are examined in section S3. "
   "We note that both slopes are atmospheric enhancement ratios from individual flights, not measurements of fossil source-gas composition, and neither is used to define βsource. "
   "Under the assumed mixing framework, co-sampled biogenic methane dilutes the atmospheric ethane:CH4 ratio relative to the fossil-source endmember. "
   "We therefore do not treat even the largest observed flight slope as an estimate of βsource; doing so would, by construction, assign that flight a fossil fraction of 100%.")
RW(M, "Per-flight urban leg count, relative fossil fraction",                                                            # comment 31
   "{{c31}}**Table 1:**{{/c31}} Per-flight number of urban transects (constant-altitude transects crossing the analysis box), relative fossil fraction (within-transect York, with 95% transect-block bootstrap interval), "
   "and the two enhancement-ratio emission estimates with their bootstrap intervals, for the nine flights with an estimable urban ethane-methane slope. "
   "Not usable marks a ratio that fails the reliability screen (correlation, at least two contributing transects, adequate anchor-enhancement range, and leave-one-transect-out stability); the final column lists which anchors passed. "
   "Flight times, transect heights, winds and mixing heights for every flight with at least three urban transects are listed in Table S1.")
NA_FL = [r["Flight"] for r in T1 if r["Fossil_pct_CI"] == "NA"]
assert len(NA_FL) == 7
assert NA_FL == ["20240702_R0_L2", "20240704_R0_L1", "20240704_R0_L2", "20240705_R0_L2", "20240708_R0_L2", "20240710_R0_L2", "20240712_R0_L2"], NA_FL
RW(M, "n/a: on 5 July (L1) none of the 5 urban legs",
   "Seven further flights crossed the box on at least three transects but had too few samples above the 20 ppb methane gate for a within-transect slope "
   "(2 July L2, 4 July L1 and L2, 5 July L2, 8 July L2, 10 July L2 and 12 July L2); they are listed in Tables S1 and S6.")

# ---- 4.2 ----
RW(M, "We anchored the ordinary-least-squares emission slopes (section 3.4) with two inventories",
   "We anchored the ordinary-least-squares emission slopes (section 3.4) with two inventories, defined over the same Denver analysis box. "
   "Scaling the CH4:CO slope by gridded GRA2PES CO (121.5 Gg CO yr−1) yields 4.4 to 10.3 t CH4/h (median 6.5) across the four flights that pass the reliability screen (section 3.4). "
   f"No flight passes the corresponding CH4:CO2 screen: the CH4:CO2 correlations of the four CO-passing flights are {min(rco2):.2f} to {max(rco2):.2f}, below the required 0.7, consistent with summertime biogenic CO2 weakening the relationship (section 3.4, Table S6). "
   "The Vulcan CO2 anchor therefore provides no independent check on the CH4:CO range in this campaign.")
RW(M, "Taken together, the four CH4:CO estimates and the single CH4:CO2 estimate",
   "Taken together, the four CH4:CO estimates indicate urban methane emission rates of roughly 4 to 10 t CH4/h, with a median of 6.5 t/h. "
   "We reiterate that these absolute rates are substantially more uncertain than the source apportionment because they scale linearly with the inventory anchor, the inventories date from 2020 to 2023 rather than 2024, "
   "and the bootstrap intervals in Table 1 capture only transect-to-transect scatter, not anchor uncertainty (section 5). "
   "Using the EPA NEI seven-county CO total instead (292.7 Gg CO yr−1) raises the CH4:CO-based estimates to 10.7 to 24.7 t CH4/h. "
   "We treat these as an upper bound sensitivity case because the county inventory covers a substantially larger area than the aircraft analysis domain and gives more CO than GRA2PES over the same counties (section 5). "
   "We do not report a single campaign-mean flux because enhancement ratios are estimated separately for each flight and vary meaningfully from day to day.")
RW(M, "The inferred fossil fraction also varies substantially across the campaign (2 to 47%)",
   "The inferred fossil fraction also varies substantially across the campaign (0 to 35%), with some qualitative correspondence to meteorology. "
   "The fraction of in-box boundary-layer samples (below 1.5 km above ground) with wind from the north-east (22.5 to 90°, north-north-east to east), computed from the aircraft winds, "
   "is weakly and not significantly associated with the fossil fraction (Pearson r = 0.46, p = 0.22, n = 9), so with this few flights the relationship is exploratory (section S9, Figure S13).")
RW(M, "The highest-fossil flight, 13 July flight L2 (47%)",
   "The two 13 July flights, among the three most fossil-influenced (35% and 34%), were sampled under persistent north-easterly flow, with 77% and 68% of in-box samples arriving from the north-east, and 9 July (34%) occurred under northerly flow. "
   "These wind directions are consistent with transport from the ethane-rich Wattenberg/DJB field north-east of Denver, though the Suncor refinery and a natural-gas power plant lie along the same fetch and may also contribute. "
   "The first 13 July flight also contained the largest methane enhancements of the campaign (section S10, Figure S16).")
RW(M, "The clearest wind-linked low-fossil case, 3 July L2 (20%)",
   "The clearest wind-linked low-fossil case, 3 July L2 (0%), occurred under strong north-westerly flow from the mountains (10.4 m/s from 299°), which limits transport of basin emissions into the urban domain. "
   "The relationship is not strictly monotonic. On 5 and 10 July L1, 70% and 50% of samples came from the north-east, more than on 9 July (39%), yet fossil fractions were 8%. "
   "Wind direction may therefore contribute to day-to-day variability but is clearly not the sole control. "
   "At the transect level, the relationship disappears: ethane slopes are uncorrelated with the angle between the upwind direction and the basin's oil-and-gas centroid (Spearman ρ = -0.23, p = 0.17, n = 36 transects) or an inventory-based upwind-exposure score (section S9, Figure S14). "
   "Transect slopes cluster around 0.014 to 0.053 mol mol−1 (interquartile range) across fetch directions and therefore do not resolve a basin-advection gradient. "
   "Replacing the wind cone with HRRR-driven STILT footprints convolved with the gridded GHGI, computed for the 25 urban level legs of the Vertical_Speed sensitivity definition (section S3), gives the same result (ρ = 0.02 between the leg slope and the modeled basin share; section S9, Figure S15).")
RW(M, "Because βsource enters only as a divisor, changing the endmember rescales",
   "Because βsource enters only as a divisor, changing the endmember rescales flight-level fossil fractions without changing their ranking. "
   "Thus, the three flights that exceed 50% under the assumed lean basin ratio of 0.061 (9 July and both 13 July flights; none does at 0.0813) are already the most fossil-influenced flights at the adopted calibration and were sampled under northerly or north-easterly flow. "
   "Source proximity within the metropolitan area, boundary-layer depth, and time of day likely also contribute to the observed variability. "
   "This day-to-day variability underscores why single-day measurements or inventory-only characterizations may not capture the full urban methane source mix.")

# ---- 4.3 ----
R(M, "This gives approximately 2.0 t CH4/h from fossil sources and 5.9 t CH4/h from biogenic sources- a combined 7.9 t/h, of which about 26% is fossil (Figure 4A). Because the two medians are calculated separately, their sum differs from the 7.6 t/h median total emission estimate.",
     "This gives approximately 1.9 t CH4/h from fossil sources and 4.6 t CH4/h from biogenic sources- a combined 6.5 t/h, of which about 29% is fossil (Figure 4A). Because the two medians are calculated separately, their sum need not equal the median total emission estimate; with four flights it does here (6.5 t/h), because the same two flights are central in all three rankings.")
R(M, "approximately 2.4 t CH4/h to municipal solid-waste landfills, 2.1 t CH4/h to domestic wastewater, and 0.9 t CH4/h to natural-gas distribution",
     "approximately 1.9 t CH4/h to municipal solid-waste landfills, 1.6 t CH4/h to domestic wastewater, and 0.8 t CH4/h to natural-gas distribution")
R(M, "The inferred biogenic total is approximately six times the inventory estimate", "The inferred biogenic total is approximately 4.5 times the inventory estimate")
R(M, "predicts a median leg-level ethane-to-methane slope of 0.041 mol mol-1, compared with 0.034 observed (section S9).",
     "predicts a median leg-level ethane-to-methane slope of 0.041 mol mol-1, compared with 0.034 observed, for the 25 urban level legs of the Vertical_Speed sensitivity definition (section S9).")
RW(M, "The three waste-source cells contain 12.0 of v2.0beta",
   "The three waste-source cells contain 12.0 of v2.0beta's 12.2 t CH4/h of box-wide waste emissions. "
   "We identified 15 aircraft crossings on seven flights within 12 km and 25° of the downwind direction from two of these cells (section S11, Figure S17, Table S10). "
   "The three crossings downwind of the DADS landfill (two flights) give a median biogenic methane rate of 0.11 t CH4/h- about 2% of the inventory's waste emissions in the upwind area, dominated by the landfill cell's 6.07 t CH4/h. "
   "Twelve crossings downwind of the waste cell immediately north of the Suncor/Metro Water Recovery complex (seven flights) give a median biogenic rate of 0.85 t CH4/h, a median 2.1 times the v2.0beta waste emissions in their upwind fetch, "
   "and total methane rates across these crossings range from 0.76 to 7.73 t CH4/h. "
   "These estimates scale directly with the lidar-derived mixing height and assume uniform vertical mixing, so they provide approximate constraints rather than reliable facility emission rates.")
R(M, "It assigns approximately 2.1 t CH4/h to metropolitan domestic wastewater.", "It assigns approximately 1.6 t CH4/h to metropolitan domestic wastewater.")
R(M, "the allocated top-down fossil share is approximately 26%", "the allocated top-down fossil share is approximately 29%")

# ---- 5. Discussion ----
RW(M, "Across the regional reference endmembers, from the DJB values of 0.081-0.10",
   "Across the regional reference endmembers, from the DJB values of 0.081-0.10 to the measured composition of gas delivered to the city (0.110 mol mol−1; sections 3.3, S5), the campaign-median fossil fraction ranges from 19 to 26%. "
   "At the adopted calibration of 0.102 mol mol−1, the median is 21%, with individual flights ranging from 0 to 35%. "
   "At 0.0813 the most fossil-influenced flight, 13 July L1, reaches 44%, so no flight reaches 50% at the adopted calibration, the production-area ratio or the delivered-gas composition. "
   "The campaign median exceeds 50% only for an effective endmember below 0.042 mol mol−1 (Figures S4-S5); an assumed lean basin ratio of 0.061 mol mol−1 gives 35%, with three flights above 50%. "
   "These reference values do not bound Denver's effective fossil endmember from below, so the absolute fraction remains conditional on it.")
R(M, "(~4 to 11 t/hr)", "(~4 to 10 t/hr)")
R(M, "(29% at the adopted 0.102 calibration, 27 to 36% across the regional reference endmembers)", "(21% at the adopted 0.102 calibration, 19 to 26% across the regional reference endmembers)")
R(M, "(44 to 59% across these endmembers)", "(33 to 44% across these endmembers)")
R(M, "whereas the aircraft enhancement-ratio estimates are approximately 4-11 t/h.", "whereas the aircraft enhancement-ratio estimates are approximately 4-10 t/h.")
R(M, "by factors of roughly 1.5-6 across flights and anchors and by about 2.7-4.5 at the CH4:CO median of 7.6 t/h",
     "by factors of roughly 1.6-6 across flights and by about 2.3-3.8 at the CH4:CO median of 6.5 t/h")
R(M, "by a factor of approximately 1.5-3.6.", "by a factor of approximately 1.6-3.7.")
R(M, "compared with an airborne biogenic fraction of 64-73% across the regional reference endmembers.", "compared with an airborne biogenic fraction of 74-81% across the regional reference endmembers.")
R(M, "The aircraft-derived biogenic total of approximately 5.9 t/h (section 4.3) is about half the v2.0beta waste total, and the crossing downwind of the DADS landfill",
     "The aircraft-derived biogenic total of approximately 4.6 t/h (section 4.3) is a little over one-third of the v2.0beta waste total, and the crossings downwind of the DADS landfill")
R(M, "above the 27-36% campaign median inferred from airborne ethane across the regional reference endmembers and approximately twice the 29% median at the adopted calibration",
     "above the 19-26% campaign median inferred from airborne ethane across the regional reference endmembers and approximately three times the 21% median at the adopted calibration")
RW(M, "The three anchor comparisons illustrate this point.",
   "The anchor comparisons illustrate this point. Scaling CH4:CO by the box-consistent GRA2PES CO total (121.5 Gg CO yr−1) yields 4.4-10.3 t CH4/h. "
   "No flight passes the CH4:CO2 reliability screen in this campaign, because summertime biogenic CO2 weakens the CH4:CO2 correlation (section 4.2), so the box-consistent Vulcan anchor cannot provide an independent tracer check here. "
   "Scaling the same CH4:CO slopes by the EPA NEI seven-county CO total (292.7 Gg CO yr−1), however, raises the estimates to 10.7-24.7 t CH4/h, approximately 2.4 times higher.")
R(M, "still gives 8.7 to 20.1 t CH4/h", "still gives 8.3 to 19.3 t CH4/h")
R(M, "raises the campaign median to 36% fossil and brings two of seven flights to or above 50%.",
     "raises the campaign median to 26% fossil, with no flight at or above 50% (the highest, 13 July L1, reaches 44%).")
R(M, "which lowers the median to 27%.", "which lowers the median to 19%.")
R(M, "roughly 36-66% of urban fossil methane for basin ratios of 0.0813 and 0.102 (6-10% under an assumed lean basin ratio of 0.061)",
     "roughly 62-91% of urban fossil methane for basin ratios of 0.0813 and 0.102 (44-77% under an assumed lean basin ratio of 0.061)")
R(M, "for legs with tightly correlated ethane and methane", "for transects with tightly correlated ethane and methane")
R(M, "and the steepest such legs yield 0.034-0.054 mol mol-1 across four flights (section S3).",
     "and the steepest such transects yield 0.024-0.044 mol mol-1 across four flights, below every reference value, so this constraint does not exclude a leaner endmember (section S3).")
R(M, "and values below 0.059 mol mol−1 would yield one.", "and values below 0.042 mol mol−1 would yield one.")
R(M, "None of the eight urban flights encircles the Denver metropolitan area;", "None of the 16 flights that crossed the box on at least three transects encircles the Denver metropolitan area;")
R(M, "although the leg-level ethane:methane slope does not vary with this modeled exposure (section S9).",
     "although the ethane:methane slope of the 25 urban level legs used in that calculation (the Vertical_Speed sensitivity definition of section S3) does not vary with this modeled exposure (section S9).")
R(M, "through leg-block resampling", "through transect-block resampling")
R(M, " so all source fractions reported here are summertime values.",                                                   # comment 32
     " so all source fractions reported here are summertime values; they rest on nine flights on seven days, with 58 urban transects and about 6.7 h of transect flight inside the analysis box (section 4.1).")
R(M, "Enhancement-ratio estimates anchored to two independent inventories place urban methane emissions at approximately 4-11 t CH4/h,",
     "Enhancement-ratio estimates anchored to a gridded CO inventory place urban methane emissions at approximately 4-10 t CH4/h,")

# ---- Table 1 ----
T1rows = []
for r in T1:
    if r["Fossil_pct_CI"] == "NA": continue
    T1rows.append([lab(r["Flight"]), str(r["Urban_legs"]), ci(r["Fossil_pct_CI"]), ci(r["CH4CO_GRA2PES_t_hr"]), ci(r["CH4CO2_Vulcan_t_hr"]), r["Usable"]])
TAB(M, 0, T1rows, header=["Flight", "Urban transects", None, None, None, None])

# ================================================================================================
# SUPPORTING INFORMATION
# ================================================================================================
S = "si"
# ---- S1 ----
R(S, "whose vertical-velocity variance exceeds", "whose noise-corrected vertical-velocity variance (the variable wVar of the lidar's velStats files, on 20 min profiles with 50 m range gates starting 102 m above the lidar) exceeds")  # Sunil comment 1
R(S, "across the eight urban flights considered here. The wider set of flights screened in section S1.2 includes",
     "across the nine flights with estimable urban slopes. The wider set of flights in Table S1 and section S1.2 includes")
RW(S, "The urban emission rate instead comes from the two enhancement-ratio methods",
   "The urban emission rate instead comes from the enhancement-ratio method (section S6), which needs no closed loop and no explicit wind or mixing-height term. "
   "Table S1 records, for each of the 16 flights that crossed the box on at least three constant-altitude transects, the flight and in-box time windows, the number of urban transects and their heights above ground, "
   "the mean in-box wind below and above 1.5 km, the number of altitude levels, the mixing height and whether the flown transects encircle the metropolitan box. "
   "The flight-coincident Doppler-lidar mixing heights are listed only as documentation. A downwind curtain, which needs no closed loop, is assessed separately in section S1.2.")
RW(S, "Loop-geometry diagnostic for each urban flight",
   "**Table S1:** Urban sampling and loop-geometry diagnostic for each flight that crossed the Denver-metro box on at least three constant-altitude transects (qualitative; no flux is computed). "
   "UTC gives the flight file's time window and the window of its in-box transect samples. Urban transects counts the transects crossing the box and in-box time their seconds inside it; heights give the range of the transects' mean height above ground. "
   "Winds are the aircraft's in-box measurements, scalar-mean speed and vector-mean direction (from), below and above 1.5 km above ground. "
   "BLH is the lidar mixing height (median over the flight window). Levels counts the distinct altitude levels flown in the box. "
   "No flight encircles the box: each covers only part of the perimeter around the city, or samples a single altitude level (5 July L2), because the campaign geometry was optimized to encircle the DJB basin. "
   "Fossil fractions for the nine flights with an estimable slope are in Table 1.")
RW(S, "Figure S1: Loop geometry.",
   "**Figure S1:** Loop geometry. For each of the 16 flights that crossed the box on at least three transects, the flown urban transects (dark blue) are drawn in map coordinates against the Denver-metropolitan analysis box (green), with a red dot marking the first urban-transect sample. "
   "A closed-loop box balance would require the path to wrap the box; instead, every AMMBEC flight covers only part of the perimeter around the city, consistent with a design optimized to encircle the DJB basin to the north-east. "
   "No closure percentage is shown, because with this geometry, such a number would carry more meaning than the flight design supports.")
R(S, "we grouped level legs near Denver into candidate screens consisting of at least two approximately collinear legs",
     "we grouped constant-altitude transects near Denver into candidate screens consisting of at least two approximately collinear transects")
RW(S, "The search identified 24 candidate screens",
   f"The search identified 48 candidate screens across 20 flights, but none met all four criteria (Table S2). {cnt['city_upwind']} placed the entire city upwind, {cnt['width_ok']} spanned at least 80% of its width, "
   f"two provided adequate vertical coverage, and {cnt['wind_ok']} had adequate and sufficiently steady transport; no screen passed more than three criteria.")
RW(S, "The two strongest candidates illustrate the limiting conditions.",
   "The strongest candidates illustrate the limiting conditions. The only screen meeting three conditions, a north-south wall about 13 km west of the box center on the second 13 July flight, stacked 11 transects on three levels (231 to 2461 m above ground, above the 1752 m mixing height), "
   "spanned 1.18 times the city width and carried 3.1 m/s of through-screen flow with a constancy of 0.86; but it bisects the metropolitan area, so the city is not entirely upwind of it. "
   "Screens fully beyond the city met at most two conditions: on 9 July, an east-west wall 35 km south of the box center had the city upwind and steady transport (4.6 m/s, constancy 0.94), but a single altitude level (272 to 421 m above ground, far below the 3102 m mixing height) spanning 41% of the city width, "
   "and on the second 13 July flight a similar wall 28 km south had one level at 232 to 297 m.")
RW(S, "The most vertically complete screens were flown over the metropolitan area itself.",
   "The most vertically complete screens were flown over the metropolitan area itself. For example, a 3 July (L1) screen 9 km west of the box center stacked 13 transects on five altitude levels (198 to 2391 m above ground) and spanned approximately 1.3 times the city width, "
   "and screens on 5, 9 and 10 July and both 13 July flights stacked three or four levels across the city. Because these screens bisected the metropolitan area, however, they intercepted only an unknown fraction of urban emissions. "
   "Conversely, screens located fully beyond the city, 24 to 42 km away, failed on vertical coverage (one or two levels, or a stack below the mixing height) and often also on width or transport. "
   "Thus, Denver was sampled densely in the vertical where a screen could not close the urban budget and sparsely where it potentially could.")
R(S, "A screen is a wall of two or more level legs sharing an axis", "A screen is a wall of two or more constant-altitude transects sharing an axis")
R(S, "None of the 24 screens meets all four conditions", "None of the 48 screens meets all four conditions")
R(S, "; that lidar has no data on 29 June",                                                                              # Sunil comment 1
     " (the highest 50 m range gate, contiguous from the surface, at which the noise-corrected vertical-velocity variance, the variable wVar of the lidar's velStats files, is at least 0.1 m2 s−2; median of the 20 min profiles within the flight's UTC window; section S1.1); that lidar has no data on 29 June")

# ---- S2 ----
INS(S, "This is a trend in a route-specific enhancement percentile",                                                     # comment 14
    "Instrument and quality control. The surveys used a Picarro G2204 cavity ring-down analyzer (CH4 and H2S) sampling through a 3 m inlet on CDPHE's mobile laboratory, with differential GPS. "
    "Before analysis, each reading was attributed to the position where its air entered the inlet by shifting its time stamp back by the inlet and analyzer delay that CDPHE measured for the vehicle (21 s, which at survey speeds of 25 to 40 km/h is 150 to 230 m of road); "
    "the delivered 1 s series, in which each roughly 5 s acquisition is repeated until the next, was averaged over each acquisition block so that repeats are not counted as independent measurements; "
    "and samples within 100 m of the vehicle depot, where deployments begin indoors, were removed. "
    "[Authors to complete with CDPHE: calibration standards and frequency for the G2204.]")
R(S, "compared with 0.029 mol mol-1 (0.002 to 0.048) for the aircraft", "compared with 0.021 mol mol-1 (-0.002 to 0.036) for the aircraft")
R(S, "well above the 0.036 mol mol−1 basin ratio measured from the aircraft", "well above the 0.032 mol mol−1 basin ratio measured from the aircraft")

# ---- S3 ----
RW(S, "For the AMMBEC urban flights, these filters are mild",
   "For the AMMBEC urban flights, these filters are mild for most flights: all nine analysis flights were flown during the day (local sampling windows within 09:36 to 18:00), and only 4 to 12% of all in-box samples (fewer on the urban transects used in the fits) fall below 200 m AGL. "
   "Six of the nine flights change by at most 1 percentage point (Table S3). Three change more: 8 July L1 falls from 17 to 0%, and the two 13 July flights move from 35 to 27% and from 34 to 43%; "
   "on 13 July L1 the in-boundary-layer filter removes most samples because the lidar mixing height that day was only 0.35 km, and on 13 July L2 it removes the transects above the 1.75 km mixing height. "
   "The campaign median is unchanged at 21% and no flight exceeds 50%, so the biogenic-dominated source-mix conclusion is robust to data selection, although individual flights are not.")
R(S, "use the urban-leg measurements without these additional filters", "use the urban-transect measurements without these additional filters")
R(S, "no flight’s estimate changed by more than 3 percentage points (Table S3).", "the campaign median did not change, although three flights changed by 8 to 17 percentage points (Table S3).")
R(S, "Per-flight fossil fraction from all urban-leg samples", "Per-flight fossil fraction from all urban-transect samples")
R(S, "The small differences confirm that the fossil fraction is robust to these data-selection filters.",
     "The campaign median is unchanged (21%); three flights change by 8 to 17 percentage points.")
R(S, "everywhere above βsource = 0.059 mol mol−1", "everywhere above βsource = 0.042 mol mol−1")
R(S, "reaches zero for βsource above about 0.096 mol mol−1", "reaches zero for βsource above about 0.072 mol mol−1")
R(S, "the campaign median reaches 50% only at βsource = 0.059 mol mol−1;", "the campaign median reaches 50% only at βsource = 0.042 mol mol−1;")
R(S, "every value in that comparison lies above the 0.059 mol mol−1 break-even", "every value in that comparison lies above the 0.042 mol mol−1 break-even")
R(S, "only the classification of the one or two most fossil-influenced flights moves with the choice.",
     "no individual flight reaches 50% at any of these values (the highest, 13 July L1, reaches 44% at 0.0813).")
R(S, "(median 48%, with two flights above 50%)", "(median 35%, with three flights above 50%)")
R(S, "With the measured Denver composition the campaign median stays between 27 and 36% for every mixture with the basin ratios of 0.0813 and 0.102 (inventory-weighted mixture, p = 0.67, basin ratio 0.0813: 29%, no flight above the halfway line; distribution gas alone: 27%) and between 27 and 48% with the assumed 0.061 (31% at the inventory weighting, with the second 13 July flight at 51%); it never reaches 50%.",
     "With the measured Denver composition the campaign median stays between 19 and 26% for every mixture with the basin ratios of 0.0813 and 0.102 (inventory-weighted mixture, p = 0.67, basin ratio 0.0813: 21%, no flight above the halfway line; distribution gas alone: 19%) and between 19 and 35% with the assumed 0.061 (22% at the inventory weighting, no flight above the halfway line); it never reaches 50%.")
R(S, "once p exceeded 0.06 to 0.10 (assumed basin ratio 0.061), 0.36 to 0.51 (0.0813) or 0.52 to 0.66 (0.102), reaching 66 to 90% at the inventory weighting",
     "once p exceeded 0.44 to 0.77 (assumed basin ratio 0.061), 0.62 to 0.88 (0.0813) or 0.72 to 0.91 (0.102), reaching 47 to 65% at the inventory weighting")
R(S, "The median (within-leg slopes) is shown", "The median (within-transect slopes) is shown")
RW(S, "Under the two-component, fixed-composition mixing model used in Equation 3, the tightly correlated legs",
   "Under the two-component, fixed-composition mixing model used in Equation 3, the tightly correlated transects provide lower-bound-type constraints on the effective fossil signature. "
   "In that model, in a transect whose ethane and methane enhancements are tightly correlated, the slope equals the effective endmember multiplied by the fossil share of that transect's methane, so the effective endmember of the air sampled cannot be smaller than the slope; "
   "a high correlation does not by itself establish a fixed composition within the transect, since several fossil components with different ethane ratios varying together would give a covariance-weighted slope instead. "
   "Taking transects with r of at least 0.7 and at least 30 gated samples, the steepest transect on each of four flights gives 0.024 (3 July L1), 0.039 (9 July), 0.044 (13 July L1) and 0.043 (13 July L2). "
   "The steepest slopes of the campaign come from weakly correlated transects with few gated samples, 0.61 mol mol−1 on a 28-sample transect at 2.35 km on 13 July L2 (r = 0.29) and 1.17 on a 25-sample transect on 5 July L1 (r = 0.24); "
   "they fail the correlation criterion, carry little weight in the within-transect fits, and are not evidence of ethane-rich air (section S9).")
RW(S, "For the assumed lean basin ratio of 0.061, the strongest leg-level constraint",
   "Even the strongest of these floors (0.044 on 13 July L1 and 0.043 on 13 July L2) lies below every endmember considered. For the assumed lean basin ratio of 0.061 and East Coast distribution gas it would still allow 41 to 72% of the fossil methane on that transect to be distribution gas, "
   "and 60 to 85% for a basin ratio of 0.0813; the weaker floors on 3 July L1 and 9 July allow more. The transect-level floors therefore do not exclude an ethane-poor fossil source, and the Denver composition is unconstrained by them. "
   "The ratio emitted to the atmosphere can differ from the ratio supplied where combustion slip or process emissions dominate over whole-gas leaks,{{h0}} which is why atmospheric constraints of this kind complement the supply composition, even though here they are not restrictive.")
RW(S, "As a separate check, we examined the individual legs from the 10 July L1 flight",
   "As a separate check, we examined the individual transects of the 10 July L1 flight. Three urban transects contribute to its within-transect fit. "
   "The longest, a 59 km north-west-to-south-east pass at about 350 m above ground, contributes 297 of the 564 gated samples and shows essentially no ethane-methane relationship (slope = 0.003, r = 0.05). "
   "A second north-west-to-south-east pass at about 270 m (73 samples) gives 0.016 (r = 0.20), and a southbound transect along 105.0°W through central Denver at about 450 m (194 samples) has higher ethane and a slope of 0.062 (r = 0.39), "
   "corresponding to fossil fractions of 3%, 16% and 60% when fit separately.")
RW(S, "The 235 m leg is a single plume whose 93 ppb",
   "The longest transect passed downwind of the DADS landfill: its 97 gated samples within 25° of the landfill's downwind line lie a mean 5.9 km from the landfill, with winds from about 326° at 2.5 m/s and methane enhancements of up to 75 ppb, "
   "and show no ethane relationship (slope = -0.0008, r = -0.04; section S8), consistent with landfill methane carrying no ethane.")
RW(S, "The whole-flight leg-block bootstrap interval of 0 to 34%",
   "The whole-flight transect-block bootstrap interval of 3 to 60% captures this variation because the bootstrap resamples entire transects: the low point estimate is dominated by the extensively sampled north-west-to-south-east transect, whereas the upper bound reflects the more fossil-influenced southbound transect. "
   "Transects that disagree also explain the wide intervals of 5 and 8 July. On the first 8 July flight, none of the four contributing transects shows a meaningful ethane-methane relationship (r = -0.35 to 0.23), and on 5 July L1 the three contributing transects hold only 102 gated samples between them, "
   "whereas on the second 13 July flight the two best-sampled transects (215 and 141 samples) show positive relationships (r = 0.37 and 0.80).")
RW(S, "Finally, regression choice affects the ethane slope and the CH4:CO emission slope differently",
   "Finally, regression choice affects the ethane slope and the CH4:CO emission slope differently when correlation is weak. Ordinary least squares (OLS), reduced major axis (RMA), and instrument-weighted York fits- both pooled across transects and with the per-transect intercepts used in the main analysis- are compared by flight in Figure S6.{{h0}} "
   "For the ethane-based fossil fraction, OLS and pooled York agree within 2 percentage points on eight of the nine flights (8 July L1: 59 against 80%). "
   "The within-transect York fit reduces slopes on flights where the pooled relationship is driven by between-transect differences, such as 13 July L1 (43 to 35%) and 8 July L1 (80 to 17%), and raises slopes on flights where the pooled slope is negative or near zero, such as 10 July L1 (0 to 8%) and 5 July L1 (3 to 8%). "
   "The biogenic-dominated conclusion is therefore not sensitive to this choice. RMA, however, becomes increasingly steep as correlation weakens and inflates the apparent fossil fraction, so we do not adopt it.")
R(S, "within-leg York used in the main text", "within-transect York used in the main text")
R(S, "The main text uses the within-leg York fit for the ethane fossil fraction", "The main text uses the within-transect York fit for the ethane fossil fraction")
RW(S, "Two points from this comparison warrant emphasis.",
   "Two points from this comparison warrant emphasis. First, the per-transect-intercept, or within-transect, fit is our primary estimator because a flight's urban transects sample different air masses with different ethane backgrounds. "
   "A single regression pooled across transects combines the within-transect ethane–methane relationship with differences among the transect means, with the latter weighted by the between-transect variation in methane. "
   "When, for example, one transect samples a landfill plume and another samples the industrial corridor, this between-transect component can oppose the within-transect relationship and even reverse the sign of the pooled slope- a form of Simpson's paradox. "
   "The within-transect estimator removes this between-transect term and therefore requires no post hoc truncation at zero. "
   "This is why the inferred fossil fraction on 10 July L1 rises from 0% under the pooled fit, whose slope is negative, to 8% under the within-transect fit, and why on 8 July L1 the pooled fit's 80%, driven by differences among transect means (pooled r = 0.49), falls to 17%.")
INS(S, "Finally, we do not use the largest observed atmospheric ethane-to-methane slope",
    "Transect definition and the vertical-speed alternative. The analysis uses constant-altitude transects identified from GPS altitude and ground track (main text section 3.1). "
    "The data files also carry two vertical-rate variables, the avionics Vertical_Speed (labelled m/s) and the differential-GPS Heave (labelled m/min). Compared with the time derivative of GPS altitude, both run at about 5.5 to 6 times the rate (r ≈ 0.99 after 30 s smoothing). "
    "Vertical_Speed is reported in steps of 0.3048, which suggests units of 10 ft/min; on 3 July L1, for example, its time integral over the flight's main climb is about 8,900 m against a GPS altitude gain of 1,613 m. "
    "A vertical-speed threshold therefore does not correspond to its stated physical climb rate: a nominal 1 m/s threshold on Vertical_Speed is in effect about 0.17 m/s and splits continuous transects at small altitude corrections. "
    "As a sensitivity test we repeated the analysis with level legs defined by a 1 m/s threshold on the 20 s smoothed Vertical_Speed, a 30° heading tolerance, gaps of up to 10 s, at least 60 s and 2 km, and an altitude range of at most 150 m, with each leg assigned to a region by its mean position. "
    "That definition yields an estimable slope on 7 of the 8 flights with at least three urban legs and a campaign-median fossil fraction of 29% (2 to 47%) at 0.102 mol mol−1, against 21% (0 to 35%) on 9 flights with constant-altitude transects; "
    "both are biogenic-dominated, and neither has a flight-level point estimate at or above 50%. The per-flight transect maps, with the vertical-speed legs marked for comparison, are produced by script 88 of the repository (figures/transects).")

# ---- S4 ----
RW(S, "We compare the threshold with two measures of variability.",
   "We compare the threshold with two measures of variability. First, the methane instrument precision is 1 ppb- the value used in the York regression- so the threshold is twenty times larger. "
   "Second, we estimate background variability using the median absolute deviation of methane enhancements along each flight's urban transects, scaled to an equivalent standard deviation. This measure is less sensitive than the ordinary standard deviation to large plume enhancements.{{h0}} "
   "Across the 19 flights with urban transects, this variability ranges from 0.6 to 40.6 ppb, with a median of 3.0 ppb, so the 20 ppb threshold is about seven times the typical variability. "
   "Two flights are exceptions: 3 July L1 (10.4 ppb) and 13 July L1 (40.6 ppb), on which the enhancement is elevated along most of the in-box transects, so that the median absolute deviation reflects the urban plume field itself rather than background noise; "
   "on 13 July L1 the 20 ppb gate still keeps 1,211 samples with enhancements up to about 150 ppb.")
RW(S, "Raising the methane-enhancement threshold leaves fewer measurements",
   "Raising the methane-enhancement threshold leaves fewer measurements available for fitting, and some flights no longer have enough data to estimate a fossil fraction. The number of usable flights falls from 17 at 5 ppb to just one at 100 ppb. "
   "Changes in the campaign median could therefore reflect both the threshold and which flights remain. To separate these effects, we compare the same six flights that yield estimates at every threshold from 5 to 50 ppb.")
RW(S, "For these four flights (Figure S7), the median fossil fraction ranges",
   "For these six flights (Figure S7), the median fossil fraction ranges from 22 to 31%, including 22% at the adopted 20 ppb threshold; it stays at 22 to 24% from 5 to 20 ppb and at 28 to 31% from 25 to 50 ppb, the largest step (9 percentage points) falling between 20 and 25 ppb. "
   "No flight in the fixed panel exceeds 50% fossil at any tested threshold when using the adopted endmember of 0.102 mol mol-1. "
   "Over all evaluable flights, a majority-fossil estimate appears at some thresholds (three flights at 5 and 10 ppb, one at 15, 25 and 30 ppb), each on a flight that yields no estimate at the adopted 20 ppb gate or that retains three or fewer contributing transects at that threshold (8 July L1 at 25 ppb, 13 July L2 at 30 ppb). "
   "Thus, changing the threshold affects the estimated percentage but does not change the conclusion that the sampled methane is predominantly biogenic at this endmember. "
   "Endmember choice remains important: using the assumed lower value of 0.061 mol mol-1 at the adopted 20 ppb threshold raises three flights above 50% fossil (section S3).")
RW(S, "Changes in a flight’s estimated fossil fraction often coincide",
   "Changes in a flight's estimated fossil fraction often coincide with the loss of contributing transects as the threshold rises. Across the six-flight panel, the median number of contributing transects falls from 5.5 to 2. "
   "The first flight on 13 July retains seven contributing transects from 5 to 20 ppb, and its fossil fraction remains within 32-36% from 5 to 50 ppb. "
   "By contrast, the estimate for 10 July L1 falls from about 21% to 0% as the number of contributing transects drops from six to three between 5 and 25 ppb, and that for 3 July L1 rises from about 21% to 30-33% as its contributing transects drop from five to two or three above 20 ppb. "
   "These changes suggest that the threshold affects the result partly by changing which air masses are represented, rather than simply changing the fitted relationship within the same air.")
R(S, "Its median positive ethane-to-methane slope remains between 0.027 and 0.037 mol mol-1 across thresholds of 5-75 ppb. At the adopted 20 ppb threshold, this median is 0.031 mol mol-1; applying the additional reliability screen described in section S5 gives 0.036 mol mol-1. The median falls to 0.018 mol mol-1 only at the highest threshold, 100 ppb, when just five flights remain.",
     "Its median positive ethane-to-methane slope remains between 0.011 and 0.022 mol mol-1 across thresholds of 5-75 ppb. At the adopted 20 ppb threshold, this median is 0.021 mol mol-1; applying the additional reliability screen described in section S5 gives 0.032 mol mol-1. The median falls to 0.006 mol mol-1 only at the highest threshold, 100 ppb, when just four flights remain.")
RW(S, "The threshold sensitivity analysis has several limitations.",
   "The threshold sensitivity analysis has several limitations. Only six flights yield estimates at every threshold from 5 to 50 ppb. "
   "Although the adopted threshold is about seven times the median background variability, it is less than twice that variability on 3 July L1 and below it on 13 July L1 (see above). "
   "The 21% campaign median reported in the main text also depends on the adopted 20 ppb threshold and the flights with estimable slopes; the fixed panel gives 22 to 24% at thresholds up to 20 ppb and 28 to 31% above. "
   "These tests support a predominantly biogenic source mix at the adopted endmember, rather than establishing 21% as a precise estimate.")
RW(S, "The background calculation introduces another source of sensitivity.",
   "The background calculation introduces another source of sensitivity. In the primary analysis, we join each flight's urban-transect samples end to end and calculate a rolling 5th-percentile background over 300 samples; the basin analysis uses the basin transects in the same way. "
   "Near a transect boundary, the window therefore includes samples from neighboring transects, even if those transects were separated in time or space.")
RW(S, "As an alternative, we calculated the background along the continuous 1 Hz flight record",
   "As an alternative, we calculated the background along the continuous 1 Hz flight record using the same window, which spans approximately 20 km of flight. "
   "This approach removes more of the broad urban enhancement and reduces the number of samples exceeding the 20 ppb threshold on most flights- for example, from 273 to 109 on 8 July L1 and from 564 to 423 on 10 July L1- while increasing it on a few (539 to 592 on 9 July). "
   "The resulting fossil fractions are 29, 0, 8, 0, 32, 16, 22, 41 and 43% for 3 July L1, 3 July L2, 5 July L1, 8 July L1, 9 July, 10 July L1, 12 July L1, 13 July L1 and 13 July L2, respectively. "
   "The campaign median changes from 21% to 22%, and no flight exceeds 50%.")
RW(S, "The alternative background also weakens the CH4:CO correlations",
   "The alternative background leaves the CH4:CO correlations of the four flights used for the primary CO-based emission estimates at or above 0.7 (0.74, 0.74, 0.79 and 0.80 for 3 July L1 and L2, 9 July and 13 July L1), and their emission estimates become 4.7 to 9.4 t CH4/h. "
   "We retain the joined-transect background to preserve broader urban enhancements, while recognizing that background choice materially affects individual flights. "
   "The largest fossil-fraction changes occur on 8 July L1 (17 to 0%) and the two 13 July flights (35 to 41% and 34 to 43%), which also have among the widest uncertainty intervals in Table 1.")
R(S, "the median over the fixed panel of four flights evaluable at every gate", "the median over the fixed panel of six flights evaluable at every gate")
R(S, "Twelve of the 22 flights give a well-defined peak", "Thirteen of the 22 flights give a well-defined peak")
R(S, "(range -2 to +1 s). Restricting the windows to DJB basin legs, where the ethane signal is strongest, gives the same -1 s (eight flights).",
     "(range -1 to 0 s). Restricting the windows to DJB basin transects, where the ethane signal is strongest, gives the same -1 s (15 flights).")
RW(S, "We then shifted the ethane record by each integer lag",
   "We then shifted the ethane record by each integer lag from -5 to +5 s, before transect selection, background estimation and gating, and repeated the within-transect York fit of section 3.3 unchanged; at zero shift the procedure reproduces Table 1 on all nine flights. "
   "The campaign median is 21% at 0 s, 22% at the estimated -1 s and 20% at -2 s. Per flight, the correction changes the fossil fraction by 4 percentage points on 8 July L1 (17 to 13%) and by at most 1.3 points on the other eight flights. "
   "Shifts beyond the estimated lag attenuate the slope, as expected when the two records are misaligned: the median is 23% at +1 s, falls to 18% at +2 s and to 12 to 13% at ±5 s, and no flight exceeds 50% fossil at any shift. "
   "The median basin ratio over the 18 basin flights with a positive slope (no correlation or transect-count screen; the screened median of section S5 is 0.032 at zero shift) holds at 0.019 to 0.021 mol mol−1 from -3 to +1 s and falls to 0.012 to 0.018 beyond. "
   "The uncorrected alignment therefore lies within 2 percentage points of the best-aligned result (21 against 22%), and the adopted analysis is left uncorrected (Figure S8).")
RW(S, "The York fit assigns ethane a 1σ precision of 0.2 ppb",
   "The York fit assigns ethane a 1σ precision of 0.2 ppb, the value in the data files. The in-flight 1 Hz scatter, estimated per flight as the median absolute deviation of the 61 s high-passed ethane record at quiet transect samples (|high-passed ΔCH4| < 5 ppb), is 0.19 ppb (0.16 to 0.24 ppb across 22 flights); "
   "because it includes sub-minute atmospheric variability, it is an upper bound on instrument noise. Refitting with each flight's empirical value gives a campaign median of 24% (largest per-flight change 7 points, on 8 July L1, whose transects are weakly correlated), and a stress value of 0.5 ppb gives 20%.")
R(S, "over the 11 basin flights with a positive slope", "over the 18 basin flights with a positive slope")
R(S, "it is flat from -3 to +2 s and falls beyond", "it is flat from -3 to +1 s and falls beyond")
R(S, "The fossil fraction peaks at the estimated offset.", "The campaign median is highest within 1 s of zero (21 to 23%).")

# ---- S5 ----
R(S, "We selected level legs north of 40.05°N", "We selected basin transects (samples north of 40.05°N)")
R(S, "and at least two contributing legs. Five of the 15 flights yield negative slopes", "and at least two contributing transects. Four of the 22 flights yield negative slopes")
RW(S, "To enter the basin analysis, a flight must contain",
   "To enter the basin analysis, a flight must contain at least three basin transects and at least ten samples with methane enhancements above 20 ppb. All 22 flights meet these requirements, together containing 267 basin transects.")
RW(S, "Seven of the 15 eligible flights also pass the reliability screen.",
   "Four of the 22 eligible flights also pass the reliability screen. Their median ethane-to-methane ratio is 0.032 mol mol-1, with individual flight values ranging from 0.020 to 0.035 (Table S5, Figure S9). "
   "Two flights with a single contributing transect, 12 July L1 (0.112, r = 0.88) and 28 June (0.044, r = 0.82), fail the transect-count criterion.")
R(S, "gives an oil-and-gas share of approximately 45% and a biogenic share of 55%, similar to the campaign report.",
     "gives an oil-and-gas share of approximately 39% and a biogenic share of 61%, a more biogenic split than the 51% biogenic share of the campaign report.")
R(S, "dividing the two aircraft medians by 0.5 gives an implied fossil-source ratio of approximately 0.063-0.073 mol mol-1.",
     "dividing the aircraft median by 0.5 gives an implied fossil-source ratio of approximately 0.063 mol mol-1.")
R(S, "All these reference values and conditional estimates remain above 0.059 mol mol-1", "All these reference values and conditional estimates remain above 0.042 mol mol-1")
R(S, "Using 0.063-0.073 mol mol-1 would raise that median to approximately 40-47%.", "Using 0.063 mol mol-1 would raise that median to approximately 34%.")
RW(S, "The more direct check is the agreement between the aircraft analyses",
   "The more direct check is the comparison between the aircraft analyses: our screened basin ratio of 0.032 mol mol-1 is about 25% below the 0.042 mol mol-1 reported from a separate analysis of the same flights,{{h0}} "
   "a difference that reflects the different sample selection and screening and shows that the basin ratio depends on them; the basin ratio does not enter the urban fossil fraction. "
   "Agreement in the inferred oil-and-gas shares would add no independent check in any case, because both analyses use the same ground-level ratio as their endmember.")
R(S, "Legs used count the legs contributing points after the ΔCH4 > 20 ppb gate", "Transects used count the transects contributing points after the ΔCH4 > 20 ppb gate")
R(S, "it is not the number of basin legs flown.", "it is not the number of basin transects flown.")
R(S, "r at least 0.5, and at least two contributing legs.", "r at least 0.5, and at least two contributing transects.")
R(S, "Four flights with slopes of −0.010 to −0.42 mol mol−1 lie below or at the lower edge of the axis and are not drawn, or only partly; their labels remain on the axis.",
     "Flights with negative slopes (−0.035 to −0.001 mol mol−1) lie at or below the lower edge of the axis; 5 July L1 (−0.035) is not drawn, but its label remains on the axis.")

# ---- S6 ----
R(S, "Table S6 lists, for every flight with urban legs,", "Table S6 lists, for every flight with at least three urban transects,")
R(S, "At least two contributing level legs. A slope built from a single leg is one plume transect, not a repeated measurement, so requiring two or more legs means",
     "At least two contributing transects. A slope built from a single transect is one plume crossing, not a repeated measurement, so requiring two or more transects means")
RW(S, "A leave-one-leg-out slope range no wider than half the slope.",
   "A leave-one-transect-out slope range no wider than half the slope. Refitting the slope with each transect dropped in turn, the spread of the resulting slopes must stay within half the full-data slope; a wider spread means one transect is driving the fit, so this bound rejects transect-fragile slopes.{{h0}} "
   "Flights that fail any criterion are marked as not usable in Table 1; the per-flight transect counts and the CH4:CO leave-one-transect-out ranges are tabulated in Table S7 below. "
   f"No CH4:CO2 fit passes the screen: the highest CH4:CO2 correlation among the nine flights of Table 1 is {rco2_9:.2f}, below the 0.7 criterion.")
R(S, "the York fossil fraction. OLS fits the emission slopes; the fossil fraction uses the within-leg York fit", "the York fossil fraction. OLS fits the emission slopes; the fossil fraction uses the within-transect York fit")
R(S, "To show that no single flight leg drives a slope, we report per-flight stability diagnostics: the number of distinct legs contributing, and the range of the CH4:CO slope under leave-one-leg-out refitting (a wide range means the slope is leg-fragile",
     "To show that no single transect drives a slope, we report per-flight stability diagnostics: the number of distinct transects contributing, and the range of the CH4:CO slope under leave-one-transect-out refitting (a wide range means the slope is transect-fragile")
R(S, "the CH4:CO2 emission also carries a leg-block bootstrap", "the CH4:CO2 emission also carries a transect-block bootstrap")
R(S, "Leave-one-leg-out slope ranges that are narrow relative to the slope indicate a robust, multi-leg fit; the CH4:CO2 emission confidence interval is the leg-block bootstrap.",
     "Leave-one-transect-out slope ranges that are narrow relative to the slope indicate a robust, multi-transect fit; the CH4:CO2 emission confidence interval is the transect-block bootstrap.")
R(S, "urban leg centroids", "urban transect centroids")

# ---- S8 ----
R(S, "to identify urban-leg samples that passed downwind", "to identify urban-transect samples that passed downwind")
R(S, "are pooled by flight, leg, and source", "are pooled by flight, transect, and source")
RW(S, "Under the strict criterion there are two encounters.",
   "Under the strict criterion there are seven encounters. Three are downwind of the DADS landfill. The longest 10 July L1 transect discussed in section S3 passed downwind with 97 gated samples, a mean 5.9 km from the landfill and 9° off its downwind line, with methane enhancements of up to 75 ppb: "
   "the slope is -0.0008 (r = -0.04), a campaign-specific confirmation that landfill methane carries no ethane, which is the assumption behind the biogenic endmember of Equation 3. "
   "A second 10 July transect, 11 km downwind, gives -0.003 (r = -0.18; 16 samples). The third, a 10-sample encounter on 8 July 5.8 km downwind with enhancements of at most 32 ppb, gives 0.11 (r = 0.71); with only ten samples, the York-fit minimum, it is too small to characterize the landfill. "
   "The other four are downwind of an oil-and-gas site near Westminster with a single detection in April 2026 (3, 9, 10 and 12 July; 10 to 24 samples, slopes 0.025 to 0.050); with one non-contemporaneous detection they say nothing about that source.")
RW(S, "The relaxed criterion adds the northern legs",
   "The relaxed criterion adds transects of 3 July L1, 9 July, 10 July L1 and the first 13 July flight that passed 4 to 20 km downwind of oil-and-gas sites near Brighton and Westminster (slopes of 0.024 to 0.066 where r is at least 0.7), "
   "and links transects 18 to 20 km downwind of the Tower Road landfill to both the landfill and nearby oil-and-gas sites on four flights (the crosses in Figure S12); that fetch is also the direction of the Adams County part of the basin, so those slopes cannot be attributed to the mapped point sources. "
   "The samples linked to no source give per-flight slopes of 0.006 to 0.081 (median 0.027), the same range as the whole-flight fits. "
   "Carbon Mapper therefore validates the location and type of the discrete sources and the zero-ethane end of the mixing model; it cannot supply a fossil-plume ratio here, because its oil-and-gas detections are few, non-contemporaneous and at the edge of the box.")

# ---- S9 (comment 17: wind roses) ----
RW(S, "Figure S13: Fossil fraction versus the fraction of in-box boundary-layer samples",
   "**Figure S13:** (a) Fossil fraction versus the fraction of in-box boundary-layer samples (below 1.5 km above ground) with wind from 22.5 to 90° (north-north-east to east; the Wattenberg/DJB gas-field sector). "
   "The weak positive correlation (Pearson r = 0.46, n = 9, not significant, p = 0.22) would be consistent with north-easterly advection of ethane-rich basin gas contributing to the day-to-day fossil signal. "
   "With only nine evaluable flights, this is an exploratory association, not a strong causal claim, and wind direction is clearly not the sole control. "
   "(b) Wind rose of the aircraft's own winds at every in-box sample below 1.5 km above ground on all 22 campaign flights; 41% of these samples are from the north-east sector (dashed outline). "
   "(c) Wind rose of the Boulder Doppler-lidar winds averaged over 200 to 1000 m above the lidar for every profile from 1 June to 31 July 2024 (all hours), 13% of them from the north-east sector. "
   "The daytime flights sampled north-easterly upslope flow far more often than the two-month, all-hours record at Boulder. A full-year 2024 wind rose would need a surface-station record (for example the Denver International Airport ASOS), which is not among the inputs of this analysis.")
RW(S, "Across flights the association is weak and not significant (Figure S13).",
   "Across flights the association is weak and not significant (Figure S13). At transect level it is absent (Figure S14). For each of the 36 fitted urban transects we computed the vector-mean aircraft wind, "
   "the angle between that upwind direction and the emission-weighted centroid of the basin's oil-and-gas methane in the gridded EPA inventory (40.45°N, 104.59°W), and an upwind-exposure score for basin-type oil and gas, natural-gas distribution, waste and livestock: "
   "the inventory emission of every grid cell within 150 km, weighted by a Gaussian in its angular offset from the upwind direction (20° width) and by exp(-d/40 km).")
RW(S, "The per-leg York slope is uncorrelated with the angle to the basin",
   "The per-transect York slope is uncorrelated with the angle to the basin (Spearman rho = -0.23, p = 0.17) and with the basin exposure fraction (rho = 0.00); DJB-facing transects (angle at most 45°, n = 22) have a median slope of 0.028 mol mol−1 against 0.022 for the rest (Wilcoxon p = 0.37). "
   "The six transects above 1 km above ground have the same median slope (0.025) as the 30 below. The steepest transect slopes (0.61 on the second 13 July flight at 2.35 km and 1.17 on 5 July L1) come from weakly correlated transects (r = 0.29 and 0.24) with fewer than 30 gated samples and are not evidence of ethane-rich air aloft. "
   "The null result is consistent with a spatially distributed fossil signal rather than a basin-advection gradient, but heterogeneous sources, short-range transport and the small number of transects would also produce it, so we report it without attribution.")
R(S, "Per-leg ethane:methane slope against (a) the angle between the leg's upwind direction", "Per-transect ethane:methane slope against (a) the angle between the transect's upwind direction")
R(S, "Circles are legs below 1 km above ground, triangles above; symbol size scales with the number of gated samples and color",
     "Circles are transects below 1 km above ground, triangles above; symbol size scales with the number of gated samples and color")
R(S, "The cone uses the aircraft's own wind and a fixed geometry.",
     "The cone uses the aircraft's own wind and a fixed geometry. The STILT calculation below uses the 25 urban level legs with fitted slopes of the Vertical_Speed sensitivity definition (section S3), not the constant-altitude transects, and has not been repeated for the transects.")
R(S, "HRRR-STILT exposure of the 25 fitted urban legs.", "HRRR-STILT exposure of the 25 fitted urban level legs of the Vertical_Speed sensitivity definition (section S3).")

# ---- S10 ----
RW(S, "Figure S16 shows, for each of the eight Denver-metro flights",
   "Figure S16 shows, for each of the 16 flights that crossed the Denver-metro box on at least three constant-altitude transects, the flight track colored by methane enhancement, which shows how much of the city each flight-level fossil fraction represents; the mean in-box winds of each flight are listed in Table S1.")
RW(S, "Per-flight urban sampling for the eight Denver-metro flights",
   "**Figure S16:** Per-flight urban sampling for the 16 flights that crossed the Denver-metro box on at least three constant-altitude transects (the flights screened for Table 1), colored by CH4 enhancement over a Vulcan fossil-CO2 background. "
   "Each panel title gives the flight and, for the nine flights with an estimable slope, its York fossil fraction (Table 1). "
   "The first 13 July flight shows the largest enhancements of the campaign and the highest fossil fraction (35%); the second 13 July flight was sampled under the same north-easterly flow from the Wattenberg/DJB field but with smaller enhancements.")

# ---- S11 ----
R(S, "For every urban leg in the boundary layer", "For every urban transect in the boundary layer")
R(S, "the along-leg integral of air density times methane enhancement times the wind component perpendicular to the leg, multiplied by the lidar mixing height over that flight's urban legs",
     "the along-transect integral of air density times methane enhancement times the wind component perpendicular to the transect, multiplied by the lidar mixing height over that flight's urban transects")
RW(S, "There are five crossings on three flights at two of the hotspot sites",
   f"There are 15 crossings on seven flights at two of the hotspot sites (Table S10, Figure S17). Their total rates are 0.11 to 7.73 t/h and their biogenic parts 0 to 5.54 t/h. "
   f"Relative to the v2.0beta waste emissions in the fetch, the aircraft biogenic rate is {ENC_RB:.2f} in the median (range 0 to {rb[-1]:.1f}); relative to the v2.0beta total in the fetch, the aircraft total is {ENC_RT:.2f} in the median. "
   "Pooling across sites mixes a landfill and a wastewater cell, so the per-site values below are the informative ones. The fetch of each crossing is oriented by the vector mean of the aircraft wind along the crossed segment.")
RW(S, "By site: the one crossing downwind of the DADS landfill",
   "By site: the three crossings downwind of the DADS landfill (v2.0beta waste 6.07 t/h in that cell; 8 and 10 July L1) give a median biogenic rate of 0.11 t/h (total 0.11 to 1.04 t/h), 0.02 times the v2.0beta waste emissions in the upwind fetch; "
   "the largest, on 8 July, carries an ethane signal (slope 0.13) that assigns it entirely to fossil methane. "
   "The twelve crossings downwind of the waste cell immediately north of the Suncor/Metro Water Recovery complex (v2.0beta waste 0.39 t/h in that cell; seven flights) give a median biogenic rate of 0.85 t/h (total 0.76 to 7.73 t/h), 2.14 times the v2.0beta waste emissions in the upwind fetch. "
   "Excluding the two crossings there flown in winds below 2 m s−1 (3 July L1 and 10 July L1) leaves both medians unchanged. "
   "Which facility the third cell represents cannot be determined from the gridded product: it lies immediately north of the Metro Water Recovery plant, and the cell that contains the plant itself carries no waste methane in v2.0beta, so the cell may be the plant with a displaced location or another waste facility. "
   "Either way, the aircraft find far less methane than v2.0beta downwind of the landfill they could test, and more biogenic methane around the wastewater plant than v2.0beta places in that cell and the plant's own cell together: "
   "the v2.0beta waste total is high, and its split between the landfills and the wastewater plant appears to be the reverse of what the aircraft, and Carbon Mapper's persistence-weighted landfill rates (Table S9), indicate.")
RW(S, "Aircraft plume crossings downwind of the v2.0beta waste hotspots (sites numbered",
   "**Table S10:** Aircraft plume crossings downwind of the v2.0beta waste hotspots (sites numbered as in Table S9): single-transect rate, ethane split, and the inventory emissions in the upwind fetch. "
   "Transect gives the transect segment's number within the flight, in time order. Crossings whose hotspot cell fell outside the fetch (the transect was too far off the downwind line) are omitted; the fossil fraction is blank where too few samples passed the methane gate for an ethane fit. "
   "Distance is the mean distance of the crossing's samples from the center of the 4-km hotspot cell. For site 1 the cell center lies 0.4 km from the Carbon Mapper DADS source (2.0 km from the facility's street address, Table S9); "
   "the 5.0 km here for the main 10 July crossing and the 5.9 km of sections S3 and S8 differ because they average different samples (the crossed segment and the samples linked to the source).")

R(S, "pressure at the leg altitude", "pressure at the transect altitude")
R(S, "(iii) vertically stacked legs extend", "(iii) vertically stacked transects extend")
R(S, "without the correlation and leg-count screen", "without the correlation and transect-count screen")
R(S, "and leg-block bootstrap used for the urban analysis", "and transect-block bootstrap used for the urban analysis")
R(S, "pooling its basin legs rather than using the per-leg-intercept estimator", "pooling its basin transects rather than using the per-transect-intercept estimator")
R(S, "which is what the leg-block bootstrap resamples", "which is what the transect-block bootstrap resamples")
R(S, "Intervals are 95% leg-block bootstrap.", "Intervals are 95% transect-block bootstrap.")
R(S, "carry 95% leg-block bootstrap intervals", "carry 95% transect-block bootstrap intervals")
R(S, "co-vary along the legs, so the slope", "co-vary along the transects, so the slope")
# ---- SI tables ----
S1rows = []
for r in T1:
    f = r["Flight"]; s = FS[f]
    S1rows.append([lab(f), s["flight_utc"], s["inbox_utc"], s["n_box_transects"], f"{int(s['inbox_transect_s']):,}", s["agl_range"],
                   s["bl_wind"].replace(" m/s from ", " m/s, "), s["above_wind"].replace(" m/s from ", " m/s, "),
                   s["blh_m"], s["n_alt_levels"], s["encircles"]])
TAB(S, 0, S1rows, header=["Flight", "UTC (flight)", "UTC (in box)", "Urban transects", "In-box time (s)", "Heights (m AGL)",
                          "Wind below 1.5 km", "Wind above 1.5 km", "BLH (m)", "Levels", "Encircles"],
    widths=[1.2, 1.0, 1.0, 0.85, 0.8, 0.9, 1.25, 1.25, 0.7, 0.65, 0.8])
def wall(r):
    off = float(r["offset_km_from_box_center"]); ax = "N-S" if r["axis"] == "NS" else "E-W"
    return f"{ax}, {rnd(off)} km"
S2rows = []
for r in sorted(cs, key=lambda r: (-int(r["conditions_met"]), r["flight"])):
    S2rows.append([lab(r["flight"]), wall(r), f"{r['n_legs']} / {r['n_alt_levels']}", f"{r['lowest_agl_m']}-{r['highest_agl_m']}", r["blh_m"],
                   f"{float(r['width_frac']):.2f}", f"{float(r['U_perp_ms']):.1f} / {float(r['constancy']):.2f}", f"{r['conditions_met']} of 4"])
TAB(S, 1, S2rows, header=["Flight", "Wall", "Transects / levels", None, None, None, None, None])
TAB(S, 2, [[lab(f), qc[f]["fossil_full"], qc[f]["fossil_QC"], qc[f]["delta"]] for f in FL])
S5rows = []
for r in rd("basin_ethane_methane.csv"):
    cil = f"{float(r['ci_lo']):.4f} to {float(r['ci_hi']):.4f}" if r["ci_lo"] != "NA" else "n/a (one transect)"
    S5rows.append([lab2(r["flight"]), r["n_basin_legs"], r["n_legs_used"], r["n_points"], f"{float(r['basin_ratio']):.4f}", cil, f"{float(r['r']):.3f}", "yes" if r["usable"] == "TRUE" else "no"])
TAB(S, 4, S5rows, header=["Flight", "Basin transects", "Transects used", None, None, None, None, None])
S6rows, S7rows = [], []
for r in T1:
    f = r["Flight"]; x = RF[f]
    fp = T1F[f]["fossil_pct"]
    S6rows.append([lab(f), x["n"], f"{float(x['ch4_co_slope']):.3f}", f"{float(x['r_co']):.2f}", f"{float(x['ch4_co2_slope']):.2f}", f"{float(x['r_co2']):.2f}", fp if fp != "NA" else "n/a"])
    l1o = f"{float(x['co_l1o_min']):.3f} to {float(x['co_l1o_max']):.3f}" if x["co_l1o_min"] != "NA" else "n/a"
    S7rows.append([lab(f), x["n_legs_co"], l1o, x["n_legs_co2"], f"{float(x['E_CH4_CO2_lo']):.2f} to {float(x['E_CH4_CO2_hi']):.2f}"])
TAB(S, 5, S6rows)
TAB(S, 6, S7rows, header=["Flight", "Transects (CO)", "CO slope, leave-one-transect-out range", "Transects (CO2)", None])
f2 = lambda v, d=2: "" if v in ("NA", "") else f"{float(v):.{d}f}"
S10rows = []
for r in sorted(ENC, key=lambda r: (r["hotspot_id"], r["flight"], int(r["leg_id"]))):
    S10rows.append([lab(r["flight"]), r["leg_id"], r["hotspot_id"], f2(r["dist_km"], 1), r["agl_m"], r["blh_m"], r["dch4_max_ppb"], f2(r["Q_t_hr"]),
                    f2(r["fossil_frac"]), f2(r["Q_bio_t_hr"]), f2(r["v2_fetch_waste_t_hr"]), f2(r["v2_fetch_total_t_hr"]), f2(r["v1_fetch_total_t_hr"])])
TAB(S, 9, S10rows, header=["Flight", "Transect", None, None, None, None, None, None, None, None, None, None, None])
CELLS = [("si", 8, 1, 8, "102"), ("si", 8, 2, 8, "420")]

# ================================================================================================
# machinery
# ================================================================================================
W_ = lambda tag: qn("w:" + tag)
def vis_text(p):          # everything the reader sees in the paragraph, hyperlinks and insertions included
    return "".join(t.text or "" for t in p._p.iter(W_("t")))
def sentinel(p):          # direct-run text; a hyperlinked citation or tracked insertion is a hard boundary
    out = []
    for ch in p._p:
        if ch.tag == W_("r"): out.append("".join(t.text or "" for t in ch.iter(W_("t"))))
        elif ch.tag in (W_("hyperlink"), W_("ins")): out.append("\x00")
    return "".join(out)
def find_para(d, anchor, label):
    hits = [p for p in L.all_paragraphs(d) if anchor in vis_text(p)]
    if len(hits) != 1: sys.exit(f"{label}: paragraph anchor found {len(hits)}x, expected 1: {anchor[:90]!r}")
    return hits[0]

def do_replace(d, old, new, count, label):
    paras = L.all_paragraphs(d)
    n = sum(sentinel(p).count(old) for p in paras)
    if n != count: sys.exit(f"{label}: anchor found {n}x in run text (citation-safe), expected {count}x: {old[:90]!r}")
    for p in paras:
        k = sentinel(p).count(old)
        if not k: continue
        if L.runs_text(p).count(old) != k: sys.exit(f"{label}: anchor spans a citation: {old[:80]!r}")
        pos = 0
        for _ in range(k): pos = L.replace_in_paragraph(p, old, new, pos)

TOK = re.compile(r"(\{\{/?c\d+\}\}|\{\{[hx]\d+\}\}|\*\*.+?\*\*)")
def do_rewrite(d, anchor, text, label):
    p = find_para(d, anchor, label); P = p._p
    if P.find(".//" + qn("m:oMath")) is not None: sys.exit(f"{label}: paragraph holds an equation; use R: {anchor[:60]!r}")
    kids = list(P); pPr = P.find(W_("pPr"))
    H = [k for k in kids if k.tag == W_("hyperlink")]; X = [k for k in kids if k.tag == W_("ins")]
    CS = {k.get(W_("id")): k for k in kids if k.tag == W_("commentRangeStart")}
    CE = {k.get(W_("id")): k for k in kids if k.tag == W_("commentRangeEnd")}
    CR = {}
    for k in kids:
        if k.tag == W_("r") and k.find(W_("commentReference")) is not None: CR[k.find(W_("commentReference")).get(W_("id"))] = k
    BM = [k for k in kids if k.tag in (W_("bookmarkStart"), W_("bookmarkEnd"))]
    textruns = [k for k in kids if k.tag == W_("r") and k.find(W_("commentReference")) is None and "".join(t.text or "" for t in k.iter(W_("t"))).strip()]
    tmpl = max(textruns, key=lambda k: len("".join(t.text or "" for t in k.iter(W_("t"))))) if textruns else None
    for k in kids:
        if k is not pPr: P.remove(k)
    for b in BM: P.append(b)
    def textrun(s, bold=False):
        r = copy.deepcopy(tmpl) if tmpl is not None else P.makeelement(W_("r"), {})
        for ch in list(r):
            if ch.tag != W_("rPr"): r.remove(ch)
        P.append(r); run = Run(r, p); run.text = s; run.font.color.rgb = RED
        rp = r.find(W_("rPr"))
        if rp is not None:
            for t in ("vertAlign", "i", "iCs", "u"):
                e = rp.find(W_(t))
                if e is not None: rp.remove(e)
        if bold: run.font.bold = True
    usedH, usedX, usedC = set(), set(), set()
    for tok in TOK.split(text):
        if not tok: continue
        m = re.fullmatch(r"\{\{(/?c|h|x)(\d+)\}\}", tok)
        if m:
            kind, i = m.group(1), m.group(2)
            if kind == "h": P.append(H[int(i)]); usedH.add(int(i))
            elif kind == "x": P.append(X[int(i)]); usedX.add(int(i))
            elif kind == "c":                   # a marker nested in a hyperlink or insertion travels with it
                if i in CS: P.append(CS[i]); usedC.add(i)
            else:
                if i in CE: P.append(CE[i])
                if i in CR: P.append(CR[i])
        elif tok.startswith("**"): textrun(tok[2:-2], bold=True)
        else: textrun(tok)
    if usedH != set(range(len(H))) or usedX != set(range(len(X))):
        sys.exit(f"{label}: rewrite of {anchor[:50]!r} places hyperlinks {sorted(usedH)} of {len(H)}, insertions {sorted(usedX)} of {len(X)}")
    for cid in CS:                                   # comments not placed explicitly span the whole paragraph
        if cid not in usedC:
            (pPr.addnext(CS[cid]) if pPr is not None else P.insert(0, CS[cid]))
            if cid in CE: P.append(CE[cid])
            if cid in CR: P.append(CR[cid])
    for cid in list(CE) + list(CR):
        for dct in (CE, CR):
            if cid in dct and dct[cid].getparent() is None: P.append(dct[cid])

def set_cell(cell, text):
    paras = cell.paragraphs; p = paras[0]
    for q in paras[1:]: q._p.getparent().remove(q._p)
    old = vis_text(p)
    if old.strip() == text.strip(): return 0
    runs = [r for r in p.runs if r._r.find(W_("commentReference")) is None]
    if runs:
        for r in runs[1:]: r._r.getparent().remove(r._r)
        base = runs[0]
    else:
        base = p.add_run()
    base.text = text; base.font.color.rgb = RED
    return 1

def strip_comments(el):
    for t in ("commentRangeStart", "commentRangeEnd"):
        for e in list(el.iter(W_(t))): e.getparent().remove(e)
    for e in list(el.iter(W_("commentReference"))):
        r = e.getparent(); r.getparent().remove(r)

def resize_table(tbl, nrows, ncols, widths=None):
    t = tbl._tbl; grid = t.find(W_("tblGrid")); cols = grid.findall(W_("gridCol"))
    total = sum(int(c.get(W_("w"))) for c in cols)
    if widths: total = max(total, 9360)                # 6.5 in
    wl = [int(total * w / sum(widths)) for w in widths] if widths else [total // ncols] * ncols
    while len(cols) < ncols: c = copy.deepcopy(cols[-1]); cols[-1].addnext(c); cols.append(c)
    while len(cols) > ncols: grid.remove(cols.pop())
    for c, w_ in zip(cols, wl): c.set(W_("w"), str(w_))
    if widths:
        tp = t.find(W_("tblPr")); tw = tp.find(W_("tblW")) if tp is not None else None
        if tw is not None: tw.set(W_("w"), str(total)); tw.set(W_("type"), "dxa")
    trs = t.findall(W_("tr"))
    while len(trs) < nrows: n = copy.deepcopy(trs[-1]); strip_comments(n); trs[-1].addnext(n); trs.append(n)
    while len(trs) > nrows: t.remove(trs.pop())
    for tr in trs:
        tcs = tr.findall(W_("tc"))
        while len(tcs) < ncols: n = copy.deepcopy(tcs[-1]); strip_comments(n); tcs[-1].addnext(n); tcs.append(n)
        while len(tcs) > ncols: tr.remove(tcs.pop())
        for tc, w_ in zip(tcs, wl):
            pr = tc.find(W_("tcPr"))
            w = pr.find(W_("tcW")) if pr is not None else None
            if w is not None: w.set(W_("w"), str(w_)); w.set(W_("type"), "dxa")

def do_table(d, ti, rows, header, label, widths=None):
    tbl = d.tables[ti]; ncols = len(rows[0]); assert all(len(r) == ncols for r in rows), f"{label} table {ti}: ragged rows"
    resize_table(tbl, len(rows) + 1, ncols, widths)
    n = 0
    if header:
        for j, h in enumerate(header):
            if h is not None: n += set_cell(tbl.rows[0].cells[j], h)
    for i, row in enumerate(rows, start=1):
        for j, v in enumerate(row): n += set_cell(tbl.rows[i].cells[j], str(v))
    if widths:                                            # a wide table: 7 pt throughout
        for row in tbl.rows:
            for c in row.cells:
                for q in c.paragraphs:
                    for r in q.runs: r.font.size = Pt(7)
    return n

def renumber_sections(d, label):
    """the original text's 'section 3.2' (source apportionment) is section 3.3 now; red text is already new"""
    n = 0
    pat = re.compile(r"(sections? )3\.2(?!\d)")
    for p in L.all_paragraphs(d):
        for r in list(p.runs):
            try:
                if r.font.color.rgb == RED: continue
            except Exception: pass
            if not pat.search(r.text): continue
            pieces, last = [], 0
            for m in pat.finditer(r.text):
                pieces += [(r.text[last:m.end(1)], False), ("3.3", True)]; last = m.end()
            pieces.append((r.text[last:], False))
            prev = r._r
            for txt, red in pieces:
                if not txt: continue
                el = copy.deepcopy(r._r); prev.addnext(el); prev = el
                nr = Run(el, p); nr.text = txt
                if red: nr.font.color.rgb = RED
            r._r.getparent().remove(r._r); n += 1
    for p in L.all_paragraphs(d):
        if pat.search(L.runs_text(p).replace("3.3", "")) and re.search(r"sections? 3\.2(?!\d)", sentinel(p)):
            sys.exit(f"{label}: a 'section 3.2' reference spans runs: {L.runs_text(p)[:100]!r}")
    print(f"{label}: {n} run(s) renumbered section 3.2 -> 3.3")

def move_section_31(d):
    body = d.element.body
    paras = list(d.paragraphs)
    start = next(p for p in paras if vis_text(p).startswith("3.1. Constant-altitude flight transects"))
    stop = next(p for p in paras if vis_text(p).startswith("3.4. Emission estimate"))
    dest = next(p for p in paras if vis_text(p).strip() == "3. Statistical Analysis")
    block, el = [], start._p
    while el is not stop._p: block.append(el); el = el.getnext()
    anchor = dest._p
    for e in block: anchor.addnext(e); anchor = e
    print(f"main text: section 3.1 (transects) moved ahead of 3.2 ({len(block)} elements)")

def gra2pes_superscript(d):
    n = 0
    for p in L.all_paragraphs(d):
        for r in list(p.runs):
            if "GRA2PES" not in r.text: continue
            pieces = []
            for k, part in enumerate(r.text.split("GRA2PES")):
                if k: pieces += [("GRA", False), ("2", True), ("PES", False)]
                pieces.append((part, False))
            prev = r._r
            for txt, sup in pieces:
                if not txt: continue
                el = copy.deepcopy(r._r); prev.addnext(el); prev = el
                nr = Run(el, p); nr.text = txt
                if sup: nr.font.superscript = True; nr.font.color.rgb = RED
            r._r.getparent().remove(r._r); n += 1
    return n

def run_doc(src, dst, which, label):
    d = docx.Document(src)
    renumber_sections(d, label)
    n = 0
    for op in OPS[which]:
        kind = op[0]
        if kind == "R": do_replace(d, op[1], op[2], op[3], label); n += 1
        elif kind == "RW": do_rewrite(d, op[1], op[2], label); n += 1
        elif kind == "INS": L.insert_after(find_para(d, op[1], label), op[2], d); n += 1
        elif kind == "TAB": c = do_table(d, op[1], op[2], op[3], label, op[4]); print(f"{label}: table {op[1]} rebuilt ({len(op[2])} rows, {c} cells changed)")
    for doc_, ti, ri, ci_, new in CELLS:
        if doc_ == which: set_cell(d.tables[ti].rows[ri].cells[ci_], new)
    if which == "main": move_section_31(d)
    allp = L.all_paragraphs(d)
    m = L.subscript_chem(allp); e = L.exponent_fix(L.all_paragraphs(d)); c = L.cite_fix(L.all_paragraphs(d)); g = gra2pes_superscript(d)
    d.save(dst)
    print(f"{label}: {n} edit(s) -> {dst} ({m} chemical subscripts, {e} exponents, {c} citations, {g} GRA2PES)")

# ---- figures: replace the embedded pictures; resize the frame where the aspect ratio changed ----
def png_size(b): return struct.unpack(">II", b[16:24]) if b[:8] == b"\x89PNG\r\n\x1a\n" else None
def swap_figures(path, mapping, resize, label):
    tmp = Path(str(path) + ".tmp"); newsz = {}
    with zipfile.ZipFile(path) as zin:
        rels = zin.read("word/_rels/document.xml.rels").decode()
        rid = {m.group(2).split("/")[-1]: m.group(1) for m in re.finditer(r'Id="(rId\d+)"[^>]*Target="([^"]+)"', rels)}
        rid.update({m.group(1).split("/")[-1]: m.group(2) for m in re.finditer(r'Target="([^"]+)"[^>]*Id="(rId\d+)"', rels)})
        docxml = zin.read("word/document.xml").decode()
        with zipfile.ZipFile(tmp, "w", zipfile.ZIP_DEFLATED) as zout:
            for item in zin.infolist():
                data = zin.read(item.filename); name = item.filename.split("/")[-1]
                if item.filename.startswith("word/media/") and name in mapping:
                    new = Path(mapping[name]).read_bytes(); so, sn = png_size(data), png_size(new)
                    if name not in resize and abs((so[0] / so[1]) / (sn[0] / sn[1]) - 1) > 0.05:
                        sys.exit(f"{label}: {name} {so} vs {Path(mapping[name]).name} {sn}: aspect ratio differs")
                    data = new; newsz[name] = sn; print(f"  {label}: {name} {so} <- {Path(mapping[name]).name} {sn}")
                if item.filename == "word/document.xml": continue
                zout.writestr(item, data)
            for name in resize:                       # keep the frame width, set the height from the new aspect ratio
                r_ = rid[name]; w, h = newsz[name]
                m = re.search(r'<w:drawing>(?:(?!</w:drawing>).)*?r:embed="%s"(?:(?!</w:drawing>).)*?</w:drawing>' % r_, docxml, re.S)
                blk = m.group(0); cx = int(6.5 * 914400); cy = int(round(cx * h / w))
                if cy > 8.4 * 914400:                     # keep inside the page: limit height to 8.4 in
                    cy = int(8.4 * 914400); cx = int(round(cy * w / h))
                nb = re.sub(r'(<wp:extent cx=")\d+(" cy=")\d+(")', rf'\g<1>{cx}\g<2>{cy}\g<3>', blk)
                nb = re.sub(r'(<a:ext cx=")\d+(" cy=")\d+(")', rf'\g<1>{cx}\g<2>{cy}\g<3>', nb)
                docxml = docxml.replace(blk, nb); print(f"  {label}: {name} frame set to {cx/914400:.2f} x {cy/914400:.2f} in")
            zout.writestr("word/document.xml", docxml)
    missing = set(mapping) - set(newsz)
    if missing: sys.exit(f"{label}: media not found: {missing}")
    shutil.move(str(tmp), str(path))

MAIN_FIGS = {"image2.png": "Fig1_study_map.png", "image1.png": "Fig2_source_mix.png", "image3.png": "Fig3_ethane_contrast.png", "image4.png": "Fig4_source_combined.png"}
SI_FIGS = {"image11.png": "FigS6_loop_closure.png",          # S1
           "image12.png": "FigS5_beta_sensitivity.png",      # S4
           "image17.png": "FigS3b_two_endmember.png",        # S5
           "image16.png": "FigS7_regression_comparison.png", # S6
           "image10.png": "FigS10_enh_threshold.png",        # S7
           "image7.png": "FigS_lag_sensitivity.png",         # S8
           "image14.png": "FigS8_basin_ethane_trend.png",    # S9
           "image8.png": "FigS13_spatial_match.png",         # S11
           "image3.png": "FigS12_carbonmapper_encounters.png",  # S12
           "image15.png": "FigS13_wind_fossil_rose.png",     # S13
           "image9.png": "FigS11_wind_exposure.png",         # S14
           "image13.png": "SI_perflight_maps.png",           # S16
           "image4.png": "FigS14_v2_pointsource.png"}        # S17
SI_RESIZE = {"image11.png", "image15.png", "image13.png"}

run_doc(a.main_in, a.main_out, "main", "main text")
run_doc(a.si_in, a.si_out, "si", "SI")
swap_figures(a.main_out, {k: FIG / v for k, v in MAIN_FIGS.items()}, set(), "main text")
swap_figures(a.si_out, {k: FIG / v for k, v in SI_FIGS.items()}, SI_RESIZE, "SI")

# ================================================================================================
# checks on the written documents
# ================================================================================================
def doc_text(path):
    d = docx.Document(path); return d, "\n".join(vis_text(p) for p in d.paragraphs)   # prose only; table cells hold numbers
dm, tm = doc_text(a.main_out); ds, ts = doc_text(a.si_out)
for bad in ("0.059", "Seven of the 8", "2 to 47%", "eight urban flights", "24 candidate", "4.6 to 10.7", "single screen-passing CH4:CO2",
            "leg-block", "within-leg", "Level-leg detection", "quiet residuals", "regenerates every number xx"):
    assert bad not in tm, f"main text: {bad!r} remains"
for bad in ("0.059", "24 candidate", "eight Denver-metro", "leg-block", "within-leg", "Twelve of the 22", "0.036 mol mol-1, with"):
    assert bad not in ts, f"SI: {bad!r} remains"
heads = [vis_text(p) for p in dm.paragraphs if re.match(r"3\.\d\. ", vis_text(p))]
assert heads[:5] == ["3.1. Constant-altitude flight transects", "3.2. Background and enhancement", "3.3. Source apportionment from the ethane-to-methane ratio",
                     "3.4. Emission estimate from the CH4:CO and CH4:CO2 ratios", "3.5. Inventory-proportional source allocation"], heads
for path, n_exp, label in ((a.main_out, 33, "main text"), (a.si_out, 2, "SI")):
    x = zipfile.ZipFile(path).read("word/document.xml").decode()
    ids = set(re.findall(r'<w:commentReference w:id="(\d+)"', x)); st_ = set(re.findall(r'<w:commentRangeStart w:id="(\d+)"', x))
    assert len(ids) == n_exp and st_ <= ids, (label, len(ids), sorted(st_ - ids))
    assert x.count("<w:ins ") >= (1 if label == "main text" else 0)
assert [c.text for c in dm.tables[0].rows[1].cells] == ["2024-07-03 (L1)", "5", "24 [23 to 26]", "5.9 [2.5 to 8.2]", "not usable", "CO"], [c.text for c in dm.tables[0].rows[1].cells]
assert len(dm.tables[0].rows) == 10 and len(ds.tables[0].rows) == 17 and len(ds.tables[1].rows) == 49 and len(ds.tables[9].rows) == 16
assert ds.tables[8].rows[1].cells[8].text == "102" and ds.tables[8].rows[2].cells[8].text == "420"
print("checks passed: stale values absent, sections renumbered, all 33 + 2 comments anchored, tables sized")

# ================================================================================================
# response to the comments
# ================================================================================================
RESP = [
 ("main 0", "Priyanka deSouza", "Add name and affiliation.", "For each co-author to complete; no change made."),
 ("main 1", "John Durant", "Seasonality: more biogenic methane in summer?", "Already discussed in the limitations (summer only; Floerchinger et al. seasonal contrast) and in the Discussion; the new closing sentence of the abstract states 'in summer'. No further change."),
 ("main 2", "John Durant", "Abstract needs a final sentence on significance.", "Added: in summer, landfills and wastewater rather than the gas system dominated the methane sampled over Denver, so monitoring and mitigation focused only on the gas system would miss most of it."),
 ("main 3", "Anonymous", "Add a reference (urban areas significant methane sources).", "Added refs 2, 3, 5 as a plain superscript (red). Convert to a Zotero citation before submission."),
 ("main 4", "Anonymous", "Add a reference (biogenic methane does not co-emit ethane).", "Added ref 3 as a plain superscript (red). Convert to a Zotero citation; a dedicated reference (e.g., a review of ethane-to-methane source signatures) would be stronger."),
 ("main 5", "John Durant", "Sentence at odds with 'two decades of research'; restructure as 'Despite two decades...'.", "Paragraph reordered: the DJB research comes first, then 'Despite these two decades of basin research, the methane source mix of the Colorado Front Range's urban core has been understudied'."),
 ("main 6", "Anonymous", "How much time (declining intensity)?", "Added '(for example, across 2010 to 2019 in the satellite record)', matching ref 9 (Lu et al. 2023). Please confirm this is the time span you intend."),
 ("main 7", "Anonymous", "How many legs make a curtain?", "Added '(typically three or more level legs at different altitudes spanning the boundary layer)'."),
 ("main 8", "John Durant", "Summary of data streams; summary table; reorder section 2.", "Added a paragraph after the '2. Data' heading listing the six data streams and the role of each. Not done: a summary table and reordering 2.4 to 2.6 (it would renumber cross-references in both documents); say if you want either."),
 ("main 9", "Priyanka deSouza", "CH4 and C2H6 calibration and QA/QC.", "Added what the data files state: revision R0 preliminary data based on the pre-campaign laboratory calibration, 1σ uncertainties (CH4 1 ppb, C2H6 0.2 ppb, CO2 0.1 ppm, CO 5 ppb) and the 2.5 s Picarro resolution. A red bracketed placeholder asks the instrument PIs for standards, in-flight checks and flagging."),
 ("main 10", "John Durant", "How do 22 files relate to 12 days and Table 1?", "Section 2.1 now gives the full accounting: 12 days, 10 of them with two files (L1, L2) = 22 files; 20 crossed the box, 16 on at least three transects, 9 (on 7 days) gave a slope and are in Table 1."),
 ("main 11", "John Durant", "Table 1 lists only some days.", "Same accounting as comment 10; the Table 1 caption and the note under it name the seven screened flights without an estimable slope, and Table S1 lists all 16."),
 ("main 12", "John Durant", "Difference between 2.2 and 2.3; combine?", "Kept separate but renamed '2.2. CDPHE facility surveys' and '2.3. NOAA ARL mobile laboratory', with a sentence in 2.2 stating the difference (methane-only, repeated circuits of three fixed facilities over 2023-2025, versus a regional campaign-period drive with ethane)."),
 ("main 13", "John Durant", "How are the surveys defined vs section 2.3?", "See comment 12."),
 ("main 14", "John Durant", "Instrument and QA/QC details for the surveys.", "Named the Picarro G2204 in 2.2 and added an 'Instrument and quality control' paragraph to SI S2.1 (21 s inlet-delay correction, native-cadence averaging, depot exclusion), with a placeholder for CDPHE's calibration procedure."),
 ("main 15", "John Durant", "Is the 173/169 files detail necessary?", "Removed; kept the 801 to 925 days per site."),
 ("main 16", "John Durant", "Vehicle details and route map.", "Section 2.3 now points to the instrument list in S2.2 and the plume map in Figure S3. A drive-route map is not yet in the SI; it can be drawn from the ARL car files if wanted."),
 ("main 17", "John Durant", "Wind roses for June-July and for 2024.", "Figure S13 now has (b) a rose of the aircraft's in-box boundary-layer winds on all 22 flights and (c) a rose of the Boulder Doppler-lidar winds (200-1000 m) for every profile in June and July 2024. A full-year 2024 rose needs a surface station record (e.g., the Denver airport ASOS); the data portal was not reachable from this session, so that panel is still to do."),
 ("main 18", "John Durant", "Eight flights here vs 12 in 2.1.", "Now 'the nine flights with estimable urban slopes (Table 1)'; the accounting is in 2.1 (comment 10)."),
 ("main 19", "John Durant", "31 or 32 plumes?", "32 in all (31 methane, 1 CO2); the text now says so."),
 ("main 20", "John Durant", "Name the species in the first sentence.", "Added the species for each platform after X(t)."),
 ("main 21", "John Durant", "Quantile or 5th percentile?", "Now 'rolling 5th percentile', matching Equation 2."),
 ("main 22", "John Durant", "Odd nomenclature 'quiet residuals'.", "Now 'quiet-period enhancements'."),
 ("main 23", "John Durant", "Conceptual model; map of Denver, DJB, landfills, winds.", "Added a conceptual-model paragraph at the end of the new section 3.1 (box air as a background plus overlapping plumes; the fitted slope as the methane-weighted upwind source mix; north-easterly vs westerly flow). It points to Figure 1, Figure S10, Table S9, Table S1 and the new wind roses rather than adding a new map."),
 ("main 24", "John Durant", "Where does 300 samples come from?", "Added '(5 minutes, about 20 km of flight; the choice of window is explained below)'; the next paragraphs give the rationale."),
 ("main 25", "John Durant", "Why would the background change over short distances?", "Added an explanation: for the survey vehicle 'background' is the local roadside baseline, often inside diffuse plumes of the facility or nearby roads, not the regional background."),
 ("main 26", "John Durant", "Compliment.", "No change."),
 ("main 27", "John Durant", "Move leg detection to 3.1.", "Done: '3.1. Constant-altitude flight transects' now opens section 3; old 3.1 and 3.2 are 3.2 and 3.3, and every cross-reference follows."),
 ("main 28", "John Durant", "What do the circles represent; are all tracks shown?", "Caption now explains each circle (mean position of one transect segment, colored by its mean CH4 enhancement) and states that all 22 flight tracks and all 570 segments are shown."),
 ("main 29", "John Durant", "'threshold' -> boundary?", "Now 'DJB basin boundary at 40.05°N (dashed line; samples north of it are treated as basin)'."),
 ("main 30", "John Durant", "Blue line hard to see.", "Figure 1 redrawn: the 13 July L1 track is dark navy over a white casing; caption updated."),
 ("main 31", "John Durant", "Add Table S1 with flight times, leg altitudes, winds, BLH.", "Table S1 is now a per-flight summary for all 16 screened flights: flight and in-box UTC windows, number of urban transects, in-box time, transect heights, mean in-box wind below and above 1.5 km, BLH, levels and loop closure."),
 ("main 32", "John Durant", "Total legs and seconds in the box; compare with other studies.", "Added to 4.1 and the limitations: 398 transects, 109 crossing the box, 8.3 h in the box overall; the 9 analysed flights hold 58 urban transects, 6.7 h (24,158 s) and 4,475 gated samples. Not added: a comparison with other urban campaigns, because they do not report in-box sampling time in comparable form; flight counts could be compared if you want."),
 ("SI 0", "Sunil Baidar", "How are urban legs defined? Upwind/middle/downwind legs in and above the BL are being split.", "Answered by the transect redesign: constant-altitude transects from GPS altitude and track (main 3.1), at every altitude in and above the mixed layer, with gentle course corrections merged rather than split; the vertical-speed legs are now a sensitivity (SI S3; median 29% vs 21%). Table S1 header is now 'Urban transects'."),
 ("SI 1", "Sunil Baidar", "Which variable defines the mixing height?", "Named in S1.1 and the Table S2 caption: the noise-corrected vertical-velocity variance (wVar in the velStats files), threshold 0.1 m2 s-2, 20 min profiles with 50 m gates from 102 m, median over the flight window."),
]
rdoc = docx.Document()
rdoc.add_heading("Responses to co-author comments (manuscript and SI, 9 October 2026)", level=1)
rdoc.add_paragraph("Every change is in red in the manuscript and SI. The comment threads are kept in both files so each can be answered and resolved in Word. "
                   "The transect redesign also changes the results; the main changes are listed after the table.")
tb = rdoc.add_table(rows=1, cols=4); tb.style = "Table Grid"
for j, h in enumerate(("Comment", "From", "Comment (short)", "Response / change")): tb.rows[0].cells[j].text = h
for row in RESP:
    cells = tb.add_row().cells
    for j, v in enumerate(row): cells[j].text = v
for row in tb.rows:
    for c in row.cells:
        for p in c.paragraphs:
            for r in p.runs: r.font.size = Pt(8)
rdoc.add_heading("Result changes with constant-altitude transects", level=2)
for t in ["Flights with an estimable slope: 9 of 16 (was 7 of 8). Campaign median 21% (0-35%) at 0.102; 19-26% across the regional endmembers; break-even endmember 0.042.",
          "CH4:CO emissions 4.4-10.3 t/h (median 6.5) on four flights. No flight passes the CH4:CO2 screen (r_CO2 0.45-0.63), so the CO2 cross-check is gone from the abstract, 4.2 and the Discussion.",
          "The QC filter (Table S3) leaves the median at 21% but moves three flights by 8-17 points (8 July L1; both 13 July flights).",
          "The transect-level ethane floors (0.024-0.044) no longer constrain the endmember: even the strongest allows 41-72% East Coast-type distribution gas.",
          "Basin ratio: four flights pass the screen, median 0.032 (was 0.036); implied O&G share 39% at 0.0813.",
          "The STILT analysis (S9, 4.3, limitations) was not rerun: it uses the 25 vertical-speed legs, and the text now says so.",
          "Carbon Mapper Table S9 rates set to 102 and 420 kg/h (your current SI had 68 and 332).",
          "Not changed: the Data and Code Availability paragraph still says the run-all script 'regenerates every number'; the earlier qualified wording (aircraft files plus the ground, lidar and inventory inputs) may be the safer one."]:
    rdoc.add_paragraph(t, style="List Bullet")
rdoc.save(a.response)
print(f"response table -> {a.response} ({len(RESP)} comments)")
