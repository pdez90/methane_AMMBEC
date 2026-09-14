#!/usr/bin/env python3
"""50_manuscript_within_edits.py -- bring the manuscript and SI to the pipeline's
current results (within-leg York fossil fractions, seeded leg-block bootstraps) and
set every changed passage in red.

    python3 scripts/50_manuscript_within_edits.py MAIN_IN.docx MAIN_OUT.docx --si SI_IN.docx SI_OUT.docx \
        --values outputs/paper_values.json

Every edit below is an exact-text replacement (or a whole-paragraph rewrite anchored on an
exact leading fragment). The script refuses to run unless every anchor occurs exactly once
in the document it targets, so it can never half-apply. Numbers that paper_values.json
carries are cross-checked against it before anything is written. The text written here
describes the current method as the method; it does not narrate earlier drafts.

With --figures <OUT_DIR>/figures the regenerated PNGs replace the embedded images whose
content changed with the estimator (Fig 2, 3, 4; Figs S3, S4, S5, S6, S7, S8). The mapping
from docx media file to figure was established by reading each image's caption, and each
swap is refused if the new image's aspect ratio differs from the old by more than 5%,
since the document's picture frame is fixed and would distort it.
"""
import sys, json, copy
from pathlib import Path

try:
    import docx
    from docx.shared import RGBColor
    from docx.oxml.ns import qn
except ImportError:
    sys.exit("python-docx is required:  pip install python-docx")

RED = RGBColor(0xC0, 0x00, 0x00)

# docx media file -> regenerated figure (outputs/figures). Only figures whose content
# depends on the fossil-fraction estimator or the seeded bootstrap are listed.
FIGS = {
    "main": {"image1.png": "Fig2_source_mix.png", "image4.png": "Fig3_ethane_contrast.png",
             "image3.png": "Fig4_source_combined.png"},
    "si":   {"image9.png": "FigS5_beta_sensitivity.png", "image5.png": "FigS7_regression_comparison.png",
             "image1.png": "FigS10_enh_threshold.png", "image6.png": "FigS8_basin_ethane_trend.png",
             "image4.png": "SI_wind_fossil.png", "image8.png": "SI_perflight_maps.png",
             "image2.png": "FigS4_mobile_plumes.png"},
}

# ----------------------------------------------------------------------------------------
# Numbers from the 13 Sep 2026 seeded run (fitted-leg bootstrap, ALL CHECKS PASSED). Keys with a leading T1_
# are Table 1 cells; keep them in one place so a re-run only has to touch this block.
# ----------------------------------------------------------------------------------------
V = {
    "median": "29", "range": "2 to 47", "best_lo": "36", "best_hi": "47",
    "breakeven": "0.059", "best_basin_lo": "40", "best_basin_hi": "47",   # at 0.063-0.073
    "n_majority_best": "two", "qc_max": "3", "wind_r": "0.35",
    "attr_fossil": "2.0", "attr_bio": "5.9", "attr_total": "7.9", "attr_pct": "26",
    "attr_lf": "2.5", "attr_ww": "2.0", "attr_ngd": "0.9",
    "ci_below50": "5", "top_day_lo": "47", "top_day_hi": "76",
    # Table 1 fossil % [CI]  -- FILLED FROM THE RE-RUN
    "T1_0703L1": "29 [26 to 34]", "T1_0703L2": "20 [single leg; no CI]", "T1_0708L1": "2 [0 to 2]",
    "T1_0709": "40 [40 to 43]", "T1_0710L1": "4 [0 to 34]", "T1_0713L1": "29 [19 to 40]",
    "T1_0713L2": "47 [45 to 100]",
    "T1_0703L1_CO2": "4.1 [-0.5 to 4.5]", "T1_0713L1_CO": "10.7 [6.2 to 12.1]",
    "T1_0703L1_CO": "4.6 [1.6 to 5.6]", "T1_0708L1_CO": "8.3 [0.1 to 10.1]", "T1_0709_CO": "6.9 [3.8 to 7.1]",
    "ci_0710": "0 to 34",
    # S4 gate sweep -- FILLED FROM THE RE-RUN (enh_threshold_summary.csv / _sensitivity.csv)
    "gate_lo": "25", "gate_hi": "29", "gate_20": "29", "gate_15": "29", "gate_step": "3", "gate_step_at": "30 and 40",
    "gate_0713": "29 to 28%, never leaving the 27 to 29% band",
    "gate_0703_sentence": "The flight whose value moves most, 10 July L1, falls from about 20 to 1% as its contributing legs drop from six to two between the 5 and 50 ppb gates, and 3 July L1 drifts from about 25 to 19% as its legs fall from eight to two; a flight fitting two legs instead of six is fitting different air, not responding to the threshold.",
    "basin_gate_lo": "0.027", "basin_gate_hi": "0.037", "basin_gate_100": "0.018",   # unchanged: basin rows use the pooled fit
    # Table S6 E from CO2 CI (t/hr), rows 1..11 in table order -- FILLED FROM THE RE-RUN
    "S6_co2_ci": {1: "-0.49 to 4.52", 2: "-3.06 to 5.08", 3: "-4.12 to -0.47", 4: "-8.4 to 8.85", 5: "-0.94 to -0.94",
                  6: "-7.45 to 6.37", 7: "-2.09 to 6.92", 8: "0.24 to 0.24", 9: "n/a", 10: "3.39 to 7.3", 11: "-1.58 to 9.76"},
}

# (anchor text, replacement text, where: "main" | "si", kind: "replace" | "rewrite" | "insert_after")
# "replace": substitute the anchor in place.  "rewrite": replace the WHOLE paragraph whose
# runs start with the anchor (hyperlink citations at the end of the paragraph are kept).
# "insert_after": add a new paragraph after the one containing the anchor.
E = []; TC = []
def R(a, b, doc="main", count=1): E.append((a, b, doc, "replace", count))
def W(a, b, doc="main"): E.append((a, b, doc, "rewrite", 1))
def I(a, b, doc="main"): E.append((a, b, doc, "insert_after", 1))
def T(doc, table, row, col, new, expect=None): TC.append((doc, table, row, col, new, expect))

# ---------------- MAIN TEXT ----------------
# Abstract
R("the median urban fossil fraction is 30 to 39% under a best-estimate 2024 source ratio of 0.063 to 0.0813 mol mol",
  f"the median urban fossil fraction is {V['best_lo']} to {V['best_hi']}% for contemporary Front Range ethane "
  "endmembers of 0.063 to 0.0813 mol mol")
R(", derived from this campaign and conservative against published values, which are all higher. Under the lowest published calibration, 0.102 mol mol",
  f", constrained by independent measurements during the 2024 campaign, and {V['median']}% (per-flight range "
  f"{V['range']}%) for the lowest previously published Front Range calibration, 0.102 mol mol")
R(", the median is 24% with a per-flight range of 0 to 57%. Denver's summertime urban methane is therefore biogenic-dominated in the median under every calibration considered, although three flights reach a fossil majority under the best estimate.",
  ". Denver's observed summertime urban methane enhancements were therefore biogenic-dominated in the median "
  f"under every calibration considered, and remain so unless the endmember falls below {V['breakeven']}; "
  f"{V['n_majority_best']} flights reach a fossil majority under the contemporary endmembers.")
R("Two independent enhancement-ratio methods gave a total urban emission of 4.6 to 10.7 t CH4 per hour (median 7.6) from CH4:CO scaled by gridded CO, and about 4.1 t CH4 per hour from CH4:CO2 scaled by gridded CO2.",
  "Scaling the CH4:CO enhancement ratio by a gridded CO inventory gave 4.6 to 10.7 t CH4 per hour (median 7.6) "
  "on the four flights that pass a reliability screen, and the single flight passing the CH4:CO2 screen gave "
  "4.1 t CH4 per hour; these absolute rates scale with the inventory anchor and are considerably less certain "
  "than the source mix.")
R("Because these enhancement ratios do not require assumptions about boundary-layer height or wind, the approach can be applied across all urban flights",
  "Because these enhancement ratios require no explicit boundary-layer-height or wind term, the approach can be applied across all urban flights")

# 3.2 Methods: the fixed-effects fit, inserted after the York paragraph
I("not biased toward zero when the predictor carries measurement error.",
  "A flight's urban legs sample different air masses, each with its own ethane background, "
  "so we fit the York regression with a separate intercept for each level leg and a common "
  "slope (a fixed-effects fit): each leg's enhancements are centred on that leg's mean before "
  "the York iteration, and each leg contributes in proportion to its methane variance. A "
  "single fit pooled across legs would instead mix this within-leg slope with the slope "
  "through the leg means, which can reverse its sign when legs sample different sources "
  "(section S3). Legs with fewer than ten "
  "in-plume samples are omitted from the fit; a flight in which only one leg clears that "
  "minimum has a point estimate but no leg-block interval, and is marked as such in Table 1.")
R("The fossil fractions are also insensitive to sample selection: re-computing them under the stricter data-selection filters used in Schafer et al. (2025) conducted in LA",
  "The ratio method needs no boundary-layer-height term, but sample selection could still affect whether a "
  "fit represents the urban aggregate rather than individual sources or decoupled air, so we repeated the "
  "apportionment under the stricter data-selection filters of Schafer et al. (2025)")
R(" changes any flight by at most 12 percentage points and preserves the ranking, so the biogenic-dominated result does not depend on which samples are retained (section S3).",
  f"; the largest change on any flight is {V['qc_max']} percentage points and the ranking is preserved (section S3).")
R("Following this evidence, the abstract and the Discussion quote the fossil fractions under the 2024 best estimate of 0.063 to 0.0813 mol mol",
  "The abstract and Discussion quote the fossil fractions for the contemporary Front Range endmembers of 0.063 to 0.0813 mol mol")
R(", while Table 1, Figure 2, and section 4.1 report the more conservative 0.102-based values, which are lower by a single multiplicative factor.",
  " (the inferred 2024 source ratio and the measured ground-level ratio, section S5), while Table 1, Figure 2, "
  "and section 4.1 report the 0.102-based values, which differ by a single multiplicative factor.")
R("0.049 mol", f"{V['breakeven']} mol")
R("campaign median fossil fraction to roughly 34 to 39%",
  f"campaign median fossil fraction to roughly {V['best_basin_lo']} to {V['best_basin_hi']}%")
R("well above the 0.049 break-even", f"well above the {V['breakeven']} break-even")

# 4.1 Results
R("fossil fractions span 0 to 57%, with a median of about 24%",
  f"fossil fractions span {V['range']}%, with a median of {V['median']}%")
R("Under the best-estimate 2024 source ratio these rescale together to a median of 30 to 39%",
  f"For the contemporary endmembers of 0.063 to 0.0813 these rescale together to a median of {V['best_lo']} to {V['best_hi']}%")
R("The 95% leg-block bootstrap intervals are the direct evidence for the source-mix claim: on 4 of the 7 flights, the entire interval lies below 50%, so those flights are predominantly biogenic.",
  "The evidence for the campaign-level source-mix claim is the distribution of flight slopes, its median, and "
  "its insensitivity to the endmember (section 3.2, Figure S3); the leg-block bootstrap intervals characterize "
  f"per-flight uncertainty, and on {V['ci_below50']} of the 7 flights the entire interval lies below 50%.")
R("On the fifth, 3 July L1, the point estimate is low at 21%, but the interval just reaches 50%.",
  "On 3 July L2 only one leg clears the ten-sample minimum, so that flight has a point estimate "
  "(20%) but no leg-block interval.")
W("The clearest exception is 13 July",
  "The most fossil-influenced flights are the second 13 July flight (47%, interval 45 to 100%), "
  "whose upper bound is set by a single high-altitude leg with a steep ethane slope, and 9 July "
  "(40%, interval 40 to 43%). No flight reaches an even fossil-biogenic split at this "
  "calibration. We therefore report that the campaign is biogenic-dominated in the median and "
  "in every flight's point estimate, while noting that the second 13 July flight's interval "
  "(45 to 100%) is wide enough to admit an equal or fossil-dominated mixture. This is, on balance, a less "
  "fossil-influenced regime than reported in previous airborne urban studies in other "
  "metropolitan areas.")
W("The lowest fractions, on 8 and 10 July (near zero), coincide with",
  "The lowest fractions, on 8 and 10 July (2% and 4%), coincide with essentially flat urban "
  "ethane slopes (Figure 3), and on 10 July the reason is visible in the legs. The 235 m leg, "
  "which carries 116 of the flight's 185 samples in the fit, is a single methane plume peaking "
  "at 93 ppb, 3.2 km downwind of the DADS landfill (wind from 328° at 2.5 m/s; the peak lies "
  "within 4° of the downwind bearing), with no ethane enhancement at all (slope 0.0005, "
  "r = 0.04). The two legs near 430 m, over the Commerce City industrial corridor, carry mild "
  "positive slopes of 0.02 to 0.03, which would read 22 to 34% fossil on their own (section "
  f"S3). The flight's leg-block bootstrap interval, {V['ci_0710']}%, spans exactly this "
  "between-leg range, so even its most fossil-leaning leg stays biogenic-dominated. On 8 July "
  "neither leg shows an ethane relationship (r = 0.10 and -0.15). The estimator matters here because the choice is not free at low "
  "correlation: ordinary least squares and York agree closely across the campaign, whereas a "
  "reduced-major-axis fit over-steepens on the low-correlation flights and inflates the "
  "apparent fossil fraction, which is why we do not adopt it (the estimators are compared per "
  "flight in section S3, Figure S4). Absolute fractions remain relative pending an ethane "
  "endmember (Eq. 3), but the biogenic-dominant median is robust to that calibration.")
R("is about 24%; only the two 13 July flights reach an even split or above.",
  f"is {V['median']}%; no flight reaches an even split at this calibration, and the highest, "
  "the second 13 July flight, is 47%.")
R("(York, with 95% leg-block bootstrap interval)",
  "(within-leg York, with 95% leg-block bootstrap interval; a flight in which only one leg "
  "clears the ten-sample minimum has no interval)")
# Table 1 cells
R("21 [19 to 50]", V["T1_0703L1"]); R("24 [20 to 24]", V["T1_0703L2"]); R("0 [0 to 2]", V["T1_0708L1"])
R("42 [40 to 43]", V["T1_0709"]);   R("0 [0 to 35]", V["T1_0710L1"]);   R("50 [18 to 55]", V["T1_0713L1"])
R("57 [46 to 100]", V["T1_0713L2"])
R("4.1 [-0.5 to 4.5]", V["T1_0703L1_CO2"]); R("10.7 [5.9 to 12.2]", V["T1_0713L1_CO"])
R("4.6 [1.6 to 5.6]", V["T1_0703L1_CO"]); R("8.3 [0.1 to 10.1]", V["T1_0708L1_CO"]); R("6.9 [3.8 to 7.1]", V["T1_0709_CO"])

# 4.2
R("varied widely across the campaign (0 to 53%)", f"varied widely across the campaign ({V['range']}%)")
R("Pearson r = 0.47, n = 7", f"Pearson r = {V['wind_r']}, n = 7")
R("The two highest-fossil days (13 July, 50 to 57%) were sampled under persistent north-easterly flow, with 68 to 77% of in-box samples arriving from the north-east.",
  "The highest-fossil flight (the second 13 July flight, 47%) was sampled under persistent "
  "north-easterly flow, with 68% of in-box samples arriving from the north-east, and the next "
  "(9 July, 40%) under northerly flow.")
R("The clearest low-fossil case (3 July L2, 24%)", "The clearest low-fossil case (3 July L2, 20%)")
R("9 July and both 13 July flights, are those already highest here",
  "9 July and the second 13 July flight, are those already highest here")

# 4.3 attribution
R("about 1.8 t/hr fossil and 4.9 t/hr biogenic", f"about {V['attr_fossil']} t/hr fossil and {V['attr_bio']} t/hr biogenic")
R("summing to 6.6 t/hr, about 29% fossil", f"summing to {V['attr_total']} t/hr, about {V['attr_pct']}% fossil")
R("places landfills at about 2.1 t/hr, domestic wastewater at about 1.7",
  f"places landfills at about {V['attr_lf']} t/hr, domestic wastewater at about {V['attr_ww']}")
R("natural-gas distribution at about 0.8 t/hr", f"natural-gas distribution at about {V['attr_ngd']} t/hr")
R("wastewater estimate (about 1.7", f"wastewater estimate (about {V['attr_ww']}")
R("top-down fossil fraction near 27%", f"top-down fossil fraction near {V['attr_pct']}%")
# Fig 4B caption: the leverage gate
R("at least 12 in-plume boundary-layer samples (below about 1.5 km)",
  "at least 12 in-plume boundary-layer samples (below about 1.5 km), spanning at least 50 ppb of "
  "methane enhancement so that a local slope can be fitted (26 of the 106 sampled cells meet both "
  "conditions),")

# 5 Discussion
R("Under the best-estimate 2024 source ratio of 0.063 to 0.0813 mol mol",
  "For contemporary Front Range endmembers of 0.063 to 0.0813 mol mol")
R("median fossil share across the two-week campaign is 30 to 39%, a best estimate that is conservative in the context of the published calibrations, which are all higher. Under the lowest published value (0.102 mol mol",
  f"median fossil share across the two-week campaign is {V['best_lo']} to {V['best_hi']}%; for the lowest "
  "previously published calibration (0.102 mol mol")
R("the median is about 24% with a range of 0 to 57%", f"the median is {V['median']}% with a range of {V['range']}%")
R("Three flights reach or cross the halfway line under the best estimate (9 July and both 13 July flights); under the 0.102 calibration only the two 13 July flights do.",
  "Two flights cross the halfway line under the contemporary endmembers (9 July and the second 13 July "
  f"flight); under the 0.102 calibration none does, the highest being 47%. The median stays below 50% unless "
  f"the endmember falls below {V['breakeven']} mol mol-1 (Figure S3).")
R("median fossil fraction (30 to 39% under the best estimate, about 24%",
  f"median fossil fraction ({V['best_lo']} to {V['best_hi']}% for the contemporary endmembers, {V['median']}%")
R("most fossil-influenced day (57 to 71% across these calibrations)",
  f"most fossil-influenced flight ({V['top_day_lo']} to {V['top_day_hi']}% across these calibrations)")
R("above the 30 to 39% best-estimate median", f"above the {V['best_lo']} to {V['best_hi']}% median for the contemporary endmembers")
R("roughly two and a half times the 24%", f"about twice the {V['median']}%")

# ---------------- SI ----------------
R("the largest change is 12 percentage points, on the 13 July", f"the largest change is {V['qc_max']} percentage points, on the 13 July", "si")
R("0.049 mol", f"{V['breakeven']} mol", "si", count=2)
R("value of 0.049", f"value of {V['breakeven']}", "si")
R("roughly 34 to 39% while leaving", f"roughly {V['best_basin_lo']} to {V['best_basin_hi']}% while leaving", "si")
R("The whole-flight leg-block bootstrap interval of 0-35%", f"The whole-flight leg-block bootstrap interval of {V['ci_0710']}%", "si")
I("corresponding to fossil fractions of 34% and 22% when fitted separately.",
  "The 235 m leg is a single plume peaking at 93 ppb of methane enhancement 3.2 km from the "
  "DADS landfill; the wind at the time was from 328° at 2.5 m/s and the peak lies within 4° "
  "of the downwind bearing from the landfill, so this leg sampled landfill methane, which is "
  "consistent with its absence of ethane.", "si")
R("least squares (OLS), reduced major axis (RMA), and the instrument-weighted York fit  are compared",
  "least squares (OLS), reduced major axis (RMA), and the instrument-weighted York fit, the latter "
  "both pooled across legs and with the per-leg intercepts adopted in the main text, are compared", "si")
R("For the ethane fossil fraction, OLS and York agree closely, so the biogenic-dominated result does not depend on that choice;",
  "For the ethane fossil fraction, OLS and pooled York agree closely, and the within-leg fit "
  "lowers the flights whose pooled slope is dominated by between-leg contrast (13 July L1) and "
  "raises the two whose pooled slope is negative (8 and 10 July), so the biogenic-dominated "
  "result does not depend on that choice;", "si")
R("Left: the ethane:CH4 fossil fraction under OLS, RMA, and York. OLS and York track each other;",
  "Left: the ethane:CH4 fossil fraction under OLS, RMA, pooled York and the within-leg York used "
  "in the main text. OLS and pooled York track each other;", "si")
R("The main text uses York for the ethane fossil fraction (where it agrees with OLS)",
  "The main text uses the within-leg York fit for the ethane fossil fraction", "si")
R("York fossil fraction versus the fraction of in-box", "Fossil fraction versus the fraction of in-box", "si")
R("(Pearson r = 0.46, n = 7)", f"(Pearson r = {V['wind_r']}, n = 7)", "si")
R("The highest-fossil day (13 July) shows the largest enhancements, sampled under north-easterly flow from the Wattenberg/DJB field.",
  "The first 13 July flight shows the largest enhancements of the campaign; the highest-fossil flight, "
  "the second 13 July flight, was sampled under the same north-easterly flow from the Wattenberg/DJB "
  "field but with smaller enhancements.", "si")
R("OLS fits the emission slopes; the York fossil fraction uses instrument-weighted York.",
  "OLS fits the emission slopes; the fossil fraction uses the within-leg York fit of section 3.2.", "si")
R("Table S3: Per-flight York fossil fraction", "Table S3: Per-flight fossil fraction", "si")
R("We re-computed each flight's York fossil fraction", "We re-computed each flight's fossil fraction", "si")

# S4 gate sweep (fixed 4-flight panel; values from enh_threshold_summary.csv, seeded run)
R("the median fossil fraction runs from 21 to 40%, and is 31% at the adopted 20 ppb gate",
  f"the median fossil fraction runs from {V['gate_lo']} to {V['gate_hi']}%, and is {V['gate_20']}% at the adopted 20 ppb gate", "si")
R("The largest step between adjacent thresholds is 9 percentage points, between 15 and 20 ppb.",
  f"The largest step between adjacent thresholds is {V['gate_step']} percentage points, between {V['gate_step_at']} ppb.", "si")
R("is between zero and two.", "is zero.", "si")
R("holds all eleven legs from 5 to 40 ppb and drifts smoothly from 48 to 55%",
  f"holds all eleven legs from 5 to 40 ppb and drifts smoothly from {V['gate_0713']}", "si")
R("The flight that drives the 9-point step loses legs at exactly that point: on 3 July the first flight falls from 39 to 21% as its contributing legs drop from five to three between the 15 and 20 ppb gates.",
  V["gate_0703_sentence"], "si")
R("And the 24% campaign median", f"And the {V['median']}% campaign median", "si")
R("threshold yields 31%, and a 15 ppb gate would yield about 40%",
  f"threshold also yields {V['gate_20']}%, as does a 15 ppb gate", "si")
R("the basin median lies between 0.027 and 0.037", f"the basin median lies between {V['basin_gate_lo']} and {V['basin_gate_hi']}", "si")
R("Only the 100 ppb gate departs, at 0.018", f"Only the 100 ppb gate departs, at {V['basin_gate_100']}", "si")

# SI tables. Table index: 2 = Table S3 (QC), 3 = Table S4 (basin CIs), 4 = Table S5, 5 = Table S6.
S3 = {1: ("29", "29", "0"), 2: ("20", "20", "0"), 4: ("2", "2", "0"), 6: ("40", "40", "0"),
      7: ("4", "4", "0"), 10: ("29", "29", "0"), 11: ("47", "44", "3")}
for r, (a, b, c) in S3.items():
    T("si", 2, r, 1, a); T("si", 2, r, 2, b); T("si", 2, r, 3, c)
S5 = {1: "29", 2: "20", 4: "2", 6: "40", 7: "4", 10: "29", 11: "47"}
for r, v in S5.items(): T("si", 4, r, 6, v)
# Table S4 95% CI column (seeded run of script 31), rows in the table's own order
S4 = {1: "0.0501 to 0.1045", 2: "0.0228 to 0.1828", 3: "0.0545 to 0.0545", 4: "0.0243 to 0.0694",
      5: "0.0173 to 0.0656", 6: "0.0307 to 0.0374", 7: "0.0102 to 0.0499", 8: "0.0155 to 0.0308",
      9: "0.0064 to 0.0436", 10: "-0.0033 to 0.0409", 11: "-0.0164 to 0.2203", 12: "-0.0970 to 0.0017",
      13: "-0.0681 to 0.8646", 14: "-0.0855 to 0.0002", 15: "-0.4169 to -0.4169"}
for r, v in S4.items(): T("si", 3, r, 5, v)
# Table S6 E-from-CO2 95% CI column (seeded run of script 15): filled from ratio_method_flux.csv
for r, v in V["S6_co2_ci"].items(): T("si", 5, r, 4, v)


# ---------------- AUDIT CORRECTIONS (13 Sep 2026 reproducibility audit) ----------------
# Main text
R("Both exceed two box-consistent bottom-up inventories two- to four-fold.",
  "The CH4:CO median exceeds two box-consistent bottom-up inventories (1.7 and 2.6 t CH4 per "
  "hour) by a factor of about 3 to 4.5.")
R("The two independent bottom-up inventories therefore corroborate a level near 1.7 to 2.6 t/hr. Our airborne top-down estimates (the two enhancement-ratio methods, about 4 to 11 t/hr)",
  "Both bottom-up inventories therefore place Denver near 2 t/hr (1.7 to 2.6). Our airborne top-down "
  "estimates (the enhancement-ratio estimates, about 4 to 11 t/hr)")
R("inventories by roughly a factor of 2 to 4.",
  "inventories by factors of about 1.6 to 6 across flights and anchors, and by a factor of "
  "about 3 to 4.5 at the CH4:CO median of 7.6 t/hr.")
R("the biogenic sectors are roughly three times larger",
  "the biogenic sectors are roughly six times larger")
R("and these days also carry the largest methane enhancements (Figure 3; per-flight",
  "and the first 13 July flight, sampled under the same north-easterly flow, carries the "
  "largest methane enhancements of the campaign (per-flight")
R("fossil fraction from about 24 to about 30% and moves three of seven flights above the halfway line rather than two",
  "fossil fraction from 29 to 36% and brings two of seven flights to or above the halfway "
  "line (9 July at 50%, the second 13 July flight at 59%) rather than none")
R("part of the compass around the city, Figure S3)", "part of the compass around the city, Figure S1)")
R("The headline fractions in the abstract and Discussion use the campaign-derived 2024 best estimate of 0.063 to 0.0813 mol mol",
  "The headline fractions in the abstract and Discussion use contemporary Front Range endmembers of 0.063 to 0.0813 mol mol")
R(" (section S5). Table 1, Figure 2, and section 4.1 retain",
  ": an inferred 2024 source ratio and a measured ambient ratio, neither a direct measurement of Denver "
  "distribution gas (section S5). Table 1, Figure 2, and section 4.1 retain")
# SI
R("and one flight sampled only a single altitude", "and several sampled only two altitude levels", "si")
R("or sample only a single altitude, because", "or sample only two altitude levels, because", "si")
R("(iv) vertically stacked legs", "(iii) vertically stacked legs", "si")
R("and (v) winds transport", "and (iv) winds transport", "si")
R("28-42 km away, were sampled at only one or two altitude levels.",
  "28 to 42 km away, failed either on vertical coverage (the single-level screen 28 km south on "
  "5 July) or on transport (0.8 to 2.3 m/s of wind through the screens 42 km east).", "si")
R("with enhancements up to about 19 ppmv", "with enhancements up to about 13 ppmv", "si")
R("(slopes about 0.008 ppmv per year in magnitude, p = 0.31 to 0.47)",
  "(slopes of 0.005 to 0.008 ppmv per year in magnitude, p = 0.42 to 0.47)", "si")
R("under the same three estimators", "under the three pooled estimators", "si")
R(" A flight fitting three legs instead of five is fitting different air, not responding to the threshold.", "", "si")
R("the two most sparsely sampled flights, each with only two contributing legs",
  "two flights with only two contributing legs", "si")
R("agree to within 13%", "agree to within 14%", "si")


# ---------------- SECOND REVIEW (13 Sep 2026, wording and method accuracy) ----------------
R("Across 8 flights with adequate urban coverage, the median urban fossil fraction is",
  "Across 8 flights with adequate urban coverage (7 with an ethane fit), the median urban fossil fraction is")
W("We retain York as the primary estimator",
  "We retain the within-leg York fit as the primary estimator. As a check on estimator choice, we also "
  "compute the slope pooled across legs by York, by ordinary least squares and by a reduced-major-axis fit; "
  "the estimators are compared per flight in section S3 (Figure S4), which also explains why the "
  "reduced-major-axis fit, which inflates the slope at low correlation, is not adopted.")
R("whole-level legs are resampled with replacement (B = 2000) and the 2.5th",
  "the level legs entering the fit are resampled with replacement (B = 2000), the within-leg fit "
  "is repeated on each resample, and the 2.5th")
R("We applied the instrument-weighted York regression to each flight",
  "We applied the within-leg York regression of section 3.2 to each flight")
R("The per-flight mean boundary-layer wind direction over the urban box, computed from the aircraft winds, is associated with the fossil fraction",
  "The fraction of in-box boundary-layer samples (below 1.6 km above ground) arriving from the "
  "north-east quadrant, computed from the aircraft winds, is associated with the fossil fraction")
R("modest positive slopes of 0.031 and 0.022", "modest positive slopes of 0.035 and 0.023", "si")
R("The dashed gray line rises steeply above 50 ppb", "The dashed gray line rises above 50 ppb", "si")
R("applied the identical background, enhancement, York-fit, and leg-block bootstrap machinery used for the urban ratio",
  "applied the identical background, enhancement and leg-block bootstrap machinery used for the "
  "urban ratio, with a single York fit pooled across each flight\'s basin legs rather than the "
  "per-leg-intercept fit of section 3.2", "si")

# ---------------- THIRD REVIEW (13 Sep 2026, accuracy and typos) ----------------
# Main text
R("the the SI", "the SI")
R("(2025):;", "(2025):")
R(", which run from 0.102 to 0.187 mol mol-1,", ",")            # duplicated range
R("fractions reported are 58", "fractions are 58")
R("fall 2023. so", "fall 2023, so")
# Box area: the geodesic box is about 2,780 km2 (vulcan_co2_boxsum.csv box_cells_expected);
# 2,856 is the number of 1-km Vulcan cells that intersect it. Make the two statements consistent.
R("over 2,856 km", "over the 2,856 1-km cells that intersect the box, 2,856 km")
R("roughly 2,750 km", "roughly 2,800 km")
R("4.3 times the analysis box", "4.2 times the analysis box")
R("than 4.3-fold", "than fourfold")
# SI
# Seven-county footprint from the committed Census outline (scripts/51_footprint_extents.R):
# 11,734 km2 against a 2,782 km2 box (ratio 4.22); 39.28 N is the 3 July L1 track minimum.
R("11,800 km", "11,700 km", count=3)
R("reaching as far south as 39.28°N", "with flight tracks reaching as far south as 39.28°N (3 July L1)")
R("emission now also carries", "emission also carries", "si")
R("The scatter of the rolling background had never been quantified. We estimate it per flight as",
  "We estimate the scatter of the rolling background per flight as", "si")

# ---------------- FRAMING REVISION (13 Sep 2026: hierarchy of results, endmember wording, ---------
# ---------------- "enhancements" not "emissions", ratio-method language, shorter Methods) ---------
# Main text, 3.2
W("The fit uses points with ΔCH4 > 20 ppb",
  "The fit uses points with ΔCH4 > 20 ppb, well above both the 1 ppb instrument precision and the scatter "
  "of the rolling background (median 4.3 ppb across flights), so the slope reflects plumes rather than "
  "baseline scatter. The biogenic-dominated conclusion holds for thresholds from 5 to 50 ppb, although the "
  "percentage itself moves with the threshold, as it does with the endmember (sections S3 and S4).")
R("yield the 95% confidence interval, propagating", "yield a 95% leg-block bootstrap interval, propagating")
W("We deliberately adopt the lowest published",
  "We adopt the lowest published value because the endmember enters as a divisor (Eq. 3), so it yields the "
  "highest fossil fraction and is conservative with respect to a biogenic-dominated conclusion; the resulting "
  "fractions are relative estimates that depend on this endmember.")
R("The biogenic-dominated median is therefore robust to the endmember over the whole plausible range.",
  "The biogenic-dominated median is therefore robust to the endmember over the whole plausible range. We "
  "have no direct measurement of the ethane-to-methane ratio of Denver distribution gas, which is processed "
  "and may be more ethane-depleted than production gas, and which the gridded inventory makes about 70% of "
  "the box's fossil methane; a lower distribution-gas endmember would raise every fossil fraction "
  f"proportionally, and the median would exceed 50% only if that endmember fell below {V['breakeven']}, "
  "which is below the contemporary range of 0.063 to 0.0813 constrained during the campaign (section S5).")
R("AMMBEC flights also constrain the present-day value directly. The DJB basin legs yield a 2024 ethane-to-methane enhancement ratio of 0.036 mol mol",
  "The 2024 campaign also constrains the contemporary value, though only indirectly: the DJB basin legs "
  "yield an ambient ethane-to-methane enhancement ratio of 0.036 mol mol")
R("this corresponds to an oil-and-gas source ratio of 0.063-0.073 mol mol",
  "this implies an oil-and-gas source ratio of 0.063 to 0.073 mol mol")
# 3.3: legs as sampling units, not flux-integral levels
W("Straight, level legs are continuous flight segments",
  "Straight, level legs, continuous flight segments of approximately constant heading and altitude, are the "
  "sampling units of the analysis. Each samples one air mass at one level, so legs serve as the blocks of "
  "the bootstrap and, for the ethane fit, carry their own intercepts (section 3.2). Excluding turns avoids "
  "banking and altitude changes that mix sampling heights and blur plume structure. Their role in a "
  "mass-balance flux integral, which this campaign's geometry does not support, is described in section S1.")
# 3.4: secondary status and the ratio-method assumption
R("Because none of the AMMBEC flights encircle the metropolitan area, a closed-loop (box) mass balance is not applicable. We instead adopt",
  "The absolute emission rate is a secondary and more assumption-dependent result than the source mix, "
  "because it scales with an inventory anchor. Because none of the AMMBEC flights encircle the metropolitan "
  "area, a closed-loop (box) mass balance is not applicable; we instead adopt")
R("Because CO, CO2, and CH4 are co-transported through the same boundary layer, their ratio is independent of mixing height and wind speed, so the method sidesteps the two largest mass-balance uncertainties and applies to every urban flight. We adopt this ratio method but not data-selection filters used in previous research,",
  "For co-sampled enhancements that have experienced sufficiently similar transport and dilution, the CH4:X "
  "enhancement ratio is far less sensitive to mixing height and wind speed than a mass-balance flux, so the "
  "method needs no explicit boundary-layer-height or wind term and can be applied to every urban flight; "
  "where the methane and anchor sources are spatially offset, as landfills and wastewater are from traffic "
  "CO, that assumption is imperfect (section 5). We adopt this ratio method but not the data-selection "
  "filters used in previous research,")
R(" because the ratio is already independent of mixing height.", " because the ratio does not require a mixing-height term.")
R("carries a 95% confidence interval from a leg-block bootstrap of the slope (whole-level legs resampled with replacement, B = 2000).",
  "carries a 95% leg-block bootstrap interval (whole-level legs resampled with replacement, B = 2000); that "
  "interval captures plume-to-plume scatter in the measured ratio, not the uncertainty of the inventory "
  "anchor (section 5).")
R("We anchor the ratio with two independent, box-consistent gridded inventories.",
  "We anchor the ratio with two box-consistent gridded inventories.")
# 4.1 / 4.2 / 4.3
R("with 95% leg-block bootstrap confidence intervals", "with 95% leg-block bootstrap intervals")
R("4.2. Urban methane emission estimates from two independent enhancement-ratio methods",
  "4.2. Urban methane emission estimates from CH4:CO and CH4:CO2 enhancement ratios")
W("We anchor the enhancement ratio in two independent ways",
  "We anchor the ordinary-least-squares emission slopes (section 3.4) with two inventories, both summed over "
  "the identical box. Anchoring the CH4:CO slope to gridded GRA2PES CO (121.5 Gg CO yr-1) gives 4.6 to "
  "10.7 t/hr (median 7.6) on the four flights that pass the reliability screen (r ≥ 0.7). Anchoring the "
  "CH4:CO2 slope to Vulcan fossil-fuel CO2 (23,622 Gg CO2 yr-1) gives 4.1 t/hr on the single flight that "
  "passes the CH4:CO2 screen. The two anchors are box-consistent, each computed over the identical "
  "Denver-metro box, and use different tracer species and different inventories, so an error in one "
  "inventory does not affect the other; with one usable CH4:CO2 flight, however, their agreement is a "
  "consistency check rather than an independent replication of the range.")
R("Together, the two enhancement-ratio methods constrain urban methane emissions at approximately 4-11 t CH4 h-1, with a median of 7.6 t CH4 h-1.",
  "Taken together, the four CH4:CO estimates and the single CH4:CO2 estimate yield urban emission-rate "
  "estimates of roughly 4 to 11 t CH4/hr (CH4:CO median 7.6). These absolute rates are considerably "
  "less certain than the fossil fraction: each scales linearly with its inventory anchor, the anchors date "
  "from 2020 to 2023 rather than 2024, and the bootstrap intervals in Table 1 capture only leg-to-leg "
  "scatter, not anchor uncertainty (section 5).")
R("reach a fossil majority under the 2024 best estimate,", "reach a fossil majority under the contemporary endmembers,")
R("under the 2024 best estimate the fossil share", "under the contemporary endmembers the fossil share")
# 5 Discussion
R("Mitigation targeting only the natural-gas distribution system would thus address a minority of Denver's urban methane, so landfills, wastewater, and other biogenic sources warrant at least equal attention.",
  "These summertime observations indicate that mitigation focused solely on the natural-gas system would "
  "miss a substantial, and during this campaign apparently majority, component of the observed urban "
  "methane signal, highlighting landfills and wastewater as complementary mitigation targets.")
R("Because the enhancement-ratio methods require no enclosing flight geometry, no wind field, and no mixing height,",
  "Because the enhancement-ratio methods require no enclosing flight geometry and no explicit wind or "
  "mixing-height term,")
R("whereas denser flight coverage would not.", "whereas denser flight coverage would not by itself reduce the anchor uncertainty.")
R("The airborne evidence here, that Denver's urban methane is biogenic-dominated, aligns with that shift, and it argues for directing measurement and mitigation toward the landfill and wastewater sources that this study finds dominate the urban signal.",
  "The airborne evidence here, that Denver's observed summertime urban methane enhancements are "
  "biogenic-dominated, aligns with that shift, and it argues for directing measurement and mitigation toward "
  "the landfill and wastewater sources that likely contribute substantially to the urban signal.")
R("and two independent enhancement-ratio methods bound the urban emission at roughly 4 to 11 t/hr, above box-consistent bottom-up inventories.",
  "and enhancement-ratio estimates anchored to two inventories yield urban emission-rate estimates of roughly "
  "4 to 11 t CH4/hr, above box-consistent bottom-up inventories; that absolute rate is considerably less certain "
  "than the source mix, because it scales with the inventory anchor.")
R("the methane is disproportionately biogenic, so mitigation aimed only at the natural-gas system would miss most of the city's urban methane.",
  "the sampled methane is disproportionately biogenic, so mitigation aimed only at the natural-gas system "
  "would miss much of the summertime urban methane signal.")
# SI
R("which need no closed loop, wind, or mixing height", "which need no closed loop and no explicit wind or mixing-height term", "si")
R("The enhancement ratio is boundary-layer-insensitive by construction, because the anchor species and methane are co-transported through the same air, so the mixing-height and altitude cuts are not needed for the ratio to be meaningful;",
  "The enhancement ratio does not require an explicit boundary-layer-height term, so the mixing-height and "
  "altitude cuts are not needed for the ratio to be defined;", "si")
W("Samples from outside the box, above the mixing height",
  "The ratio method does not mathematically require a boundary-layer height. Sample selection could "
  "nevertheless affect whether the observations represent an urban aggregate rather than individual sources "
  "or decoupled air masses, which is why we repeated the source apportionment under these filters as a "
  f"sensitivity analysis: the maximum change is {V['qc_max']} percentage points, and the filters are not "
  "imposed on the primary fits or on the enhancement-ratio emission estimates.", "si")
I("A fourth choice, the methane enhancement threshold that defines a plume, is assessed in section S4.",
  "Two points from this comparison are worth spelling out. First, why the per-leg-intercept (within-leg) "
  "fit is the primary estimator: a flight's urban legs sample different air masses, each with its own "
  "ethane background, and a single fit pooled across legs blends the within-leg slope with the slope "
  "through the leg means, weighted by the between-leg spread of methane. Where one leg samples a landfill "
  "plume and another the industrial corridor, that between-leg term is negative and can reverse the sign "
  "of the pooled slope even though every leg's own slope is positive (Simpson's paradox); the within-leg "
  "slope carries no such term and needs no clamping at zero, which is why 8 and 10 July move from 0% "
  "(pooled) to 2 and 4% (within-leg). Second, why the reduced-major-axis fit is not adopted: the RMA slope "
  "minimizes the product of the vertical and horizontal residuals and equals the ratio of the two "
  "enhancements' standard deviations, which is the ordinary-least-squares slope divided by the absolute "
  "correlation coefficient; that division steepens the slope wherever the ethane-methane correlation is "
  "weak, which over the city is precisely the biogenic signal, so RMA inflates the apparent fossil "
  "fraction on the low-correlation flights. Finally, we do not use the largest observed atmospheric "
  "ethane-to-methane slope as a lower bound on the endmember, because airborne slopes reflect diluted and "
  "potentially mixed plumes rather than undiluted source gas; doing so would by construction assign that "
  "flight a fossil fraction of 100%.", "si")

# ---------------- FINAL EDITS (13 Sep 2026, second round of the critique) ----------------
R("A distribution-weighted endmember would fall below the range swept in Figure S3 and would raise the fossil fractions proportionally",
  "A distribution-weighted endmember would likely fall below the production-gas values considered above and "
  "would raise the fossil fractions proportionally")
R("and the box-consistent Vulcan fossil-fuel CO2 anchor gives about 4.1 t/hr near the low end of that range; the two agree despite using different tracer species and independent inventories, which is the reassurance a box-consistent anchor is meant to provide.",
  "and the single usable CH4:CO2 estimate, anchored to box-consistent Vulcan fossil-fuel CO2 (4.1 t/hr), "
  "lies near the lower end of that range despite relying on a different tracer and inventory, which is the "
  "reassurance a box-consistent anchor is meant to provide.")
R("It is used to document the daytime boundary layer and to apply the in-boundary-layer data filter, not to compute a flux.",
  "It is used to document the daytime boundary layer and in the sensitivity analysis that applies an "
  "in-boundary-layer filter (section S3), not to compute a flux.")
R("split the budget into fossil and biogenic", "split the airborne-derived total into fossil and biogenic")
R("The observed budget is larger and less fossil than the inventory", "The airborne-derived estimate is larger and less fossil than the inventory")
# SI
R("Here, they simply confirm ongoing, non-trending facility emissions.",
  "Here, they simply document persistent facility-associated methane plumes, with no detectable trend in the "
  "route-specific enhancement metric.", "si")
R(" despite proximity to a major oil-and-gas basin", "", "si")   # SI title now matches the main title

# ---------------- FINAL POLISH (13 Sep 2026: unit and range consistency) ----------------
# Body text uses "t/hr" (t CH4/hr where the species is named); ranges in prose use "to";
# the abstract keeps the spelled-out "t CH4 per hour". Superscript "-1" runs are normalised
# to a true minus sign by minus_fix() below, and exponents in red text by exponent_fix().
R("yields 11.1-25.8 t CH4 h-1, which", "yields 11.1 to 25.8 t CH4/hr, which")
R("equivalent to ~0.6 t CH4 h-1)", "equivalent to ~0.6 t CH4/hr)")
R("(121.5 Gg/yr, summed over the identical Denver-metro box)", "(121.5 Gg CO yr-1, summed over the identical Denver-metro box)")
R("CO total (292.7 Gg/yr)", "CO total (292.7 Gg CO yr-1)")
R("(39.52-40.03°N)", "(39.52 to 40.03°N)")
R("values of 0.04-0.16 mol mol", "values of 0.04 to 0.16 mol mol")
R("Kille et al. (0.102-0.187 mol mol", "Kille et al. (0.102 to 0.187 mol mol")
R("t CH4 h-1", "t CH4/hr", "si", count=3)
R("(1070-1151 m above ground)", "(1070 to 1151 m above ground)", "si")
R("spanning 371-2503 m above ground", "spanning 371 to 2503 m above ground", "si")
R("(237-2397 m above ground)", "(237 to 2397 m above ground)", "si")
R("from 0.04 to 0.16 mol mol", "from 0.04 to 0.16 mol mol", "si")

# ----------------------------------------------------------------------------------------
def runs_text(p): return "".join(r.text for r in p.runs)

def all_paragraphs(d):
    ps = list(d.paragraphs)
    for tb in d.tables:
        for row in tb.rows:
            for cell in row.cells:
                ps.extend(cell.paragraphs)
    return ps

def replace_in_paragraph(p, old, new, start=0):
    """Replace the first occurrence of `old` at or after character offset `start`.
    Returns the offset just past the inserted text, or -1 if not found."""
    from docx.text.run import Run
    runs = p.runs; full = runs_text(p); i = full.find(old, start)
    if i < 0: return -1
    spans, pos = [], 0
    for ri, r in enumerate(runs):
        spans.append((pos, pos + len(r.text), ri)); pos += len(r.text)
    start_ri = next(ri for s_, e, ri in spans if s_ <= i < e)
    end_ri = next(ri for s_, e, ri in spans if s_ < i + len(old) <= e)
    s0 = spans[start_ri][0]; head = runs[start_ri].text[: i - s0]
    if start_ri == end_ri:
        tail = runs[start_ri].text[i - s0 + len(old):]
    else:
        tail = ""
        remaining = len(old) - (spans[start_ri][1] - i)
        for ri in range(start_ri + 1, end_ri + 1):
            t = runs[ri].text; take = min(len(t), remaining); runs[ri].text = t[take:]; remaining -= take
            if remaining <= 0: break
    # head | new (red, same formatting) | tail
    runs[start_ri].text = head
    newrun = copy.deepcopy(runs[start_ri]._r); runs[start_ri]._r.addnext(newrun)
    nr = Run(newrun, p); nr.text = new; nr.font.color.rgb = RED
    if tail:
        tailrun = copy.deepcopy(runs[start_ri]._r); newrun.addnext(tailrun)
        tr = Run(tailrun, p); tr.text = tail; tr.font.color.rgb = None
    return i + len(new)

def rewrite_paragraph(p, new):
    runs = p.runs
    for r in runs[1:]: r._r.getparent().remove(r._r)
    runs[0].text = new; runs[0].font.color.rgb = RED

def insert_after(p, text):
    newp = copy.deepcopy(p._p)
    for child in list(newp):
        if child.tag != qn("w:pPr"): newp.remove(child)
    p._p.addnext(newp)
    from docx.text.paragraph import Paragraph
    np_ = Paragraph(newp, p._parent)
    r = np_.add_run(text); r.font.color.rgb = RED
    return np_

import re
CHEM = re.compile(r"(CH4|C2H6|CO2|NO2|H2O)")
def subscript_chem(paras):
    """Split red-marked runs so the digits of CH4 / C2H6 / CO2 are true subscripts, matching
    the rest of the document, which writes them as separate subscript runs."""
    from docx.text.run import Run
    n = 0
    for p in paras:
        for r in list(p.runs):
            try:
                if r.font.color.rgb != RED or not CHEM.search(r.text): continue
            except Exception:
                continue
            pieces = []
            for tok in CHEM.split(r.text):
                if not tok: continue
                if CHEM.fullmatch(tok):
                    for ch in tok: pieces.append((ch, ch.isdigit()))
                else: pieces.append((tok, False))
            prev = r._r
            for txt, sub_ in pieces:
                el = copy.deepcopy(r._r); prev.addnext(el); prev = el
                nr = Run(el, p); nr.text = txt
                if sub_: nr.font.subscript = True
            r._r.getparent().remove(r._r); n += 1
    return n

EXP = re.compile(r"(?:(?<=\byr)|(?<=\bmol)|(?<=\bkm)|(?<=\bh)|(?<=\bs)|(?<=\bm))(-1|−1|-2|−2)(?![\d])|(?<=\bkm)(2)(?![\d,.])")
def exponent_fix(paras):
    """In red runs, make unit exponents true superscripts with a minus sign (yr-1 -> yr^−1, km2 -> km^2)."""
    from docx.text.run import Run
    n = 0
    for p in paras:
        for r in list(p.runs):
            try:
                if r.font.color.rgb != RED or r.font.superscript or not EXP.search(r.text): continue
            except Exception:
                continue
            pieces, last = [], 0
            for m in EXP.finditer(r.text):
                pieces.append((r.text[last:m.start()], False)); pieces.append((m.group(0).replace("-", "\u2212"), True)); last = m.end()
            pieces.append((r.text[last:], False))
            prev = r._r
            for txt, sup in pieces:
                if not txt: continue
                el = copy.deepcopy(r._r); prev.addnext(el); prev = el
                nr = Run(el, p); nr.text = txt
                if sup: nr.font.superscript = True
            r._r.getparent().remove(r._r); n += 1
    return n

def minus_fix(paras):
    """Superscript exponents typed with a hyphen ("-1") become a true minus ("−1"), in red."""
    n = 0
    for p in paras:
        for r in p.runs:
            if r.font.superscript and re.fullmatch(r"-\d+\s?", r.text or ""):
                r.text = r.text.replace("-", "\u2212"); r.font.color.rgb = RED; n += 1
    return n

def apply(doc_path, out_path, edits, cells, label):
    d = docx.Document(str(doc_path)); paras = all_paragraphs(d)
    whole = "\n".join(runs_text(p) for p in paras)
    bad = [(a, c) for a, _, _, _, c in edits if whole.count(a) != c]
    if bad:
        sys.exit(f"{label}: these anchors do not occur the expected number of times; nothing written:\n  " +
                 "\n  ".join(f"found {whole.count(a)}x, expected {c}x  {a[:80]!r}" for a, c in bad))
    n = 0
    for a, b, _, kind, c in edits:
        if a == b: continue
        hits = [p for p in paras if a in runs_text(p)]
        for p in hits:
            if kind == "replace":
                pos = 0
                while True:
                    pos = replace_in_paragraph(p, a, b, pos)
                    if pos < 0: break
                    n += 1
            elif kind == "rewrite": rewrite_paragraph(p, b); n += 1
            elif kind == "insert_after": insert_after(p, b); n += 1
    for _, ti, ri, ci, new, expect in cells:
        cell = d.tables[ti].rows[ri].cells[ci]; cp = cell.paragraphs[0]
        old = runs_text(cp)
        if expect is not None and old.strip() != expect:
            sys.exit(f"{label}: table {ti} row {ri} col {ci} holds {old!r}, expected {expect!r}; nothing written")
        if old.strip() == new.strip(): continue
        rewrite_paragraph(cp, new); n += 1
    after = "\n".join(runs_text(p) for p in all_paragraphs(d))
    missing = [b for a, b, _, _, _ in edits if b and a != b and b not in after]
    if missing:
        sys.exit(f"{label}: {len(missing)} replacement(s) did not land (an earlier edit consumed the anchor?):\n  " +
                 "\n  ".join(repr(b[:90]) for b in missing))
    m = subscript_chem(all_paragraphs(d)); e = exponent_fix(all_paragraphs(d)); k = minus_fix(all_paragraphs(d))
    d.save(str(out_path)); print(f"{label}: {n} edit(s) written to {out_path} (all in red; {m} run(s) given chemical subscripts, "
                                 f"{e} run(s) given unit exponents, {k} superscript hyphen(s) changed to minus signs)")

def swap_figures(docx_path, figdir, mapping, label):
    import zipfile, shutil, io, struct
    def png_size(b):
        assert b[:8] == b"\x89PNG\r\n\x1a\n", "not a PNG"
        w, h = struct.unpack(">II", b[16:24]); return w, h
    tmp = Path(str(docx_path) + ".tmp")
    n = 0
    with zipfile.ZipFile(docx_path) as zin, zipfile.ZipFile(tmp, "w", zipfile.ZIP_DEFLATED) as zout:
        for item in zin.infolist():
            data = zin.read(item.filename)
            name = item.filename.split("/")[-1]
            if item.filename.startswith("word/media/") and name in mapping:
                src = Path(figdir) / mapping[name]
                if not src.exists(): sys.exit(f"{label}: figure {src} not found")
                new = src.read_bytes()
                ow, oh = png_size(data); nw, nh = png_size(new)
                if abs((ow / oh) / (nw / nh) - 1) > 0.05:
                    sys.exit(f"{label}: {name} ({ow}x{oh}) vs {src.name} ({nw}x{nh}): aspect ratio differs; "
                             "the frame would distort it. Regenerate at the old proportions or resize the frame.")
                data = new; n += 1
                print(f"  {label}: {name} <- {src.name} ({nw}x{nh})")
            zout.writestr(item, data)
    shutil.move(str(tmp), str(docx_path))
    print(f"{label}: {n} figure(s) replaced")

def main():
    a = sys.argv[1:]
    if len(a) < 2: sys.exit(__doc__)
    main_in, main_out = Path(a[0]), Path(a[1])
    si_in = si_out = None
    if "--si" in a: si_in, si_out = Path(a[a.index("--si") + 1]), Path(a[a.index("--si") + 2])
    if "--values" in a:
        pv = json.loads(Path(a[a.index("--values") + 1]).read_text())
        chk = [("median", pv["fossil_median"]), ("attr_pct", pv["attr_fossil_pct"]),
               ("ci_below50", pv["ci_below50"]), ("wind_r", pv["wind_r"])]
        bad = [f"  {k}: script {V[k]}, paper_values.json {g}" for k, g in chk if str(V[k]) != str(g)]
        rng = f"{pv['fossil_min']:.0f} to {pv['fossil_max']:.0f}"
        if V["range"] != rng: bad.append(f"  range: script {V['range']}, paper_values.json {rng}")
        if pv.get("fossil_estimator") != "within": bad.append("  paper_values.json was not written under the within-leg estimator")
        if bad: sys.exit("Values disagree with paper_values.json:\n" + "\n".join(bad))
        print("checked against paper_values.json: OK")
    apply(main_in, main_out, [e for e in E if e[2] == "main"], [t for t in TC if t[0] == "main"], "main text")
    if si_in: apply(si_in, si_out, [e for e in E if e[2] == "si"], [t for t in TC if t[0] == "si"], "SI")
    if "--figures" in a:
        figdir = a[a.index("--figures") + 1]
        swap_figures(main_out, figdir, FIGS["main"], "main text")
        if si_in: swap_figures(si_out, figdir, FIGS["si"], "SI")

if __name__ == "__main__":
    main()
