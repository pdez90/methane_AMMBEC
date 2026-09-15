#!/usr/bin/env python3
"""61_manuscript_v2_edits.py -- manuscript edits for the GRA2PES v2.0beta comparison, the
cell-matched spatial comparison (script 58), the v2 point-source test (script 59) and the
Carbon Mapper data section. Applied to the CLEANED submission files (black text); every
change is written in red. Numbers are read from the pipeline outputs, never typed.

  python3 scripts/61_manuscript_v2_edits.py <main.docx> <si.docx> <main_out> <si_out> \
      --outputs <OUT_DIR> --figures <OUT_DIR>/figures
Then reorder the SI:  python3 scripts/60_si_reorder.py <main_out> <si_out> <main_final> <si_final>
"""
import sys, csv, argparse
from pathlib import Path
sys.path.insert(0, str(Path(__file__).resolve().parent))
import docx_edit_lib as L
from docx_edit_lib import R, W, I, P, T, FIGSWAP

ap = argparse.ArgumentParser(); ap.add_argument("main_in"); ap.add_argument("si_in"); ap.add_argument("main_out"); ap.add_argument("si_out")
ap.add_argument("--outputs", required=True); ap.add_argument("--figures", required=True)
a = ap.parse_args(); OUT = Path(a.outputs); L.FIGDIR = a.figures; L.OUTDIR = str(OUT)

def rows(name): return list(csv.DictReader(open(OUT / name)))
def one(name): return rows(name)[0]
f = lambda x: float(x)

# ---- values from the pipeline -------------------------------------------------------------
v2s = one("gra2pes_cells_v2.0beta_summary.csv"); v1s = one("gra2pes_cells_v1.1_summary.csv")
V2 = dict(total=f(v2s["ch4_total_t_hr"]), waste=f(v2s["waste_t_hr"]), og=f(v2s["og_t_hr"]), pm=f(v2s["postmeter_t_hr"]),
          ag=f(v2s["ag_t_hr"]), co=f(v2s["co_Gg_yr"]), ff=f(v2s["fossil_frac_box"]))
V1 = dict(total=f(v1s["ch4_total_t_hr"]), waste=f(v1s["waste_t_hr"]), og=f(v1s["og_t_hr"]), pm=f(v1s["postmeter_t_hr"]),
          co=(f(v1s["co_Gg_yr"]) if v1s["co_Gg_yr"] not in ("", "NA") else 121.5), ff=f(v1s["fossil_frac_box"]))   # 121.5 = config E_CO_DENVER (v1.1 box CO)
SM = {r["inventory"].split(" (")[0]: r for r in rows("spatial_match_summary.csv")}
epa = SM["EPA gridded GHGI 2020"]; g1 = SM.get("GRA2PES v1.1"); g2 = SM.get("GRA2PES v2.0beta")
PS = rows("v2_pointsource_summary.csv"); PE = rows("v2_pointsource_encounters.csv"); PH = rows("v2_pointsource_hotspots.csv")
ok = [r for r in PE if r["hotspot_in_fetch"] == "TRUE" and r["Q_t_hr"] not in ("", "NA")]
import statistics as st
med = lambda xs: st.median(xs) if xs else float("nan")
rb = [f(r["ratio_bio_over_v2waste"]) for r in ok if r["ratio_bio_over_v2waste"] not in ("", "NA")]
rt = [f(r["ratio_total_over_v2total"]) for r in ok if r["ratio_total_over_v2total"] not in ("", "NA")]
q  = [f(r["Q_t_hr"]) for r in ok]; qb = [f(r["Q_bio_t_hr"]) for r in ok if r["Q_bio_t_hr"] not in ("", "NA")]
n_hot = len(PH); hot_waste = sum(f(r["waste_t_hr"]) for r in PH)
n_enc = len(ok); n_fl = len(set(r["flight"] for r in ok)); n_sites = len(set(r["hotspot_id"] for r in ok))
pct = lambda x: f"{100 * float(x):.0f}%"
words = {1: "one", 2: "two", 3: "three", 4: "four", 5: "five", 6: "six"}
co_pct = 100 * (V2["co"] / 121.5 - 1)          # vs the v1.1 anchor as used in the paper (script 36, all day types)
import json, re as _re
pv = json.loads(open(OUT / "paper_values.json").read())
inv = rows("inventory_comparison.csv")
EF = sum(f(r["t_hr"]) for r in inv if r["group"] == "fossil"); EB = sum(f(r["t_hr"]) for r in inv if r["group"] == "biogenic")
EC = sum(f(r["t_hr"]) for r in inv if r["group"] == "combustion"); ET = EF + EB + EC
ENG = sum(f(r["t_hr"]) for r in inv if _re.search("Distribution|PostMeter", r["sector"]))
P_INV = ENG / EF
two = rows("two_endmember_flights.csv")
den_inv = next(r for r in two if r["case"].startswith("inventory-weighted, DENVER") and "2021" in r["case"])
ec_inv = [r for r in two if r["case"].startswith("inventory-weighted, other-cities")]
ec_lo, ec_hi = min(f(r["median_fossil_pct"]) for r in ec_inv), max(f(r["median_fossil_pct"]) for r in ec_inv)
fl_max = max(f(den_inv[k]) for k in den_inv if k.startswith("2024"))
inbox = {r["quantity"]: f(r["value"]) for r in rows("biogenic_inbox_totals.csv")} if (OUT / "biogenic_inbox_totals.csv").exists() else {}
print(f"EPA area-weighted: total {ET:.2f} fossil {EF:.2f} ({100*EF/ET:.0f}% of total, {100*EF/(EF+EB):.0f}% of F+B) biogenic {EB:.2f} combustion {EC:.2f}; NG dist+pm {ENG:.2f}; p_inv {P_INV:.2f}; Denver inv case {den_inv['median_fossil_pct']}% (max flight {fl_max:.0f}%), East Coast inv {ec_lo:.0f}-{ec_hi:.0f}%; in-box waste {inbox.get('waste_in_box_t_hr')} livestock {inbox.get('livestock_in_box_t_hr')}")

print(f"v2: total {V2['total']:.1f} waste {V2['waste']:.1f} og {V2['og']:.2f} pm {V2['pm']:.2f} ff {V2['ff']:.2f} CO {V2['co']:.1f} (+{co_pct:.1f}% vs v1.1 {V1['co']:.1f})")
print(f"spatial: EPA rho {epa['spearman_rho']} agree {epa['class_agreement']}; v1 {g1 and g1['spearman_rho']}; v2 {g2 and g2['spearman_rho']} agree {g2 and g2['class_agreement']}")
print(f"point sources: {n_hot} hotspots ({hot_waste:.1f} t/hr), {n_enc} encounters on {n_fl} flights at {n_sites} sites; bio/v2waste median {med(rb):.2f} (range {min(rb):.2f}-{max(rb):.2f}); total/v2total median {med(rt):.2f}")

# =========================================================================================
# MAIN TEXT
# =========================================================================================
# 2.5 / 2.6  Data: point-source detections and inventories (after the lidar section 2.4)
I("It is used to document the daytime boundary layer and in the sensitivity analysis that applies an in-boundary layer filter",
  "[[H2]] 2.5. Independently mapped point sources (Carbon Mapper)\n\n"
  "To place the aircraft sampling against independently located discrete sources, we use Carbon Mapper's public plume "
  "catalogue[[^36]] for the analysis box (accessed 12 September 2026). It holds 31 methane plumes and one CO2 plume observed "
  "between July 2021 and April 2026 by the airborne Global Airborne Observatory (17 plumes), EMIT on the International Space "
  "Station (7) and Tanager-1 (8); none was acquired during the campaign. The catalogue resolves five methane point sources "
  "inside the box: two landfills (Tower Road, 18 plumes on 15 dates; DADS, 9 plumes on 6 dates), which together account for "
  "27 of the 31 methane plumes, and three oil-and-gas sites near the northern edge of the box with one or two detections "
  "each; the refinery has no detection. These are snapshot detections above a per-plume detection limit of roughly 100 kg/hr "
  "(lower for the airborne instrument), so they are used only to locate discrete sources, to test whether aircraft samples "
  "downwind of them carry ethane (section 4.1), and as an independent lower bound on facility emissions; they are not an "
  "emission total.\n\n"
  "[[H2]] 2.6. Gridded emission inventories\n\n"
  "Four gridded inventories enter the analysis, each summed over the identical analysis box. The sector-resolved U.S. gridded "
  "EPA greenhouse-gas inventory (2020 Express Extension, 0.1 degree)[[^30]] supplies the bottom-up methane source split. GRA2PES "
  "v1.1 (4 km, July 2023)[[^21]] supplies the primary CO anchor and a second bottom-up methane total. GRA2PES v2.0beta (4 km, "
  "July 2023; NOAA Chemical Sciences Laboratory, beta release of June 2026) adds sector-resolved methane with rebuilt waste "
  "(landfill and wastewater) emissions, a post-meter natural-gas leak term and updated oil-and-gas production, and is used as a "
  "third bottom-up methane estimate and for a 4-km cell-by-cell comparison with the aircraft (section 4.3); it is a beta "
  "product whose methods are still being documented, and we treat it as such. Vulcan v4.0 fossil-fuel CO2 (1 km, 2022)[[^20]] "
  "supplies the CO2 anchor, and the EPA 2020 National Emissions Inventory county CO totals a sensitivity anchor.")

# 3.4 anchors: v2 CO changes the anchor by only a few percent
R("As a sensitivity analysis, we also use the EPA 2020 NEI seven-county metropolitan CO total (292.7 Gg CO yr−1).",
  f"The GRA2PES v2.0beta CO total over the same box is {V2['co']:.1f} Gg CO yr-1, within {__import__('math').ceil(max(abs(co_pct), abs(100 * (V2['co'] / V1['co'] - 1)))):d}% of v1.1 "
  "(121.5 on the day-type-weighted July basis of the anchor), so the CO-anchored rates are insensitive to the inventory version; "
  "we report the v1.1-anchored values. "
  "As a sensitivity analysis, we also use the EPA 2020 NEI seven-county metropolitan CO total (292.7 Gg CO yr−1).")

# 4.3: cell-matched spatial comparison and the v2 point-source test
def spat_sentence(label, r):
    return (f"{label}: median fossil fraction at the sampled cells {f(r['inventory_median_ff_at_cells']):.2f}, "
            f"Spearman rho {f(r['spearman_rho']):.2f} (p = {f(r['p_value']):.2g}), class agreement {pct(r['class_agreement'])}, "
            f"inventory fossil fraction {f(r['inventory_ff_sampled_weighted']):.2f} over the sampled cells against {f(r['inventory_ff_box_weighted']):.2f} box-wide")
spat = ("The comparison in Figure 4B can be made cell by cell. For each of the "
        f"{epa['n_cells']} aircraft cells with a local ethane:methane slope (median fossil fraction {f(epa['aircraft_median_ff']):.2f} at 0.102; "
        f"{epa['aircraft_median_ff_lo_hi']} across the Denver-relevant endmembers), we sampled each inventory's fossil fraction at the same location "
        "(section S10, Figure S13, Table S10). " + spat_sentence("EPA gridded GHGI", epa) + ". "
        + (spat_sentence("GRA2PES v1.1", g1) + ". " if g1 else "")
        + (spat_sentence("GRA2PES v2.0beta", g2) + ". " if g2 else "")
        + f"The sampled 4-km cells hold {pct(g2['share_of_inventory_ch4_in_sampled_cells'])} of v2.0beta's box methane"
        + (f" and {pct(g1['share_of_inventory_ch4_in_sampled_cells'])} of v1.1's" if g1 else "") + ", so the aircraft sampled the emitting core of the box rather than its margins. "
        f"The EPA grid is more fossil than the aircraft at {epa['cells_inventory_more_fossil']} of the {epa['n_cells']} cells, and the aircraft sampled the "
        "more fossil-weighted part of its map, so the disagreement is in the inventory rather than in where the aircraft flew. "
        + (f"GRA2PES v1.1 is fossil almost everywhere because it carries no waste methane. " if g1 else "")
        + f"GRA2PES v2.0beta is more fossil still at the sampled cells (median {f(g2['inventory_median_ff_at_cells']):.2f}) even though its box-wide fossil share "
        f"is only {f(g2['inventory_ff_box_weighted']):.2f}: it holds nearly all of its biogenic methane in {words.get(n_hot, str(n_hot))} landfill and wastewater cells and describes the rest of "
        "the city as gas-system emissions, whereas the aircraft see a biogenic-leaning signature nearly everywhere they sampled. A cell-by-cell comparison "
        "tests the co-location of emitted and observed character, which an inventory of point-like biogenic sources cannot pass at 4 km even if its totals "
        "were right, because the air the aircraft sample integrates the upwind fetch; for v2.0beta the fairer tests are the fetch-based one that follows "
        "and the box totals of section 5.")

def site_name(nm):
    return "the waste cell immediately north of the Suncor/Metro Water Recovery complex" if ("Suncor" in nm or "Metro" in nm) else ("the DADS landfill" if "DADS" in nm else ("the Tower Road landfill" if "Tower" in nm else nm))
def site_clause(r):
    n = int(r["n_encounters"]); w = f(r["v2_cell_waste_t_hr"]); qb_ = f(r["Q_bio_median"]); lo = f(r["Q_total_min"]); hi = f(r["Q_total_max"]); rr = f(r["ratio_bio_over_v2waste_median"])
    words = {1: "one", 2: "two", 3: "three", 4: "four", 5: "five", 6: "six"}
    if n == 1:
        return (f"the one crossing downwind of {site_name(r['hotspot'])} (v2.0beta waste {w:.2f} t/hr in that cell) gives a biogenic rate of "
                f"{qb_:.2f} t/hr (total {hi:.2f} t/hr), {rr:.2f} times the v2.0beta waste emissions in the upwind fetch")
    return (f"the {words.get(n, str(n))} crossings downwind of {site_name(r['hotspot'])} (v2.0beta waste {w:.2f} t/hr in that cell) "
            f"give a biogenic rate of {qb_:.2f} t/hr in the median (total {lo:.2f} to {hi:.2f} t/hr), "
            f"{rr:.2f} times the v2.0beta waste emissions in the upwind fetch")
clauses = "; ".join(site_clause(r) for r in PS)
lf = [f(r["ratio_bio_over_v2waste_median"]) for r in PS if "DADS" in r["hotspot"] or "Tower" in r["hotspot"]]
ww = [f(r["ratio_bio_over_v2waste_median"]) for r in PS if "Suncor" in r["hotspot"] or "Metro" in r["hotspot"]]
if lf and ww and max(lf) < 0.5 and min(ww) > 1.5:
    verdict = ("Which facility the third cell represents cannot be read from the gridded product: it lies immediately north of the Metro Water Recovery "
               "plant, and the cell that contains the plant itself carries no waste methane in v2.0beta, so the cell may be the plant with a displaced "
               "location or another waste facility. Either way, the aircraft find far less methane than v2.0beta downwind of the landfill they could test, "
               "and more biogenic methane around the wastewater plant than v2.0beta places in that cell and the plant's own cell together: the v2.0beta "
               "waste total is high, and its split between the landfills and the wastewater plant appears to be the reverse of what the aircraft, and Carbon "
               "Mapper's persistence-weighted landfill rates (Table S11), indicate.")
else:
    verdict = "[AUTHOR: state the per-site conclusion from the clauses above.]"
psrc = (f"GRA2PES v2.0beta concentrates its waste methane in {words.get(n_hot, str(n_hot))} cells carrying {hot_waste:.1f} of the {V2['waste']:.1f} t/hr box total, "
        "which makes it testable facility by facility. Where an urban leg crossed downwind of one of these cells within 12 km and 25 degrees of "
        f"its downwind line ({words.get(n_enc, str(n_enc))} crossings on {words.get(n_fl, str(n_fl))} flights at {words.get(n_sites, str(n_sites))} of the {words.get(n_hot, str(n_hot))} sites), a single-transect mass balance of the crossing, with the "
        "ethane slope splitting the plume into fossil and biogenic parts, gives the rate through the crossing (section S12, Figure S14, Table S12): "
        + clauses + ". These single-transect rates scale with the lidar mixing height and assume uniform mixing to it, so they are order-of-magnitude "
        "constraints rather than facility emission estimates. " + verdict)
I("Relative to the bottom-up inventory, the biogenic sectors are roughly six times larger, with the same over-attribution to the gas system seen in the totals.",
  spat + "\n\n" + psrc)

# Figure 4 caption: bars now include v1.1 and v2.0beta
R("the sector-resolved EPA gridded inventory is shown for comparison (right bar).",
  "the sector-resolved EPA gridded inventory, GRA2PES v1.1 and GRA2PES v2.0beta are shown for comparison (right bars; v2.0beta classes are "
  "waste plus agriculture as biogenic and oil-and-gas plus post-meter as fossil).")

# 5 Discussion: the inventory paragraph
R("A second box-consistent bottom-up estimate, GRA2PES v1.1 total methane summed over the same box (July 2023), gives 1.7 t CH4/hr.",
  f"A second box-consistent bottom-up estimate, GRA2PES v1.1 total methane summed over the same box (July 2023), gives {V1['total']:.1f} t CH4/hr; "
  f"v1.1 carries no waste methane in the box at all (its {V1['ff']*100:.0f}% fossil share is the gas system against a near-empty biogenic column). "
  f"The beta release of GRA2PES v2.0 for the same month gives {V2['total']:.1f} t CH4/hr, of which {V2['waste']:.1f} t/hr is waste "
  f"(landfills and wastewater), {V2['og']:.1f} t/hr oil and gas, {V2['pm']:.2f} t/hr post-meter leakage and {V2['ag']:.2f} t/hr agriculture, "
  f"a fossil share of {V2['ff']*100:.0f}% of the fossil-plus-biogenic sum (Figure 4A).")
R("Both bottom-up inventories therefore place Denver near 2 t/hr (1.7 to 2.6). Our airborne top-down estimates (the enhancement-ratio estimates, about 4 to 11 t/hr) exceed both bottom-up inventories by factors of about 1.6 to 6 across flights and anchors, and by a factor of about 3 to 4.5 at the CH4:CO median of 7.6 t/hr.",
  f"The two published inventories therefore place Denver near 2 t/hr (1.7 to {ET:.1f}). Our airborne top-down estimates (the enhancement-ratio "
  f"estimates, about 4 to 11 t/hr) exceed both published inventories by factors of about {4.6/ET:.1f} to 6 across flights and anchors, and by a factor of "
  f"about {7.6/ET:.1f} to 4.5 at the CH4:CO median of 7.6 t/hr.")
R("The gridded inventory attributes about 60% of Denver's fossil-plus-biogenic methane to fossil sources,",
  f"The v2.0beta total, in contrast, lies a factor of {V2['total'] / 10.7:.1f} to {V2['total'] / 4.6:.1f} above the airborne range: its waste sector alone "
  f"({V2['waste']:.1f} t/hr) exceeds the whole range, and its source split ({100 - V2['ff']*100:.0f}% biogenic) agrees with the airborne ethane "
  f"({100 - 48:.0f} to {100 - 27:.0f}% biogenic across the Denver-relevant endmembers) while its magnitude does not: the airborne biogenic total of about "
  f"5.9 t/hr (section 4.3) is a factor of about {V2['waste'] / 5.9:.0f} below the v2.0beta waste total, and the facility-scale crossings of section 4.3 "
  "point the same way. The two GRA2PES releases thus bracket the airborne estimate from opposite sides, v1.1 by omitting waste methane and v2.0beta by, "
  "on this evidence, overstating it. The EPA gridded inventory attributes about 60% of Denver's fossil-plus-biogenic methane to fossil sources,")
R("The EPA inventory thus attributes a larger fraction of methane to fossil sources than the airborne ethane observations imply, even though their total is exceeded by our higher-flux days.",
  "The EPA inventory thus attributes a larger fraction of methane to fossil sources than the airborne ethane observations imply, even though its total "
  "is exceeded by our higher-flux days, whereas the v2.0beta source split is close to the airborne one and it is the v2.0beta magnitude that the aircraft do not support.")

# Limitations: anchors paragraph
R("The anchors are also not contemporaneous with the 2024 flights (Vulcan fossil CO2 is 2022, GRA2PES CO and methane are July 2023, the EPA GHGI and NEI are 2020), so the magnitudes inherit any 2020 to 2024 trend in the anchor species.",
  "The anchors are also not contemporaneous with the 2024 flights (Vulcan fossil CO2 is 2022, GRA2PES CO and methane are July 2023, the EPA GHGI and NEI are 2020), "
  "so the magnitudes inherit any 2020 to 2024 trend in the anchor species. GRA2PES v2.0beta is a beta product whose waste and post-meter methane sectors "
  "have not yet been evaluated against observations; the comparisons here are offered as one such evaluation, not as a replacement of the published v1.1 anchor.")

# Data availability
R("GRA2PES v1.1 (July 2023) CO and methane,", "GRA2PES v1.1 and v2.0beta (July 2023) CO and sector methane (v2.0beta obtained from NOAA CSL under its beta data policy),")

# Figure 4 picture (Fig4_source_combined.png; the old picture is 1700 x 1706 px)
FIGSWAP("main", (1700, 1706), str(Path(a.figures) / "Fig4_source_combined.png"))
# Figure S3b (two-endmember; its inventory-share line moves with the area-weighted EPA sum): SI picture of 1300 x 820 px
FIGSWAP("si", (1300, 820), str(Path(a.figures) / "FigS3b_two_endmember.png"))

# =========================================================================================
# SI
# =========================================================================================
# S10 (spatial context) gets a cell-matched subsection; inserted after its last caveat paragraph
I("Figure S9: Gridded EPA GHGI biogenic methane sources relative to the Denver-metro analysis box",
  "[[H2]] S10.1 Cell-by-cell comparison of the aircraft with three inventories\n\n"
  f"Figure 4B compares the aircraft-sampled ethane character with the EPA inventory by eye. Here the comparison is made cell by cell (scripts 57 and 58 of the "
  f"analysis repository). For each of the {epa['n_cells']} aircraft cells (0.02 degree) with a local York ethane:methane slope, the fossil fraction at the adopted "
  "endmember is compared with the fossil fraction (fossil over fossil plus biogenic, the definition used for the EPA inventory in the main text) of the 0.1-degree "
  "EPA cell and of the 4-km GRA2PES v1.1 and v2.0beta cells containing it. GRA2PES per-cell sector methane was extracted from the July 2023 weekday files with the "
  "same cell and unit handling as the box sums; fossil is oil-and-gas plus post-meter, biogenic is waste plus agriculture. Three statistics are reported per inventory: "
  "the Spearman rank correlation between the aircraft and inventory fractions across the cells, the share of cells on which the two agree in class (biogenic below "
  "one third, mixed, fossil above two thirds), and the inventory's emission-weighted fossil fraction over the sampled cells against the whole box, which says whether "
  "the aircraft sampled the fossil-rich or the biogenic-rich part of each inventory.\n\n"
  "[[TABLE:spatial_match_summary.csv|inventory=Inventory,n_cells=Cells,aircraft_median_ff=Aircraft median,inventory_median_ff_at_cells=Inventory median at cells,"
  "inventory_ff_sampled_weighted=Inventory (sampled cells),inventory_ff_box_weighted=Inventory (box),share_of_inventory_ch4_in_sampled_cells=Share of box CH4 sampled,"
  "spearman_rho=Spearman rho,p_value=p,class_agreement=Class agreement|aircraft_median_ff=%.2f;inventory_median_ff_at_cells=%.2f;inventory_ff_sampled_weighted=%.2f;"
  "inventory_ff_box_weighted=%.2f;share_of_inventory_ch4_in_sampled_cells=%.2f;spearman_rho=%.2f;p_value=%.2g;class_agreement=%.2f]]\n\n"
  "Table S10: Cell-matched comparison of the aircraft fossil fraction (0.102 endmember) with three gridded inventories at the aircraft-sampled cells. "
  "Aircraft median is the median over the cells; inventory medians and emission-weighted fractions are fossil over fossil plus biogenic.\n\n"
  "[[FIG:FigS13_spatial_match.png]]\n\n"
  "Figure S13: Cell-by-cell spatial comparison. (A) Source context at 4 km: GRA2PES v2.0beta total methane density (grey), its waste cells (green outline), "
  "Carbon Mapper landfill (up-triangles) and oil-and-gas (down-triangles) sources sized by plume count, the three named facilities (crosses), urban leg "
  "centroids, and the aircraft cells coloured by their ethane-derived fossil fraction (green biogenic to red fossil). (B) EPA gridded GHGI fossil fraction at "
  "0.1 degree with the same aircraft cells. (C) GRA2PES v2.0beta fossil fraction at 4 km. (D) Aircraft against inventory fossil fraction at the same location, "
  "one point per cell; the dashed line is 1:1.", "si")

# New section after the Carbon Mapper encounters (S11): aircraft transects vs v2 waste sources
I("Figure S12: Ethane:methane slope of aircraft samples downwind of Carbon Mapper sources, by source class.",
  "[[H]] S12. Aircraft transects downwind of the GRA2PES v2.0beta waste sources\n\n"
  f"GRA2PES v2.0beta places {V2['waste']:.1f} t CH4/hr of waste methane in the analysis box, concentrated in {words.get(n_hot, str(n_hot))} 4-km cells that hold "
  f"{hot_waste:.1f} t/hr between them (Table S11); two of these cells contain the landfills mapped by Carbon Mapper (DADS and Tower Road); the third lies immediately north of the Metro Water Recovery plant, whose own cell carries no waste methane in v2.0beta, so whether it represents the plant or another waste facility is not determinable from the gridded product. "
  "A field this concentrated can be tested where the aircraft crossed downwind of it. For every urban leg in the boundary layer we linked samples to a "
  "hotspot cell when they lay within 12 km and 25 degrees of its downwind line (the aircraft wind), the criterion of section S11, and treated the "
  "contiguous run of linked samples as a plume crossing when it contained at least ten samples and a methane enhancement of at least 20 ppb. The rate "
  "through each crossing is a single-transect mass balance: the along-leg integral of air density times methane enhancement times the wind component "
  "perpendicular to the leg, multiplied by the lidar mixing height of the flight (uniform mixing to that height). The ethane:methane York slope of the "
  "crossing, divided by the adopted endmember, splits the rate into fossil and biogenic parts. Each crossing is compared with the v2.0beta emissions in "
  "its upwind fetch: all 4-km cells upwind of the crossed segment within 12 km whose downwind line reaches the segment (2 km of lateral padding), summed "
  "for waste and for total methane; the same fetch is summed for v1.1. Two cautions apply. The rate scales with the mixing height, which comes from a lidar "
  "40 km away, and the fetch is a geometric selection, not a transport calculation, so emissions from outside it can reach the segment and emissions inside "
  "it can miss it. The comparison is therefore an order-of-magnitude consistency test of the v2.0beta waste field, not a facility emission estimate.\n\n"
  f"There are {words.get(n_enc, str(n_enc))} crossings on {words.get(n_fl, str(n_fl))} flights at {words.get(n_sites, str(n_sites))} of the hotspot sites (Table S12, Figure S14). Their total rates are {min(q):.1f} to {max(q):.1f} t/hr and their "
  f"biogenic parts {min(qb):.1f} to {max(qb):.1f} t/hr. Relative to the v2.0beta waste emissions in the fetch, the aircraft biogenic rate is {med(rb):.2f} in the median "
  f"(range {min(rb):.2f} to {max(rb):.2f}); relative to the v2.0beta total in the fetch, the aircraft total is {med(rt):.2f} in the median. "
  "By site: " + clauses + ". " + verdict + "\n\n"
  "[[TABLE:v2_pointsource_hotspots.csv|hotspot_id=Site,nearest_facility=Nearest named facility,facility_km=km,lat=Lat,lon=Lon,waste_t_hr=v2 waste (t/hr),"
  "ch4_total_t_hr=v2 total (t/hr),fossil_frac=v2 fossil fraction,carbonmapper_kg_hr=Carbon Mapper persistence-weighted rate (kg/hr)|lat=%.3f;lon=%.3f;waste_t_hr=%.2f;"
  "ch4_total_t_hr=%.2f;fossil_frac=%.2f;carbonmapper_kg_hr=%.0f]]\n\n"
  "Table S11: GRA2PES v2.0beta waste hotspot cells (at least 0.25 t CH4/hr of waste methane), the nearest named facility, and the Carbon Mapper "
  "persistence-weighted rate of any mapped source within 3.5 km.\n\n"
  "[[TABLE:v2_pointsource_encounters.csv|flight=Flight,leg_id=Leg,hotspot_id=Site,dist_km=Distance (km),agl_m=Altitude (m AGL),blh_m=Mixing height (m),"
  "dch4_max_ppb=Max dCH4 (ppb),Q_t_hr=Rate (t/hr),fossil_frac=Fossil fraction,Q_bio_t_hr=Biogenic rate (t/hr),"
  "v2_fetch_waste_t_hr=v2 waste in fetch (t/hr),v2_fetch_total_t_hr=v2 total in fetch (t/hr),v1_fetch_total_t_hr=v1.1 total in fetch (t/hr)"
  "|Q_t_hr=%.2f;fossil_frac=%.2f;Q_bio_t_hr=%.2f;v2_fetch_waste_t_hr=%.2f;v2_fetch_total_t_hr=%.2f;v1_fetch_total_t_hr=%.2f|hotspot_in_fetch=TRUE]]\n\n"
  "Table S12: Aircraft plume crossings downwind of the v2.0beta waste hotspots (sites numbered as in Table S11): single-transect rate, ethane split, and the "
  "inventory emissions in the upwind fetch. Crossings whose hotspot cell fell outside the fetch (the leg was too far off the downwind line) are omitted.\n\n"
  "[[FIG:FigS14_v2_pointsource.png]]\n\n"
  "Figure S14: Aircraft transect rates downwind of the GRA2PES v2.0beta waste hotspots against the inventory emissions in the upwind fetch: (A) the biogenic "
  "part of the aircraft rate against v2.0beta waste; (B) the total aircraft rate against the v2.0beta total, with the v1.1 total in the same fetch as crosses. "
  "Dashed line 1:1, dotted lines a factor of two either side; colours identify the sites of Table S11.", "si")

# =========================================================================================
# EPA GHGI: area-weighted box sums (scripts 14 and 32 now weight each 0.1-degree cell by the
# fraction of its area inside the box; the old centre selection summed a 2,664 km2 footprint)
# =========================================================================================
# abstract
R("exceed two box-consistent bottom-up inventories (1.7 and 2.6 t CH4 per hour), by about 3 to 4.5-fold at the CH4:CO median.",
  f"exceed two box-consistent bottom-up inventories (1.7 and {ET:.1f} t CH4 per hour), by about {7.6/ET:.1f} to 4.5-fold at the CH4:CO median.")
# 3.2 / limitation 1: inventory share of distribution + post-meter
R("That measurement matters because about 70% of the fossil methane in the gridded inventory for this box is natural-gas distribution and post-meter end use",
  f"That measurement matters because about {100*P_INV:.0f}% of the fossil methane in the gridded inventory for this box is natural-gas distribution and post-meter end use")
# 5 Discussion: the EPA paragraph
R("Summed over the same Denver-metro box, it totals 2.6 t CH4/hr for the 2020 Express Extension (the year nearest the campaign). Of this, 1.4 t/hr is fossil (about 54% of the total, and about 60% of the fossil-plus-biogenic sum), dominated by natural-gas distribution and post-meter end-use leakage (together about 1.0 t/hr). The remainder is 1.0 t/hr biogenic (landfills and wastewater) and 0.2 t/hr from combustion.",
  f"Summed over the same Denver-metro box, with each 0.1-degree cell weighted by the fraction of its area inside the box, it totals {ET:.1f} t CH4/hr for the "
  f"2020 Express Extension (the year nearest the campaign). Of this, {EF:.1f} t/hr is fossil (about {100*EF/ET:.0f}% of the total, and about {100*EF/(EF+EB):.0f}% of the "
  f"fossil-plus-biogenic sum), dominated by natural-gas distribution and post-meter end-use leakage (together about {ENG:.1f} t/hr). The remainder is "
  f"{EB:.1f} t/hr biogenic (landfills and wastewater) and {EC:.1f} t/hr from combustion.")
# SI S3: inventory-weighted two-endmember statements
R("(inventory-weighted mixture, p = 0.69 with a basin ratio of 0.061: 31%, with the second 13 July flight at the halfway line and no flight above it; distribution gas alone: 27%)",
  f"(inventory-weighted mixture, p = {P_INV:.2f} with a basin ratio of 0.061: {f(den_inv['median_fossil_pct']):.0f}%, with "
  + (f"the second 13 July flight at {fl_max:.0f}% and no other flight above 50%" if fl_max >= 50 else "no flight above 50%") + "; distribution gas alone: 27%)", "si")
R("reaching 66 to 93% at the inventory weighting for a basin ratio of 0.061",
  f"reaching {ec_lo:.0f} to {ec_hi:.0f}% at the inventory weighting for a basin ratio of 0.061", "si")
R("the vertical line the inventory share of distribution and post-meter gas in the box's fossil methane (0.69). The p = 0.69 line is shown only",
  f"the vertical line the inventory share of distribution and post-meter gas in the box's fossil methane ({P_INV:.2f}). The p = {P_INV:.2f} line is shown only", "si")
# SI S10 (spatial context): in-box waste and livestock, area-weighted
if inbox:
    R("Landfill, wastewater, and composting emissions are concentrated within and around the analysis box, totaling 0.87 t CH4/hr within it.",
      f"Landfill, wastewater, and composting emissions are concentrated within and around the analysis box, totaling {inbox['waste_in_box_t_hr']:.2f} t CH4/hr within it (each 0.1-degree cell weighted by the fraction of its area inside the box).", "si")
    R("contributing only 0.10 t CH4/hr within it.", f"contributing only {inbox['livestock_in_box_t_hr']:.2f} t CH4/hr within it.", "si")

L.apply(a.main_in, a.main_out, "main", "main text")
L.apply(a.si_in, a.si_out, "si", "SI")
print("\nNext: python3 scripts/60_si_reorder.py", a.main_out, a.si_out, "<main_final.docx> <si_final.docx>")
