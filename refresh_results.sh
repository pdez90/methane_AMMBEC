#!/usr/bin/env bash
# refresh_results.sh -- copy the curated pipeline outputs into results/ after a run.
# The output folder is $METHANE_OUT_DIR, else the first of ../outputs, ../MethaneData_outputs, ~/MethaneData_outputs.
set -euo pipefail
cd "$(dirname "$0")"
OUT="${METHANE_OUT_DIR:-}"
[ -n "$OUT" ] || for c in ../outputs ../MethaneData_outputs "$HOME/MethaneData_outputs"; do [ -d "$c" ] && { OUT="$c"; break; }; done
[ -d "$OUT" ] || { echo "no output folder: $OUT" >&2; exit 1; }
FILES="transects.csv transects_vs_legs.csv flight_summary.csv transects_vs_blh.csv airborne_blh_flights.csv campaign_sampling_totals.csv aircraft_mobile_overlap.csv arc_ground_ambient.csv arc_ground_coverage.csv arc_ground_plumes.csv
arc_ground_ratios.csv arc_ground_summary.csv background_scatter.csv basin_ethane_methane.csv
basin_implied_source_ratio.csv basin_summary.csv beta_breakeven.csv beta_endmember_ARC.csv
beta_endmember_pipeline.csv beta_sensitivity.csv biogenic_grid.csv biogenic_inbox_totals.csv blh_timeseries.csv
carbonmapper_denver_plumes.csv carbonmapper_denver_sources.csv carbonmapper_encounter_summary.csv
carbonmapper_encounters.csv closeloop_diagnostic.csv denver_curtain_screen.csv denver_curtain_verdict.csv
emission_estimates.csv enh_threshold_balanced.csv enh_threshold_basin.csv enh_threshold_sensitivity.csv
enh_threshold_summary.csv ethane_precision_sensitivity.csv facility_sector_fits.csv facility_separation_scan.csv
facility_separation_scan_rolling.csv background_mode_sensitivity.csv
fig3_leg_structure.csv flight_ethane_methane.csv footprint_extents.csv fossil_estimator_compare.csv
fossil_signature_grid.csv gra2pes_CO_202307_v1.1_county_box.csv gra2pes_cells_v1.1_summary.csv
gra2pes_cells_v2.0beta_summary.csv gra2pes_sector_v1_vs_v2.csv inventory_comparison.csv lag_estimates.csv
lag_sensitivity_flights.csv lag_sensitivity_summary.csv manifest_flights.csv manifest_mobile.csv
mobile_survey_summary.csv mobile_trend_fits.csv mobile_trends.csv paper_values.json perleg_fossil_fraction.csv
pooled_slope_decomposition.csv qc_robustness.csv ratio_method_flux.csv regression_comparison.csv sessionInfo.txt
source_attribution.csv source_attribution_representative.csv spatial_match_cells.csv spatial_match_summary.csv
stilt_exposure_legs.csv stilt_exposure_summary.csv table1.csv table1_full.csv toc_graphic_values.csv
two_endmember_breakeven.csv two_endmember_breakeven_eastcoast.csv two_endmember_flights.csv
two_endmember_floor_flights.csv two_endmember_floor_pdist.csv two_endmember_sensitivity.csv urban_flux.csv
urban_legs.csv v2_pointsource_encounters.csv v2_pointsource_hotspots.csv v2_pointsource_summary.csv
vulcan_co2_boxsum.csv wind_fossil.csv wind_source_exposure_legs.csv wind_source_exposure_summary.csv"
mkdir -p results; n=0; miss=""
for f in $FILES; do
  if [ -f "$OUT/$f" ]; then cp "$OUT/$f" "results/$f"; n=$((n+1)); else miss="$miss $f"; fi
done
echo "copied $n files from $OUT to results/"
[ -z "$miss" ] || echo "not in $OUT (left as is):$miss"
