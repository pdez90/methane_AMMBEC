/**
 * 46_mapl_emit_gee.js — MAPL-EMIT methane plumes over the Denver analysis box.
 *
 * Paste into the Earth Engine Code Editor (code.earthengine.google.com). Earth Engine
 * registration is required; it cannot be run from this repository, which is why this is
 * a .js snippet rather than a pipeline stage.
 *
 * WHAT MAPL-EMIT IS. Google Research / Nature Trace ran a vision-transformer detector
 * over full EMIT radiance granules (Aug 2022 - Jun 2026, 60 m) and published the
 * resulting plume complexes: a methane column enhancement field (ppm-m), an instance
 * mask, a most-likely source location, and a confidence label. It is a SECOND, automated
 * reading of the SAME instrument whose plumes already reach us through Carbon Mapper
 * (the "emi" detections in results/carbonmapper_denver_plumes.csv), where they come from
 * the matched-filter product and human review.
 *
 * WHAT IT ADDS HERE
 *   - Recall. The model is tuned to catch plumes a matched filter misses, so it should
 *     find EMIT detections over Denver that the Carbon Mapper catalog does not list.
 *     That matters most for the question the paper cannot answer from the flights:
 *     how often is a given facility actually emitting?
 *   - Extent. The instance mask gives a plume footprint, not just a point, so a
 *     detection can be placed against the 0.02 deg cells of the ethane signature grid.
 *
 * WHAT IT CANNOT DO
 *   - It reports column ENHANCEMENTS (ppm-m), not emission rates. Converting to kg/hr
 *     needs an IME calculation with a wind field and carries its own uncertainty, so
 *     nothing here is directly comparable to the 4.6-10.7 t/hr campaign estimate or to
 *     the inventory sectors. Carbon Mapper stays the quantitative source.
 *   - Medium-confidence plumes have a ~50-55% false-positive rate by the producers' own
 *     human review. Use 'high' confidence (detected 3+ times, ~3-5% false positive), or
 *     filter medium ones against known infrastructure before believing them.
 *   - The model misses ~16% of expert-annotated NASA EMIT L2B plumes, and degrades over
 *     cloud, shadow and dark surfaces. Absence still is not absence of emission.
 *
 * Out: a CSV in your Drive with one row per plume (date, confidence, source location,
 * and whatever else the collection carries), to sit beside
 * results/carbonmapper_denver_plumes.csv.
 */

// The analysis box of config.R (URBAN_BOX), west, south, east, north.
var BOX = ee.Geometry.Rectangle([-105.20, 39.50, -104.55, 39.95]);

var plumes = ee.ImageCollection('projects/nature-trace/assets/ghg/emit/mapl_emit_plumes_v1_0')
                 .filterBounds(BOX);

print('plume complexes intersecting the Denver box:', plumes.size());
print('first element, to see what properties exist:', plumes.first());
print('confidence values present:', plumes.aggregate_histogram('confidence'));

// One feature per plume: its own properties plus the centroid of its footprint.
// (The collection carries a source location; the centroid is kept as a fallback for
// plumes where that property is absent, and as a check on it.)
var rows = plumes.map(function (img) {
  var c = img.geometry().centroid(100);
  return ee.Feature(c, img.toDictionary()
      .set('image_id', img.get('system:index'))
      .set('millis', img.get('system:time_start'))
      .set('centroid_lon', c.coordinates().get(0))
      .set('centroid_lat', c.coordinates().get(1)));
});

Export.table.toDrive({
  collection: ee.FeatureCollection(rows),
  description: 'mapl_emit_plumes_denver',
  fileFormat: 'CSV'
});

// ---- visual check, high confidence only -------------------------------------------
var high = plumes.filter(ee.Filter.eq('confidence', 'high'));
print('high-confidence plumes:', high.size());
var enh = high.map(function (img) {
  return img.select('methane_enhancement')
            .updateMask(img.select('plume_probability').gt(0.5));
});
Map.centerObject(BOX, 10);
Map.addLayer(ee.Image().paint(BOX, 0, 2), {palette: ['000000']}, 'analysis box');
Map.addLayer(enh.mosaic(), {min: 0, max: 1500, palette: ['white', 'yellow', 'red']},
             'CH4 enhancement, ppm-m (high confidence)');

// The two landfills that dominate the Carbon Mapper catalog over this box, for
// orientation: Tower Rd (Commerce City) and DADS (Aurora).
Map.addLayer(ee.Geometry.Point([-104.757, 39.852]), {color: 'blue'}, 'Tower Rd landfill');
Map.addLayer(ee.Geometry.Point([-104.690, 39.662]), {color: 'blue'}, 'DADS landfill');
Map.addLayer(ee.Geometry.Point([-104.938, 39.792]), {color: 'green'}, 'Metro Water Recovery');
Map.addLayer(ee.Geometry.Point([-104.939, 39.802]), {color: 'red'}, 'Suncor refinery');
