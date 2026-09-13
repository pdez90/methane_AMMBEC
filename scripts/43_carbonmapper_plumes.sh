#!/usr/bin/env bash
# 43_carbonmapper_plumes.sh — pull Carbon Mapper point-source plume detections over
# the Denver analysis box and summarise them by facility cluster and IPCC sector.
#
#   bash scripts/43_carbonmapper_plumes.sh            # the urban box from config.R
#   BOX="-105.4 39.9 -104.2 40.7" bash scripts/43_carbonmapper_plumes.sh   # the DJB
#
# Out: <OUT_DIR>/carbonmapper_denver_plumes.csv  + a summary on stdout
#
# NOT part of run_all.R: it needs the internet, and the catalog changes as new
# overpasses are processed and emission versions are reprocessed, so it is a
# snapshot rather than a reproducible pipeline stage. The CSV records the fetch date.
#
# WHAT THIS IS AND IS NOT COMPARABLE TO. Carbon Mapper reports INDIVIDUAL POINT-SOURCE
# plumes seen at the moment of an overpass, above a detection limit of roughly 100 kg/hr
# for EMIT and Tanager (lower for the airborne GAO). The campaign estimate in this paper
# is a whole-city flux including diffuse and area sources. The sum of detected plumes is
# therefore a LOWER BOUND on the city total, and the absence of a detection at a facility
# is not evidence that the facility does not emit. Read it as: which discrete sources are
# big enough to see, how often, and how large they are relative to the city total.
# -----------------------------------------------------------------------------
set -uo pipefail
cd "$(dirname "$0")/.."
OUT="${METHANE_OUT_DIR:-$(cd .. && pwd)/outputs}"
mkdir -p "$OUT"
# west south east north — the URBAN_BOX of config.R
read -r W S E N <<< "${BOX:--105.20 39.50 -104.55 39.95}"
API="https://api.carbonmapper.org/api/v1/catalog/plumes/annotated"
URL="$API?bbox=$W&bbox=$S&bbox=$E&bbox=$N&limit=500"

echo "box: $W $S $E $N"
curl -fsSL "$URL" -o "$OUT/.carbonmapper_raw.json" || { echo "fetch failed"; exit 1; }

python3 - "$OUT" <<'PY'
import json, sys, csv, datetime, statistics as st
from collections import defaultdict
out = sys.argv[1]
j = json.load(open(out + "/.carbonmapper_raw.json"))
items = j.get("items", [])
fetched = datetime.date.today().isoformat()
rows = []
for p in items:
    lon, lat = p["geometry_json"]["coordinates"]
    rows.append(dict(plume_id=p["plume_id"], gas=p["gas"], sector=p.get("sector"),
                     date=p["scene_timestamp"][:10], platform=p["platform"],
                     instrument=p["instrument"], lat=round(lat, 4), lon=round(lon, 4),
                     kg_hr=(None if p.get("emission_auto") is None else round(p["emission_auto"])),
                     kg_hr_unc=(None if p.get("emission_uncertainty_auto") is None
                                else round(p["emission_uncertainty_auto"])),
                     wind_m_s=p.get("wind_speed_avg_auto"), fetched=fetched))
rows.sort(key=lambda r: r["date"], reverse=True)
path = out + "/carbonmapper_denver_plumes.csv"
with open(path, "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=list(rows[0].keys())); w.writeheader(); w.writerows(rows)
print(f"wrote {path}  ({len(rows)} plumes, in-box count reported by the API: {j.get('bbox_count')})")

ch4 = [r for r in rows if r["gas"] == "CH4"]
q = [r["kg_hr"] for r in ch4 if r["kg_hr"] is not None]
print(f"\n{len(ch4)} CH4 plumes, {len(q)} with a quantified rate, "
      f"{min(r['date'] for r in rows)} to {max(r['date'] for r in rows)}")
if q:
    print(f"  rate kg/hr: min {min(q)}  median {st.median(q):.0f}  max {max(q)}")

# IPCC sector codes as Carbon Mapper tags them: 6A solid waste, 4D wastewater,
# 1B2 oil and gas fugitive, 1B1 coal, 4A/3A agriculture, 1A1 energy industries.
bysec = defaultdict(list)
for r in ch4: bysec[r["sector"] or "unlabelled"].append(r)
print("\nby sector:")
for s, v in sorted(bysec.items(), key=lambda kv: -len(kv[1])):
    qq = [r["kg_hr"] for r in v if r["kg_hr"] is not None]
    print(f"  {s:<12} {len(v):3d} plume(s)" + (f", median {st.median(qq):.0f} kg/hr" if qq else ""))

# Cluster by 0.04 deg (~4 km) so repeat visits to one facility group together
# (a large landfill spreads detections over a couple of km).
cl = defaultdict(list)
for r in ch4: cl[(round(r["lat"]/0.04), round(r["lon"]/0.04))].append(r)
print("\nby site (plumes within ~4 km grouped):")
for k, v in sorted(cl.items(), key=lambda kv: -len(kv[1])):
    qq = [r["kg_hr"] for r in v if r["kg_hr"] is not None]
    la = st.mean([r["lat"] for r in v]); lo = st.mean([r["lon"] for r in v])
    line = f"  {la:.3f}, {lo:.3f}  n={len(v):2d}  sectors={sorted({r['sector'] or '?' for r in v})}"
    if qq: line += f"  median {st.median(qq):.0f}, max {max(qq)} kg/hr"
    print(line)
    print(f"      dates: {', '.join(sorted({r['date'] for r in v}))}")

# What a single overpass sees, which is the only number comparable to a flux estimate.
byday = defaultdict(float)
for r in ch4:
    if r["kg_hr"]: byday[r["date"]] += r["kg_hr"]
if byday:
    tot = sorted(byday.values())
    print(f"\nper-overpass detected total (t CH4/hr): min {min(tot)/1000:.2f}, "
          f"median {st.median(tot)/1000:.2f}, max {max(tot)/1000:.2f}  over {len(byday)} dates")
    print("Compare with emission_estimates.csv (campaign urban flux) and with the")
    print("waste sector of the inventories in inventory_sector_compare.csv.")
PY

# ---- source-level table -------------------------------------------------------------
# A SOURCE is a DBSCAN cluster of plumes at one location; a PLUME is one overpass of it.
# Carbon Mapper's source emission rate is persistence-weighted -- it already carries the
# overpasses that saw nothing, which a plume rate does not. For the Cherokee CO2 source
# the portal shows 26.4 t/hr against the plume's 237, a factor of exactly 9.
#
# ENDPOINT. It is catalog/sources.geojson, NOT "sources/annotated" (which does not exist;
# an earlier version of this script guessed that by analogy with plumes/annotated and got
# a 404). Parameters come from the API's own schema at /api/v1/openapi.json. `eps` and
# `minpoints` are the DBSCAN clustering radius (m) and minimum cluster size: the clustering
# is a QUERY PARAMETER, so a "source" is a spatial cluster at the radius you ask for, not a
# facility boundary. Left at the server default here, and the radius used is recorded in
# each source id ({gas}_{sector}_{eps}m_{lon}_{lat}).
SURL="https://api.carbonmapper.org/api/v1/catalog/sources.geojson?bbox=$W&bbox=$S&bbox=$E&bbox=$N"
if curl -fsSL "$SURL" -o "$OUT/.carbonmapper_sources.json"; then
python3 - "$OUT" <<'PY'
import json, sys, csv, datetime
out = sys.argv[1]
j = json.load(open(out + "/.carbonmapper_sources.json"))
feats = j.get("features", j if isinstance(j, list) else [])
fetched = datetime.date.today().isoformat()

rows = []
for f in feats:
    props = dict(f.get("properties") or {})
    geom = f.get("geometry") or {}
    coords = geom.get("coordinates")
    lon = lat = None
    if isinstance(coords, (list, tuple)) and len(coords) >= 2 and \
       all(isinstance(c, (int, float)) for c in coords[:2]):
        lon, lat = coords[0], coords[1]
    props.setdefault("id", f.get("id"))
    props["geom_lon"] = round(lon, 5) if lon is not None else None
    props["geom_lat"] = round(lat, 5) if lat is not None else None
    props["fetched"] = fetched
    rows.append(props)

if not rows:
    print("\nsources.geojson returned no features for this box.")
else:
    # Take every property the API returns rather than a hand-picked list, so a field we
    # have not seen before shows up as a column instead of being silently dropped.
    cols, seen = [], set()
    for r in rows:
        for k in r:
            if k not in seen:
                seen.add(k); cols.append(k)
    path = out + "/carbonmapper_denver_sources.csv"
    with open(path, "w", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=cols, extrasaction="ignore")
        w.writeheader()
        for r in rows:
            w.writerow({k: r.get(k) for k in cols})
    print(f"\nwrote {path}  ({len(rows)} sources, {len(cols)} columns)")
    print("source fields returned by the API:")
    print("  " + ", ".join(cols))

    # Print each source with whatever rate/count fields exist, without assuming names.
    def pick(r, *names):
        for n in names:
            if r.get(n) not in (None, ""):
                return r[n]
        return None
    print("\nsources in the box:")
    for r in sorted(rows, key=lambda r: -(pick(r, "emission_auto", "emission", "rate") or 0)):
        rate = pick(r, "emission_auto", "emission", "rate")
        unc  = pick(r, "emission_uncertainty_auto", "emission_uncertainty")
        npl  = pick(r, "plume_count", "n_plumes", "count")
        name = pick(r, "source_name", "name", "facility_name") or "(unnamed)"
        gas  = pick(r, "gas", "plume_gas") or "?"
        sec  = pick(r, "sector") or "?"
        print(f"  {str(name)[:34]:<34} {gas:<4} {sec:<5} "
              f"{(f'{float(rate):9.0f}' if rate is not None else '        -')} kg/hr"
              f"{f' +/- {float(unc):.0f}' if unc is not None else ''}"
              f"  plumes={npl}  {r.get('geom_lat')}, {r.get('geom_lon')}")
        print(f"      id: {r.get('id')}")

    # ---- persistence-weighted totals ------------------------------------------------
    # persistence = detection_date_count / observation_date_count, and emission_auto
    # ALREADY has it applied: it is the mean per-overpass detected total scaled by how
    # often the source is seen emitting. Verified against the plume table on 12 Sep 2026:
    # for three single/two-plume sources the identity is exact (e.g. 848.0 x 1/10 = 84.80,
    # 237493 x 1/9 = 26388.1) and for Tower Rd it reproduces to 0.02% using per-date sums.
    # So these are time-averaged rates and are the ones to place beside a campaign flux;
    # the per-overpass plume sums above are instantaneous and are NOT the same quantity.
    def num(r, k):
        try: return float(r.get(k))
        except (TypeError, ValueError): return None
    ch4 = [r for r in rows if (r.get("gas") == "CH4") and num(r, "emission_auto") is not None]
    if ch4:
        tot = sum(num(r, "emission_auto") for r in ch4)
        lf  = sum(num(r, "emission_auto") for r in ch4 if r.get("sector") == "6A")
        og  = sum(num(r, "emission_auto") for r in ch4 if r.get("sector") == "1B2")
        unq = [r for r in rows if r.get("gas") == "CH4" and num(r, "emission_auto") is None]
        print(f"\npersistence-weighted CH4 point sources: {tot:.1f} kg/hr = {tot/1000:.3f} t/hr"
              f"  ({len(ch4)} quantified, {len(unq)} detected but unquantified)")
        if tot:
            print(f"  landfill (6A) {lf:.1f} kg/hr ({100*lf/tot:.0f}%)   "
                  f"oil and gas (1B2) {og:.1f} kg/hr ({100*og/tot:.0f}%)")
        print("  persistence by source (detections / clear observations):")
        for r in sorted(ch4, key=lambda r: -(num(r, "persistence") or 0)):
            print(f"    {r.get('geom_lat')}, {r.get('geom_lon')}  {r.get('sector'):<4}"
                  f"  {num(r,'persistence') or 0:.2f}"
                  f"  ({r.get('detection_date_count')}/{r.get('observation_date_count')})"
                  f"  {num(r,'emission_auto'):.0f} kg/hr")
        print("  A persistence near 1 is a chronic emitter; near 0.1 is an intermittent one.")
        print("  Compare tot/1000 with the campaign flux in emission_estimates.csv, and the")
        print("  6A subtotal with the waste sector in inventory_sector_compare.csv.")
PY
else
  echo "(sources.geojson unavailable; plume table written anyway)"
fi

rm -f "$OUT/.carbonmapper_raw.json" "$OUT/.carbonmapper_sources.json"
