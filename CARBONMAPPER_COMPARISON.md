# Carbon Mapper plumes over the Denver box — comparison with the campaign estimate

Fetched 12 Sep 2026 from `api.carbonmapper.org` for the analysis box
(39.50-39.95 N, -105.20 to -104.55 W), the same box used for every number in the paper.
Reproduce with `bash scripts/43_carbonmapper_plumes.sh`; the snapshot used here is
`results/carbonmapper_denver_plumes.csv`.

## What is in the catalog

31 CH4 plumes and one CO2 plume, 18 Jul 2021 to 8 Apr 2026, from three platforms:
the airborne Global Airborne Observatory (GAO), EMIT on the ISS, and Tanager-1.
24 CH4 plumes carry a quantified rate; the rest are detections without one.

| site | n | sector | median | max | dates |
|---|---|---|---|---|---|
| 39.85 N, -104.76 W (landfill, Tower Rd / Commerce City) | 18 | 6A solid waste | 309 kg/hr | 1292 kg/hr | 15 dates, 2021-2026 |
| 39.66 N, -104.69 W (DADS landfill, Aurora) | 9 | 6A solid waste | 118 kg/hr | 224 kg/hr | 6 dates, 2021-2023 |
| 39.93 N, -104.81 W | 3 | 1B2 oil and gas | 224 kg/hr | 305 kg/hr | 2021, 2023 |
| 39.91 N, -105.05 W | 1 | 1B2 oil and gas | 848 kg/hr | | 2026-04-08 |
| 39.807 N, -104.964 W (Cherokee Generating Station site) | 1 | 1A1 energy, **CO2** | 237 t CO2/hr (plume) | | 2023-06-25 |

## Three things this says

**1. The discrete methane sources over Denver are landfills, not the refinery.**
27 of 31 CH4 plumes are tagged solid waste (6A) and they sit at two landfills. Four are
oil-and-gas fugitive (1B2), and **Suncor has no detection at all, of any gas**. The one
non-landfill point source in the box is CO2, not methane, and it is not the refinery: it
sits at 39.80738, -104.96386 — the Cherokee Generating Station site, Xcel's gas-fired plant
on the west bank of the South Platte, about 2 km west of Suncor. Sector 1A1 (energy
industries) fits a power plant, not a refinery. (The facility name is ours, from the
coordinate; Carbon Mapper supplies only the location and sector — see the caveats.) That is
an independent line of evidence for the paper's biogenic-dominated conclusion, from remote
sensing rather than from an ethane tracer, and it comes with facility-level location, which
the flight geometry cannot give.

**2. Point sources are a small part of the city total — about 8 percent.** The right
quantity to put beside a campaign flux is the persistence-weighted source rate, which
averages each source over the overpasses that saw nothing as well as those that saw a
plume. Summed over the four quantified CH4 sources that is **0.640 t CH4/hr**, against the
campaign urban estimate of 4.6 to 10.7 t/hr (median 7.6, `emission_estimates.csv`):
**8.4 percent at the median, 6 to 14 percent across the range.** The instantaneous
per-overpass plume sums (0.10 to 1.29 t/hr, median 0.44) give a similar picture by a
different construction. Denver's methane is not one or two super-emitters; it is
distributed, which is why an enhancement-ratio method over the whole box gives a much
larger number than a plume inventory.

Of that 0.640 t/hr, **82 percent is the two landfills** (0.523 t/hr) and 18 percent is
oil-and-gas fugitive (0.117 t/hr).

**3. GRA2PES v2's waste sector looks high, independently.** v2 puts 12.197 t CH4/hr of
waste emissions in this box (`gra2pes_sector_v1_vs_v2.csv`), against EPA gridded GHGI
0.867 and v1.1 exactly 0.000. The persistence-weighted landfill observation is 0.523 t/hr.
So the satellite-observed landfill total is **60 percent of what EPA assigns to the whole
waste sector** — two independent estimates agreeing within a factor of two — while v2 is
**23 times the observation and 14 times EPA**. Carbon Mapper cannot see diffuse whole-site
emissions well, so this is not a ceiling; but for v2 to be right the unseen diffuse part
would have to be roughly 23 times everything the instruments do see, and the campaign
measurement of *all* Denver methane (4.6-10.7 t/hr) would have to be low by a factor of
two, since v2's waste sector alone exceeds it.

## Caveats that belong in any sentence written from this

- **Detection limit.** Roughly 100 kg/hr for EMIT and Tanager, lower for GAO. Anything
  smaller, and anything diffuse, is invisible. The sum of detected plumes is a lower
  bound on the city total, never an estimate of it.
- **Snapshots, not averages.** Each value is an instantaneous rate at one overpass under
  clear-sky, adequate-wind conditions. Landfill emissions vary with cover, weather and
  gas-collection operation; the 24-1292 kg/hr spread at one site is that variability.
- **No temporal overlap with AMMBEC.** No plume falls in the 28 Jun - 13 Jul 2024 flight
  window. The nearest are EMIT on 17 Aug 2024 (1292 kg/hr) and 21 Aug 2024 (303 kg/hr),
  both at the Tower Rd landfill, five to six weeks after the campaign.
- **Absence is not absence of emission.** Metro Water Recovery (Robert W. Hite), the
  wastewater plant this analysis keeps returning to, has no detection in the catalog.
  Wastewater methane is typically diffuse and spread across basins, which is the kind of
  source these instruments are worst at, so this neither supports nor contradicts the
  1.6 t/hr the inventory-proportional attribution assigns to wastewater.
- **Sector labels are Carbon Mapper's**, assigned by proximity to known infrastructure,
  not verified here.
- **The API returns no facility name at all.** `source_name` is the machine identifier
  (`CH4_6A_1000m_-104.75752_39.85168`), not a plant. The portal's own label for the CO2
  source is "Denver, Colorado, US"; "Cherokee Generating Station" is the *basemap* label
  under the marker. So every site name in this file is ours, from matching coordinates to
  infrastructure by hand, and should be written as such. What the data supports without
  interpretation is the coordinate: 39.80738, -104.96386, which is Cherokee's site and
  about 2 km west of Suncor.
- **Plume rate is not source rate.** The site table above is `emission_auto` per plume:
  the instantaneous rate at one overpass. The source endpoint returns a persistence-weighted
  rate instead, and the section below uses those. `scripts/43` now pulls both.
- **"Source" is a clustering result, not a facility.** `eps` (radius, m) and `minpoints` are
  query parameters, so sources are DBSCAN clusters computed per request; the radius is
  recorded in each id. Denver comes back with the two landfills clustered at 1000 m and the
  smaller sources at 250 m. Any source-level number quoted in the paper needs its radius
  stated, the way a grid resolution would be.

## How the persistence weighting works (verified, 12 Sep 2026)

`persistence = detection_date_count / observation_date_count`, and `emission_auto` on a
source **already has it applied**: it is the mean per-overpass detected total multiplied by
that fraction. Checked against the plume table: exact for the three sources whose member
plumes are all quantified (848.0 x 1/10 = 84.80; 237,493 x 1/9 = 26,388.1; 224.5 x 2/14 =
32.07) and reproducing to 0.02 percent for Tower Rd using per-date sums (532.70 x 15/19 =
420.5). DADS sits about 4 percent off, which is what a member plume without a published
rate does to the reconstruction — the identity is theirs, not ours, so this is a check on
our reading of it rather than a derivation.

| source | sector | persistence | source rate |
|---|---|---|---|
| 39.852, -104.758 (Tower Rd landfill) | 6A | **0.79** (15/19) | 420 kg/hr |
| 39.662, -104.690 (DADS landfill) | 6A | **0.67** (6/9) | 102 kg/hr |
| 39.936, -104.804 | 1B2 | 0.14 (2/14) | 32 kg/hr |
| 39.909, -105.053 | 1B2 | 0.10 (1/10) | 85 kg/hr |
| 39.929, -104.810 | 1B2 | 0.07 (1/14) | unquantified |
| 39.807, -104.964 (Cherokee site) | 1A1, CO2 | 0.11 (1/9) | 26.4 t CO2/hr |

This is the one thing here the seven flights cannot provide: **the landfills are chronic,
seen on 79 and 67 percent of clear overpasses, while every oil-and-gas source is
intermittent at 7 to 14 percent.** A campaign flux is a handful of days; this is a
five-year duty cycle at the same locations, and it says the waste sources are on
essentially all the time.

## A second reading of EMIT: MAPL-EMIT (Google Research / Nature Trace)

`projects/nature-trace/assets/ghg/emit/mapl_emit_plumes_v1_0` in Earth Engine is a
vision-transformer detector run over full EMIT radiance granules (Aug 2022 - Jun 2026,
60 m). It publishes plume complexes with a column enhancement field (ppm-m), an instance
mask, a source location and a confidence label.

It is the SAME instrument as the `emi` rows above, read by a different algorithm: Carbon
Mapper's EMIT plumes come from the matched-filter product with human review, MAPL-EMIT
from an automated model tuned for recall. So it is a recall and persistence check, not an
independent measurement. Two things it can add:


## No spatial overlap with the aircraft ethane map (corrected 12 Sep 2026)

`scripts/44_carbonmapper_overlay.R` places the plumes on the Figure 4B ethane-signature
grid. After script 25 gained a leverage gate, the result is unambiguous: **no Carbon Mapper
plume falls inside a coloured cell.** Both landfill clusters lie east of the flight tracks,
and the four oil-and-gas plumes that previously appeared to sit in fossil-leaning cells
were landing in cells that should not have been coloured — 80 of 106 grid cells hold too
narrow a range of methane enhancement to constrain a slope, and 16 carried a York slope
outside the physical range, clamping to a saturated 0% or 100%.

The earlier statement that those four plumes sat in cells with a median fossil fraction of
0.67 against 0.39 overall, offered as "a small but independent consistency check", **is
withdrawn.** It was an artefact of unconstrained cells. There is no spatial agreement
between the two datasets to report, in either direction — only a coverage gap.

- plumes over Denver that the matched filter missed, which speaks to how OFTEN a facility
  emits — the question the seven flights cannot answer;
- a plume footprint rather than a point, which can be placed against the 0.02 deg cells of
  the ethane signature grid.

What it cannot do: it reports enhancements, not emission rates. Converting to kg/hr needs
an IME calculation with a wind field and adds its own uncertainty, so nothing from it is
directly comparable to the 4.6-10.7 t/hr campaign estimate or to the inventory sectors —
Carbon Mapper stays the quantitative source. Medium-confidence plumes carry a 50-55%
false-positive rate by the producers' own review (high confidence: 3-5%), and the model
misses about 16% of expert-annotated NASA EMIT L2B plumes.

`scripts/46_mapl_emit_rgee.R` pulls these plumes into R with **rgee**, writes
`mapl_emit_denver_plumes.csv`, and joins each one to its nearest Carbon Mapper detection
(`mapl_vs_carbonmapper.csv`), so the whole comparison stays in R with the rest of the
pipeline:

```
Rscript scripts/46_mapl_emit_rgee.R
```

One-time setup: `install.packages("rgee"); rgee::ee_install()`, then
`rgee::ee_Initialize(project = "ee-priyankadesouza")` — Earth Engine sign-in is
interactive and browser-based, so no credential is stored in this repository. Override the
project with `EE_PROJECT=...`. `scripts/46_mapl_emit_gee.js` is the same query as a Code
Editor snippet, kept for the map view.
