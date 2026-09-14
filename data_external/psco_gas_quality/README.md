# Public Service Company of Colorado (Xcel Energy) — Colorado Monthly Gas Quality Report

Archived copies of the utility's monthly gas-quality reports used for the Denver
delivered-gas C2H6:CH4 endmember (manuscript section 3.2, SI section S3, Table S8).

| file | content |
|---|---|
| PSCo_Monthly_Gas_Quality_Report_2018.pdf | Jan–Sep 2018, all zones (PDF export of "PSCO Zone - 2018.xlsx") |
| PSCo_Monthly_Gas_Quality_Report_2023.pdf | Jan–Dec 2023, all zones |
| PSCo_Monthly_Gas_Quality_Report_2024.pdf | Jan–Dec 2024, all zones |
| PSCo_Monthly_Gas_Quality_Report_2025.pdf | Jan–Dec 2025, all zones |

Source: Xcel Energy gas transportation customer pages for PSCo
(https://corporate.my.xcelenergy.com/s/gas-transport/psco, "Gas Quality" reports),
downloaded 14 September 2026. Values are volume-weighted averages of all supplies
into each zone (ASTM D3588; GPA 2145); the reports state they may not represent
deliveries to a specific location at a given time.

The reports were transcribed to `../../psco_gas_quality.csv` (one row per zone and
month: methane, ethane and propane mol %, gross heating value, specific gravity, and
the derived C2H6:CH4 molar ratio). Scripts 52 and 50 read that CSV; config.R carries
the Denver-zone June–July 2024 value (0.110) and the 2023–2025 monthly range
(0.095–0.141) with the same provenance.
