"""95_gra2pes_v2_region.py -- GRA2PES v2.0beta methane (HC01) over the STILT domain, by sector.

Reads the extracted July 2023 weekday files of the v2.0beta total and of the OG, RES, WASTE and
AG sectors (the same trees script 57 uses), sums the vertical levels, averages the 24 hourly
slices and writes one row per 4-km cell inside 37.5-42.5 N, 108.5-101.5 W (the STILT domain of
script 64), in mol km-2 h-1. Script 64 maps it onto a 0.02-degree grid and convolves it with
the STILT footprints.

Needs python3 with numpy and netCDF4. Runs in about 30 s.

usage: python3 scripts/95_gra2pes_v2_region.py <GRA2PES dir> <out csv>
  e.g. python3 scripts/95_gra2pes_v2_region.py ../inventories/GRA2PES ../outputs/gra2pes_v2.0beta_ch4_region_weekdy.csv
"""
import sys, os, csv, time
import numpy as np, netCDF4

G, DST = sys.argv[1], sys.argv[2]
YMN, XMN, YMX, XMX = 37.5, -108.5, 42.5, -101.5
SRC = {"total": "v2total/202307/weekdy/GRA2PESv2.0beta_total_202307_weekdy_%s.nc",
       "OG": "sectors/v2.0beta/OG/202307/weekdy/GRA2PESv2.0beta_OG_202307_weekdy_%s.nc",
       "RES": "sectors/v2.0beta/RES/202307/weekdy/GRA2PESv2.0beta_RES_202307_weekdy_%s.nc",
       "WASTE": "sectors/v2.0beta/WASTE/202307/weekdy/GRA2PESv2.0beta_WASTE_202307_weekdy_%s.nc",
       "AG": "sectors/v2.0beta/AG/202307/weekdy/GRA2PESv2.0beta_AG_202307_weekdy_%s.nc"}
out, t0, idx = {}, time.time(), None
for k, pat in SRC.items():
    acc, nh = None, 0
    for half in ("00to11Z", "12to23Z"):
        nc = netCDF4.Dataset(os.path.join(G, pat % half))
        if idx is None:
            lat = nc.variables["lat"][:]; lon = nc.variables["lon"][:]
            m = (lat >= YMN) & (lat <= YMX) & (lon >= XMN) & (lon <= XMX)
            ii, jj = np.where(m); idx = (ii.min(), ii.max() + 1, jj.min(), jj.max() + 1)
            out["lat"] = np.asarray(lat[idx[0]:idx[1], idx[2]:idx[3]]); out["lon"] = np.asarray(lon[idx[0]:idx[1], idx[2]:idx[3]])
        i0, i1, j0, j1 = idx
        a = np.asarray(nc.variables["HC01"][:, :, i0:i1, j0:j1], dtype="f8").sum(axis=1)   # levels summed -> (time, y, x)
        acc = a.sum(axis=0) if acc is None else acc + a.sum(axis=0); nh += a.shape[0]
        nc.close()
    assert nh == 24, (k, nh)
    out[k] = acc / nh
    print(f"{time.time() - t0:.0f}s {k}: 24 h read", flush=True)
with open(DST, "w", newline="") as f:
    w = csv.writer(f); w.writerow(["lat", "lon", "total", "OG", "RES", "WASTE", "AG"])
    for y in range(out["lat"].shape[0]):
        for x in range(out["lat"].shape[1]):
            w.writerow([round(float(out["lat"][y, x]), 4), round(float(out["lon"][y, x]), 4)] +
                       [round(float(out[k][y, x]), 3) for k in ("total", "OG", "RES", "WASTE", "AG")])
box = (out["lat"] >= 39.5) & (out["lat"] <= 39.95) & (out["lon"] >= -105.2) & (out["lon"] <= -104.55)
print(f"wrote {DST} {out['lat'].shape}; box total {out['total'][box].sum() * 16 * 16.04 / 1e6:.2f} t CH4/h (expect 16.5)")
