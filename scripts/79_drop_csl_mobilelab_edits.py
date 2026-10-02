"""79_drop_csl_mobilelab_edits.py -- remove the NOAA CSL Mobile Laboratory data from the paper.

The CSL Mobile Laboratory drive of 9 July 2024 was a test drive: the GPS and several instruments
were not working well, the files were never finalized (revision RA) and the laboratory carried no
ethane. On the instrument team's advice (C. Womack, NOAA CSL, October 2026) the data are left out
of the paper; the ground-based ethane context comes from the NOAA ARL Air Resources Car alone.

  Main text  section 2.3 retitled and rewritten around the car; the clause tying the car's 9 July
             route to the laboratory removed; Limitations and Data availability updated.
  SI         section S2.2 (the laboratory transect) deleted, so S2.3 becomes S2.2 in both documents;
             the laboratory comparison and the shared-route sentences removed from the car section;
             the S2 introduction rewritten, because the car drove within the flight window and its
             ambient ratio is compared with the aircraft (it was written when S2 held only
             non-coincident data); the 0.0813 production-area ratio attributed to the car by name.

Stages 07 (laboratory merge) and 77 (route overlap) leave run_all.R; see REPRODUCIBILITY.md.
Deleted text is removed outright (there is nothing left to colour); inserted text is red. The
one number written here (the 39.80 N metropolitan line) is read from arc_ground_summary.csv.

usage: python3 scripts/79_drop_csl_mobilelab_edits.py <main.docx> <si.docx> <main_out> <si_out> --outputs <OUT_DIR>
"""
import sys, csv, re, argparse, tempfile, os
from pathlib import Path
sys.path.insert(0, str(Path(__file__).resolve().parent))
import docx
import docx_edit_lib as L
from docx_edit_lib import R, W, runs_text, all_paragraphs

ap = argparse.ArgumentParser(); ap.add_argument("main_in"); ap.add_argument("si_in"); ap.add_argument("main_out"); ap.add_argument("si_out")
ap.add_argument("--outputs", required=True)
a = ap.parse_args(); OUT = Path(a.outputs); L.OUTDIR = str(OUT)

ARC = {r["quantity"]: r["value"] for r in csv.DictReader(open(OUT / "arc_ground_summary.csv"))}
MLAT = f"{float(ARC['metro_lat']):.2f}"

# ---- SI: delete section S2.2 (heading through the paragraph before the S2.3 heading) ---------------
si = docx.Document(a.si_in); ps = si.paragraphs
i0 = [i for i, p in enumerate(ps) if p.text.strip().startswith("S2.2 CSL mobile-laboratory transect")]
i1 = [i for i, p in enumerate(ps) if p.text.strip().startswith("S2.3 NOAA ARL Air Resources Car")]
assert len(i0) == 1 and len(i1) == 1 and i0[0] < i1[0], "SI sections S2.2/S2.3 not found as expected (already run?)"
gone = ps[i0[0]:i1[0]]
assert not any(p._p.xpath(".//w:drawing") for p in gone), "the S2.2 block holds a figure; check before deleting"
for p in gone: p._p.getparent().remove(p._p)
print(f"SI: deleted section S2.2 ({len(gone)} paragraphs, incl. blanks)")
tmp_si = Path(tempfile.mkdtemp()) / "si_tmp.docx"; si.save(tmp_si)

# ---- main text ----------------------------------------------------------------------------------
W("2.3. Mobile laboratory transect", "2.3. Ground-based ethane measurements")
R("The NOAA CSL Mobile Laboratory conducted a ~56 km Front Range transect on 9 July 2024, traveling from Boulder "
  "south along the foothills toward Golden (39.52 to 40.03°N). A Picarro analyzer measured CH4, CO2, CO, and H2O "
  "alongside navigation and meteorological variables. Because the mobile laboratory did not measure ethane, these "
  "data cannot be used for source apportionment and are included only to provide qualitative source context. "
  "A second ground platform, the NOAA ARL Air Resources Car, drove",
  "The NOAA Air Resources Laboratory (ARL) Air Resources Car drove")
R(f"{MLAT}°N, partly along the foothills roads the CSL laboratory had driven about 11 minutes earlier, and",
  f"{MLAT}°N, and")
R("Instrumentation, data merging, and record counts for both ground platforms are described in section S2.",
  "The car's instrumentation, calibration screening, and plume analysis are described in section S2.2.")
R("The CDPHE facility surveys and CSL mobile-laboratory transect are neither spatially nor temporally coincident "
  "with the airborne observations. We therefore use them only to characterize the source types contributing to the "
  "urban methane field, not to constrain the airborne source fractions or emission rates.",
  "The CDPHE facility surveys are neither spatially nor temporally coincident with the airborne observations, and "
  f"the ARL car, although it drove within the flight window, sampled metropolitan Denver south of {MLAT}°N "
  "appreciably on only one day. We therefore use the ground data to characterize the source types contributing to "
  "the urban methane field and as a check on the airborne enhancement ratio and the ethane endmember, not to "
  "constrain the airborne source fractions or emission rates.")
R("The ground-based data are the NOAA CSL Mobile Laboratory files for 9 July 2024 and the NOAA Air Resources "
  "Laboratory Air Resources Car files",
  "The ground-based ethane data are the NOAA Air Resources Laboratory Air Resources Car files")
R("[archive location and data-use statement to be confirmed with the PIs]",
  "[archive location and data-use statement to be confirmed with the PI]")

L.apply(a.main_in, a.main_out, "main", "main text")

# The shared-route passage runs to "(script 77)."; cut it as one span from the paragraph text so the
# numbers in it (written by script 78) need not be retyped here.
# ---- SI -----------------------------------------------------------------------------------------
si = docx.Document(tmp_si)
hit = [p for p in all_paragraphs(si) if "drove partly the same roads" in runs_text(p)]
assert len(hit) == 1, "shared-route passage not found"
t = runs_text(hit[0]); s = t.index("On 9 July the car and the CSL Mobile Laboratory"); e = t.index("(script 77).", s) + len("(script 77).")
span = t[s - 1:e]                                   # include the space before it
assert span.startswith(" On 9 July") and "CSL" in span
si.save(tmp_si)
R(span, "", "si")
W("The ground-based datasets below are neither spatially nor temporally coincident with the aircraft",
  "The CDPHE facility surveys (section S2.1) are neither spatially nor temporally coincident with the aircraft "
  "(the closest same-day aircraft/survey approach in the campaign was 11.4 km, and the surveys span 2023 to 2025, "
  "mostly outside the July 2024 flight window), and are used only to characterize the facility source types that "
  "populate the airborne source mix. The NOAA ARL Air Resources Car (section S2.2) drove within the flight window "
  "and measured ethane; its data are used as a check on the airborne enhancement ratio and on the ethane endmember, "
  "but not to compute the urban fossil fraction or any emission rate.", "si")
R("Unlike the CSL mobile laboratory these data carry ethane and", "These data carry ethane and", "si")
R("measured by the NOAA Air Resources Mobile Lab during this campaign",
  "measured by the NOAA ARL Air Resources Car during this campaign (section S2.2)", "si")
L.apply(tmp_si, a.si_out, "si", "SI")

# ---- renumber S2.3 -> S2.2 in both documents (the old S2.2 no longer exists) ---------------------
for path, label in ((a.main_out, "main text"), (a.si_out, "SI")):
    d = docx.Document(path); n = 0
    for p in all_paragraphs(d):
        assert "S2.2 CSL" not in runs_text(p)
        while "S2.3" in runs_text(p):
            L.replace_in_paragraph(p, "S2.3", "S2.2"); n += 1
    left = [runs_text(p)[:80] for p in all_paragraphs(d) if re.search(r"CSL Mobile|mobile laboratory|mobile-laboratory|script 77", runs_text(p))]
    assert not left, f"{label}: laboratory text remains: {left}"
    d.save(path); print(f"{label}: {n} reference(s) S2.3 -> S2.2")
os.remove(tmp_si)
