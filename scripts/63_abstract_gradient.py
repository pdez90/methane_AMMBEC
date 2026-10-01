"""63_abstract_gradient.py -- elevate the regional fossil-to-biogenic gradient into the abstract
(co-author request, 16 Sep 2026), with offsetting trims so the abstract does not grow.

Two variants:
  --variant keep250 (default)  minimal change to the author's abstract; result 249 words
  --variant est200             abstract rewritten to the ES&T Research Article limit (150-200
                               words; result 198), keeping the source-mix, gradient, rate and
                               inventory-comparison messages

Edits are exact-anchor replacements written in red (docx_edit_lib.py); the script stops without
writing if an anchor is missing or duplicated.

usage: python3 scripts/63_abstract_gradient.py <main.docx> <main_out> [--variant keep250|est200]
"""
import sys, argparse
from pathlib import Path
sys.path.insert(0, str(Path(__file__).resolve().parent))
import docx_edit_lib as L
from docx_edit_lib import R, W

ap = argparse.ArgumentParser(); ap.add_argument("main_in"); ap.add_argument("main_out")
ap.add_argument("--variant", choices=["keep250", "est200"], default="keep250")
a = ap.parse_args()

GRADIENT = ("Together with source apportionment in the adjacent basin, these results place Denver toward the "
            "biogenic end of a regional fossil-to-biogenic gradient from the producing basin into the metropolitan area.")

if a.variant == "keep250":
    # trims that pay for the new sentence (238 -> 249 words, under the 250-word working cap)
    R("campaign, flown 28 June to 13 July over Denver and the adjacent Denver-Julesberg Basin.",
      "campaign, flown over Denver and the adjacent Denver-Julesberg Basin.")
    R("Across 8 flights with adequate urban coverage, 7 yielded estimable urban ethane-methane slopes.",
      "Seven of the 8 flights with adequate urban coverage yielded estimable urban ethane-methane slopes.")
    R("Using an effective fossil endmember of 0.102 mol mol-1, the inferred median fossil fraction ranged from 2 to 47%, with a campaign median of 29%.",
      "At an effective fossil endmember of 0.102 mol mol-1, the median fossil fraction was 29% (2 to 47% across flights).")
    R("Across contemporary Denver-relevant constraints on the fossil endmember (0.061-0.110 mol mol-1), the median ranged from 27 to 48% "
      "and would exceed 50% only for effective endmembers below 0.059 mol mol-1. Previous airborne urban studies reported fossil fractions of 40 to 88%.",
      "Across contemporary Denver-relevant endmembers (0.061-0.110 mol mol-1), the median ranged from 27 to 48% and would exceed 50% only "
      "below 0.059 mol mol-1, whereas previous airborne urban studies reported 40 to 88%. " + GRADIENT)
else:
    W("A city's methane emissions arise from its natural-gas system",
      "Urban methane arises from the natural-gas system and from biogenic sources such as landfills, wastewater, and agriculture. "
      "Fossil methane co-emits ethane and biogenic methane does not, so airborne ethane-to-methane ratios separate them. "
      "We apply this tracer to the 2024 Airborne Methane Mass Balance Emissions in Colorado (AMMBEC) campaign over Denver and the "
      "adjacent Denver-Julesberg Basin. At an effective fossil endmember of 0.102 mol mol-1, the median urban fossil fraction was 29% "
      "(2 to 47% across flights); across contemporary Denver-relevant endmembers (0.061-0.110 mol mol-1) it ranged from 27 to 48%, "
      "exceeding 50% only below 0.059 mol mol-1, whereas previous airborne urban studies reported 40 to 88%. " + GRADIENT +
      " Scaling the CH4:CO enhancement ratio by a gridded CO inventory gave 4.6 to 10.7 t CH4 per hour; these anchor-dependent rates "
      "exceed two box-consistent bottom-up inventories (1.7 and 2.8 t CH4 per hour) by 2.7 to 4.5-fold at the median, and the airborne "
      "fossil fraction is below the EPA inventory's ~60%. Dedicated urban flights and isotopic measurements are needed to close "
      "Denver's methane budget.")

L.apply(a.main_in, a.main_out, "main", "main text")

import docx
words = len(docx.Document(a.main_out).paragraphs[2].text.split())
print(f"abstract now {words} words ({a.variant})")
