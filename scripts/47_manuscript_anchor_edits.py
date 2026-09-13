#!/usr/bin/env python3
"""47_manuscript_anchor_edits.py -- retarget the manuscript's inventory-anchor numbers
to the values the pipeline emitted on 12 Sep 2026, and mark every change in red.

    python3 scripts/47_manuscript_anchor_edits.py IN.docx OUT.docx [--values paper_values.json]

WHY THIS EXISTS. config.R's anchors are now re-derived by the pipeline (scripts 16/17/18/36,
checked by scripts/45). The .docx is edited by hand, so it drifts. This applies the specific
numeric substitutions below, refuses to run if the document does not contain exactly the
occurrences expected, and sets each replacement in red so the edits can be eyeballed in Word.

IT IS DELIBERATELY NOT GENERAL. It does not reflow sentences or touch anything but these
literals. Anything that needs a rewritten claim (not just a restated number) is left alone
and reported, because a script that silently rewrites argument text cannot be reviewed.

Replacements, with provenance (paper_values.json keys in brackets):
  23,478 -> 23,622  Gg CO2/yr, Vulcan box total            [E_CO2]
  23.5 million -> 23.6 million  the same number in words   [E_CO2]
  2,755 -> 2,856    km2, Vulcan box footprint (1-km cells) [vulcan_cells]
  291.5 -> 292.7    Gg CO/yr, EPA 2020 NEI seven-county    [E_CO_NEI]
  25.7 -> 25.8      t CH4/hr, NEI-anchored upper bound     [nei_hi]
Unchanged and verified consistent: 121.5 [E_CO], 11.1 [nei_lo], 4.1 [vulcan],
4.6/10.7/7.6 [gra_*], "about 2.4 times" (292.7/121.5 = 2.41).
"""
import sys, json, re
from pathlib import Path

try:
    import docx
    from docx.shared import RGBColor
except ImportError:
    sys.exit("python-docx is required:  pip install python-docx")

EDITS = [
    ("23,478", "23,622", "Vulcan box CO2 total, Gg/yr"),
    ("23.5 million", "23.6 million", "the same Vulcan total, in words"),
    ("2,755", "2,856", "Vulcan box footprint, km2 (1-km cells)"),
    ("291.5", "292.7", "EPA 2020 NEI seven-county CO, Gg/yr"),
    ("25.7", "25.8", "NEI-anchored upper bound, t CH4/hr"),
]
# How many times each literal must appear. A mismatch means the document is not the
# draft these edits were written against -- stop rather than half-apply them.
EXPECTED = {"23,478": 2, "23.5 million": 1, "2,755": 1, "291.5": 3, "25.7": 2}


def para_text(p):
    return "".join(r.text for r in p.runs)


def replace_in_paragraph(p, old, new, mark_red=True):
    """Replace every occurrence of `old` across run boundaries, keeping formatting.

    Runs carry the character formatting, so the replacement is written into the run
    where the match starts (inheriting its formatting) and the remainder of the match
    is deleted from the following runs. Matches inside one run are the common case and
    keep formatting exactly.
    """
    n = 0
    while True:
        runs = p.runs
        full = "".join(r.text for r in runs)
        i = full.find(old)
        if i < 0:
            return n
        # map character offsets to (run index, offset within run)
        spans, pos = [], 0
        for ri, r in enumerate(runs):
            spans.append((pos, pos + len(r.text), ri))
            pos += len(r.text)
        start_ri = next(ri for s, e, ri in spans if s <= i < e)
        end_ri = next(ri for s, e, ri in spans if s < i + len(old) <= e)
        s0 = spans[start_ri][0]
        head = runs[start_ri].text[: i - s0]
        if start_ri == end_ri:
            tail = runs[start_ri].text[i - s0 + len(old):]
            runs[start_ri].text = head + new + tail
        else:
            runs[start_ri].text = head + new
            consumed = len(runs[start_ri].text) - len(head) - len(new)  # 0
            remaining = len(old) - (spans[start_ri][1] - i)
            for ri in range(start_ri + 1, end_ri + 1):
                t = runs[ri].text
                take = min(len(t), remaining)
                runs[ri].text = t[take:]
                remaining -= take
                if remaining <= 0:
                    break
        if mark_red:
            # Red font is the convention for a changed number in a circulated draft, and
            # unlike a highlight it survives printing and PDF export.
            try:
                runs[start_ri].font.color.rgb = RGBColor(0xC0, 0x00, 0x00)
            except Exception:
                pass
        n += 1


def main():
    if len(sys.argv) < 3:
        sys.exit(__doc__)
    src, dst = Path(sys.argv[1]), Path(sys.argv[2])
    vals = None
    if "--values" in sys.argv:
        vals = json.loads(Path(sys.argv[sys.argv.index("--values") + 1]).read_text())

    d = docx.Document(str(src))
    paras = list(d.paragraphs)
    for tb in d.tables:
        for row in tb.rows:
            for cell in row.cells:
                paras.extend(cell.paragraphs)

    whole = "\n".join(para_text(p) for p in paras)
    problems = []
    for old, count in EXPECTED.items():
        got = whole.count(old)
        if got != count:
            problems.append(f"  {old!r}: expected {count} occurrence(s), found {got}")
    if problems:
        sys.exit("Document does not match the draft these edits were written for:\n"
                 + "\n".join(problems)
                 + "\n\nNothing was changed. Re-check the numbers by hand.")

    # Guard the values against the pipeline's own output when it is available.
    if vals:
        checks = [("23,622", f"{vals['E_CO2']:,.0f}".replace(",", ","), "E_CO2"),
                  ("2,856", f"{vals['vulcan_cells']:,}", "vulcan_cells"),
                  ("292.7", f"{vals['E_CO_NEI']}", "E_CO_NEI"),
                  ("25.8", f"{vals['nei_hi']}", "nei_hi")]
        bad = [f"  {k}: script writes {w}, paper_values.json says {g}"
               for w, g, k in checks if w.replace(",", "") != g.replace(",", "")]
        if bad:
            sys.exit("Replacement values disagree with paper_values.json:\n" + "\n".join(bad))
        print("checked replacement values against paper_values.json: OK")

    total = 0
    for old, new, why in EDITS:
        n = sum(replace_in_paragraph(p, old, new) for p in paras)
        total += n
        print(f"  {old:>12} -> {new:<12} x{n}   ({why})")

    after = "\n".join(para_text(p) for p in paras)
    leftover = [o for o, _, _ in EDITS if o in after and o not in ("25.7",)]
    if leftover:
        print("WARNING: still present after editing:", leftover)

    d.save(str(dst))
    print(f"\n{total} replacement(s) written to {dst}")
    print("Every changed number is set in red in the output.")


if __name__ == "__main__":
    main()
