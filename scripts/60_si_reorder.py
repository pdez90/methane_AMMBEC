#!/usr/bin/env python3
"""60_si_reorder.py -- put the SI sections in the order the main text first cites them,
and renumber sections, figures and tables consistently in BOTH documents.

  python3 scripts/60_si_reorder.py <main.docx> <si.docx> <main_out.docx> <si_out.docx> [--red]

What it does
  1. Reads the main text and records the order in which "section S<n>" citations first
     appear (tables and captions included). Sections never cited keep their relative
     order and go after the cited ones.
  2. Moves each SI section block (its Heading-1 paragraph through the element before the
     next Heading-1, tables and figures included) into that order. Everything before the
     first section heading and from the References heading on is left where it is.
  3. Renumbers the sections (S<n>., S<n>.<m> sub-headings, "section S<n>" cross-references
     in both documents), then renumbers figures and tables by their physical order in the
     reordered SI ("Figure S<n>:" / "Table S<n>:" captions) and rewrites every
     "Figure S..", "Figures S.. and S..", "Table S.." reference in both documents.
     A lettered figure such as "Figure S3b" becomes an ordinary number.
  4. Writes a renumbering key (old -> new) next to the outputs (si_renumbering_key.csv).
Cross-references are rewritten in place with their original formatting (pass --red to
colour the changed tokens red instead).
"""
import sys, re, copy, csv
from pathlib import Path
import docx
from docx.shared import RGBColor
from docx.oxml.ns import qn
from docx.text.run import Run

RED = RGBColor(0xC0, 0x00, 0x00)
COLOUR = "--red" in sys.argv
args = [a for a in sys.argv[1:] if not a.startswith("--")]
if len(args) != 4: sys.exit(__doc__)
main_in, si_in, main_out, si_out = map(Path, args)

def runs_text(p): return "".join(r.text for r in p.runs)
def all_paragraphs(d):
    ps = list(d.paragraphs)
    for tb in d.tables:
        for row in tb.rows:
            for cell in row.cells: ps.extend(cell.paragraphs)
    return ps

def replace_in_paragraph(p, old, new, start=0):
    """Replace the first occurrence of `old` at or after `start` (character offset into the
    run text). Keeps the formatting of the run where the match begins. Returns the offset
    just past the replacement, or -1 if not found."""
    runs = p.runs; full = runs_text(p); i = full.find(old, start)
    if i < 0: return -1
    spans, pos = [], 0
    for ri, r in enumerate(runs):
        spans.append((pos, pos + len(r.text), ri)); pos += len(r.text)
    start_ri = next(ri for s_, e, ri in spans if s_ <= i < e)
    end_ri = next(ri for s_, e, ri in spans if s_ < i + len(old) <= e)
    s0 = spans[start_ri][0]; head = runs[start_ri].text[: i - s0]
    if start_ri == end_ri:
        tail = runs[start_ri].text[i - s0 + len(old):]
    else:
        tail = ""; remaining = len(old) - (spans[start_ri][1] - i)
        for ri in range(start_ri + 1, end_ri + 1):
            t = runs[ri].text; take = min(len(t), remaining); runs[ri].text = t[take:]; remaining -= take
            if remaining <= 0: break
    runs[start_ri].text = head
    newrun = copy.deepcopy(runs[start_ri]._r); runs[start_ri]._r.addnext(newrun)
    nr = Run(newrun, p); nr.text = new
    if COLOUR: nr.font.color.rgb = RED
    if tail:
        tailrun = copy.deepcopy(runs[start_ri]._r); newrun.addnext(tailrun)
        tr = Run(tailrun, p); tr.text = tail
    return i + len(new)

main = docx.Document(str(main_in)); si = docx.Document(str(si_in))

# ---- 1. citation order in the main text -------------------------------------------------
SEC_RE = re.compile(r"[Ss]ections?\s+(S\d+(?:\s*(?:,|and|to|-|–)\s*S\d+)*)")
order = []
for p in all_paragraphs(main):
    for m in SEC_RE.finditer(p.text):
        for s in re.findall(r"S(\d+)", m.group(1)):
            n = int(s)
            if n not in order: order.append(n)
print("main-text first-citation order of SI sections:", ["S%d" % n for n in order])

# ---- 2. SI section blocks ---------------------------------------------------------------
body = si.element.body
kids = list(body)
def is_h1(el):
    if el.tag != qn("w:p"): return False
    ppr = el.find(qn("w:pPr"))
    if ppr is None: return False
    ps = ppr.find(qn("w:pStyle"))
    return ps is not None and ps.get(qn("w:val")) in ("Heading1", "Heading 1")
def ptext(el): return "".join(t.text or "" for t in el.iter(qn("w:t")))
heads = [(k, el) for k, el in enumerate(kids) if is_h1(el)]
sec_heads = [(k, el, int(re.match(r"\s*S(\d+)", ptext(el)).group(1))) for k, el in heads if re.match(r"\s*S\d+[.:\s]", ptext(el))]
if not sec_heads: sys.exit("no 'S<n>.' Heading 1 paragraphs found in the SI")
ref_k = next((k for k, el in heads if ptext(el).strip().lower().startswith("reference")), None)
end_k = ref_k if ref_k is not None else next(k for k, el in enumerate(kids) if el.tag == qn("w:sectPr"))
blocks = {}
for idx, (k, el, n) in enumerate(sec_heads):
    k_end = sec_heads[idx + 1][0] if idx + 1 < len(sec_heads) else end_k
    blocks[n] = kids[k:k_end]
old_nums = [n for _, _, n in sec_heads]
new_order = [n for n in order if n in blocks] + [n for n in old_nums if n not in order]
if new_order == old_nums: print("SI sections already in citation order; only renumbering checks will run.")
sec_map = {old: new for new, old in enumerate(new_order, start=1)}
print("section map:", ", ".join("S%d->S%d" % (o, sec_map[o]) for o in old_nums if sec_map[o] != o) or "identity")

# within each section, place every figure (its picture paragraph + caption) right after the
# paragraph that first cites it, so the physical order follows the discussion order
def has_drawing(el): return el.tag == qn("w:p") and el.find(".//" + qn("w:drawing")) is not None
def reflow_figures(block):
    caps = [(k, re.match(r"\s*Figure\s+S(\d+[a-z]?)\s*[:.]", ptext(el)).group(1))
            for k, el in enumerate(block) if el.tag == qn("w:p") and re.match(r"\s*Figure\s+S\d+[a-z]?\s*[:.]", ptext(el))]
    pairs = {}
    for k, key in caps:
        first = k - 1 if k > 0 and has_drawing(block[k - 1]) else k
        pairs[key] = block[first:k + 1]
    if len(pairs) < 2: return block
    taken = {id(el) for els in pairs.values() for el in els}
    rest = [el for el in block if id(el) not in taken]
    cite_at = {}
    for key in pairs:
        pat = re.compile(r"Figures?\s+(?:S\d+[a-z]?\s*(?:,|and)\s*)*S%s\b" % re.escape(key))
        cite_at[key] = next((k for k, el in enumerate(rest) if el.tag == qn("w:p") and pat.search(ptext(el))), None)
    if any(v is None for v in cite_at.values()): return block          # leave uncited layouts alone
    out = []
    for k, el in enumerate(rest):
        out.append(el)
        for key in sorted(pairs, key=lambda x: (cite_at[x], x)):
            if cite_at[key] == k: out.extend(pairs[key])
    return out
for n in blocks: blocks[n] = reflow_figures(blocks[n])

# rebuild the body: prefix, blocks in the new order, then the rest (References ...)
first_k = sec_heads[0][0]
prefix = kids[:first_k]; suffix = kids[end_k:]
for el in kids: body.remove(el)
for el in prefix: body.append(el)
for n in new_order:
    for el in blocks[n]: body.append(el)
for el in suffix: body.append(el)

# ---- 3. figure / table maps from the physical order of captions in the reordered SI -------
CAP_FIG = re.compile(r"^\s*Figure\s+S(\d+)([a-z]?)\s*[:.]")
CAP_TAB = re.compile(r"^\s*Table\s+S(\d+)\s*[:.]")
fig_map, tab_map = {}, {}
for p in si.paragraphs:
    m = CAP_FIG.match(p.text)
    if m:
        key = m.group(1) + m.group(2)
        if key not in fig_map: fig_map[key] = str(len(fig_map) + 1)
    m = CAP_TAB.match(p.text)
    if m and m.group(1) not in tab_map: tab_map[m.group(1)] = str(len(tab_map) + 1)
print("figure map:", ", ".join("S%s->S%s" % (o, n) for o, n in fig_map.items() if o != n) or "identity")
print("table map: ", ", ".join("S%s->S%s" % (o, n) for o, n in tab_map.items() if o != n) or "identity")
smap = {str(o): str(n) for o, n in sec_map.items()}

# ---- 4. rewrite references in both documents ----------------------------------------------
TOK = r"S\d+[a-z]?"
LIST = r"(?:%s(?:\s*(?:,|and|to|-|–)\s*%s)*)" % (TOK, TOK)
REF_RE = re.compile(r"(Figures?|Tables?|[Ss]ections?)(\s+)(%s)" % LIST)
HEAD_RE = re.compile(r"^(S)(\d+)((?:\.\d+)*)([.:\s])")          # S10. / S1.1 / S2.1:
CAPF_RE = re.compile(r"^(Figure\s+S)(\d+[a-z]?)(\s*[:.])")
CAPT_RE = re.compile(r"^(Table\s+S)(\d+)(\s*[:.])")
def map_tok(tok, kind):
    m = re.match(r"S(\d+)([a-z]?)", tok); key = m.group(1) + m.group(2)
    if kind.lower().startswith("fig"): return "S" + fig_map.get(key, key)
    if kind.lower().startswith("tab"): return "S" + tab_map.get(key, key)
    return "S" + smap.get(m.group(1), m.group(1)) + m.group(2)
def map_list(lst, kind):
    # split on the connectors, map each token, then re-join; a "S3-S4" range whose members
    # are no longer adjacent is written "S3 and S5"
    parts = re.split(r"(\s*(?:,|and|to|-|–)\s*)", lst)
    toks = [map_tok(t, kind) if re.fullmatch(TOK, t) else t for t in parts]
    out = "".join(toks)
    if not re.search(r"[-–]|\bto\b", lst):                       # a plain list: keep it ascending
        nums = re.findall(TOK, out); key = lambda t: (int(re.sub(r"[a-z]", "", t[1:])), t)
        if nums != sorted(nums, key=key):
            it = iter(sorted(nums, key=key)); out = re.sub(TOK, lambda m: next(it), out)
    if re.search(r"S\d+\s*[-–]\s*S\d+", out):
        a, b_ = [int(x) for x in re.findall(r"S(\d+)", out)[:2]]
        if b_ != a + 1: out = re.sub(r"(S\d+)\s*[-–]\s*(S\d+)", r"\1 and \2", out)
    return out
def rewrite(p):
    n = 0
    text = runs_text(p)
    edits = {}                                                   # start offset -> (old, new)
    m = HEAD_RE.match(text)
    if m and p.style is not None and str(p.style.name).startswith("Heading"):
        punct = m.group(4)                                        # normalise "S2:" -> "S2." and "S2.1:" -> "S2.1 "
        if m.group(3): punct = "" if punct in (":", ".") else punct
        elif punct == ":": punct = "."
        new_head = "S" + smap.get(m.group(2), m.group(2)) + m.group(3) + punct
        edits[0] = (m.group(0), new_head)
    m = CAPF_RE.match(text)
    if m: edits[0] = (m.group(0), m.group(1) + fig_map.get(m.group(2), m.group(2)) + m.group(3))
    m = CAPT_RE.match(text)
    if m: edits[0] = (m.group(0), m.group(1) + tab_map.get(m.group(2), m.group(2)) + m.group(3))
    for m in REF_RE.finditer(text):
        if m.start() in edits: continue                          # the caption / heading edit already covers it
        edits[m.start()] = (m.group(0), m.group(1) + m.group(2) + map_list(m.group(3), m.group(1)))
    pos = 0
    for _, (old, new) in sorted(edits.items()):
        if old == new: pos = runs_text(p).find(old, pos) + len(old); continue
        pos2 = replace_in_paragraph(p, old, new, pos)
        if pos2 < 0: raise SystemExit("could not place %r in: %s" % (old, text[:80]))
        pos = pos2; n += 1
    return n
n_main = sum(rewrite(p) for p in all_paragraphs(main))
n_si = sum(rewrite(p) for p in all_paragraphs(si))
print("rewrote %d reference(s) in the main text and %d in the SI" % (n_main, n_si))

# ---- 5. sanity: every cited section / figure / table exists in the new SI -----------------
secs_now = set(); figs_now = set(); tabs_now = set()
for p in si.paragraphs:
    m = HEAD_RE.match(p.text)
    if m and str(p.style.name).startswith("Heading 1"): secs_now.add("S" + m.group(2))
    m = CAP_FIG.match(p.text)
    if m: figs_now.add("S" + m.group(1) + m.group(2))
    m = CAP_TAB.match(p.text)
    if m: tabs_now.add("S" + m.group(1))
missing = set()
for p in all_paragraphs(main) + all_paragraphs(si):
    for m in REF_RE.finditer(p.text):
        kind = m.group(1).lower(); have = figs_now if kind.startswith("fig") else tabs_now if kind.startswith("tab") else secs_now
        for t in re.findall(TOK, m.group(3)):
            if t not in have: missing.add((m.group(1), t))
if missing: print("WARNING: references to items that do not exist in the SI:", sorted(missing))
print("SI sections now:", sorted(secs_now, key=lambda s: int(s[1:])))
print("SI figures now:", sorted(figs_now, key=lambda s: int(re.sub(r'[a-z]', '', s[1:]))), " tables:", sorted(tabs_now, key=lambda s: int(s[1:])))

main.save(str(main_out)); si.save(str(si_out))
with open(si_out.parent / "si_renumbering_key.csv", "w", newline="") as f:
    w = csv.writer(f); w.writerow(["kind", "old", "new"])
    for o, n in sec_map.items(): w.writerow(["section", "S%d" % o, "S%d" % n])
    for o, n in fig_map.items(): w.writerow(["figure", "S" + o, "S" + n])
    for o, n in tab_map.items(): w.writerow(["table", "S" + o, "S" + n])
print("wrote", main_out, si_out, "and si_renumbering_key.csv")
