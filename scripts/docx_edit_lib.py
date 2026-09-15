"""docx_edit_lib.py -- small toolkit for scripted, auditable edits to the manuscript .docx files.

Used by scripts/61_manuscript_v2_edits.py (and later edit scripts). Every edit is an exact-anchor
operation on the document text; anchors are checked before anything is written, every
replacement is checked after, and inserted text is coloured red so the author can see what
changed. Citations are numbered superscripts written as [[^N]] markers.

Operations (registered per document with doc = "main" | "si"):
  R(a, b, doc, count=1)  replace text a with b (a must occur exactly `count` times)
  W(a, b, doc)           rewrite the whole paragraph containing a (keeps trailing hyperlinks)
  I(a, text, doc)        insert paragraphs after the paragraph containing a; blocks separated by
                         blank lines; special blocks:
                           [[FIG:name.png]]            picture from --figures (6 in wide, centred)
                           [[H]] title / [[H2]] title  Heading 1 / Heading 2 paragraph
                           [[TABLE:file.csv|col=Label,...|fmt]]  table from a CSV (red, 8 pt)
  P(prefix, new, doc)    replace a whole paragraph whose full text starts with prefix
                         (reference-list entries, which are hyperlink-wrapped)
  T(doc, ti, ri, ci, new, expect=None)  replace a table cell
  FIGSWAP(doc, old_size (w,h) or media name, new png)  replace an embedded picture
"""
import sys, re, copy, csv
from pathlib import Path
import docx
from docx.shared import RGBColor, Pt, Inches
from docx.oxml.ns import qn
from docx.text.run import Run
from docx.text.paragraph import Paragraph

RED = RGBColor(0xC0, 0x00, 0x00)
E = []; TC = []; RP = []; FS = []
FIGDIR = None; OUTDIR = None

def R(a, b, doc="main", count=1): E.append((a, b, doc, "replace", count))
def W(a, b, doc="main"): E.append((a, b, doc, "rewrite", 1))
def I(a, b, doc="main"): E.append((a, b, doc, "insert_after", 1))
def P(doc, startswith, new): RP.append((doc, startswith, new))
def T(doc, table, row, col, new, expect=None): TC.append((doc, table, row, col, new, expect))
def FIGSWAP(doc, key, png): FS.append((doc, key, png))

def runs_text(p): return "".join(r.text for r in p.runs)
def all_paragraphs(d):
    ps = list(d.paragraphs)
    for tb in d.tables:
        for row in tb.rows:
            for cell in row.cells: ps.extend(cell.paragraphs)
    return ps

def replace_in_paragraph(p, old, new, start=0):
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
    nr = Run(newrun, p); nr.text = new; nr.font.color.rgb = RED
    if tail:
        tailrun = copy.deepcopy(runs[start_ri]._r); newrun.addnext(tailrun)
        tr = Run(tailrun, p); tr.text = tail; tr.font.color.rgb = None
    return i + len(new)

def rewrite_paragraph(p, new):
    runs = p.runs
    for r in runs[1:]: r._r.getparent().remove(r._r)
    runs[0].text = new; runs[0].font.color.rgb = RED

def _new_para_after(p, after=None, style_from=None):
    src = style_from if style_from is not None else p
    newp = copy.deepcopy(src._p)
    for child in list(newp):
        if child.tag != qn("w:pPr"): newp.remove(child)
    (after if after is not None else p)._p.addnext(newp)
    return Paragraph(newp, p._parent)

def _heading_para(doc, level):
    want = "Heading %d" % level
    for q in doc.paragraphs:
        if q.style is not None and str(q.style.name) == want and q.text.strip(): return q
    return None

def _table_from_csv(after, doc, spec):
    """[[TABLE:file.csv|col=Label,col=Label,...|fmt]] where fmt is e.g. 'Q_t_hr=%.2f;dist_km=%.1f'."""
    parts = spec.split("|"); f = Path(parts[0]); cols = [c.split("=") for c in parts[1].split(",")]
    fmts = dict(kv.split("=") for kv in parts[2].split(";")) if len(parts) > 2 and parts[2] else {}
    flt = dict(kv.split("=") for kv in parts[3].split(";")) if len(parts) > 3 and parts[3] else {}   # keep rows with col == value
    path = f if f.is_absolute() else Path(OUTDIR) / f
    rows = [r for r in csv.DictReader(open(path)) if all(r.get(k) == v for k, v in flt.items())]
    tbl = doc.add_table(rows=1 + len(rows), cols=len(cols))
    styled = False
    for st_ in ("Table Grid", "TableGrid"):
        try: tbl.style = doc.styles[st_]; styled = True; break
        except Exception: pass
    if not styled:
        try: tbl.style = doc.tables[0].style
        except Exception: pass
    # explicit thin borders so the table reads as a table whatever the document's table styles are
    from docx.oxml import OxmlElement
    tblPr = tbl._tbl.tblPr; borders = OxmlElement("w:tblBorders")
    for edge in ("top", "left", "bottom", "right", "insideH", "insideV"):
        el = OxmlElement("w:" + edge); el.set(qn("w:val"), "single"); el.set(qn("w:sz"), "4"); el.set(qn("w:space"), "0"); el.set(qn("w:color"), "808080")
        borders.append(el)
    tblPr.append(borders)
    for j, (c, lab) in enumerate(cols): tbl.rows[0].cells[j].text = lab
    for i, r in enumerate(rows, start=1):
        for j, (c, lab) in enumerate(cols):
            v = r.get(c, "")
            if c in fmts and v not in ("", "NA"):
                try: v = fmts[c] % float(v)
                except Exception: pass
            tbl.rows[i].cells[j].text = "" if v == "NA" else v
    for row in tbl.rows:
        for cell in row.cells:
            for para in cell.paragraphs:
                for run in para.runs: run.font.color.rgb = RED; run.font.size = Pt(7)
    after._p.addnext(tbl._tbl)
    return tbl

def insert_after(p, text, doc):
    last = p
    for block in [b.strip() for b in text.split("\n\n") if b.strip()]:
        if block.startswith("[[FIG:") and block.endswith("]]"):
            name = block[6:-2]
            np_ = _new_para_after(p, last); np_.alignment = 1
            src = Path(FIGDIR) / name if FIGDIR else None
            if src is None or not src.exists(): sys.exit(f"figure {name} not found under --figures ({FIGDIR})")
            np_.add_run().add_picture(str(src), width=Inches(6.0)); last = np_
        elif block.startswith("[[TABLE:") and block.endswith("]]"):
            tbl = _table_from_csv(last, doc, block[8:-2])
            marker = copy.deepcopy(p._p)
            for child in list(marker):
                if child.tag != qn("w:pPr"): marker.remove(child)
            tbl._tbl.addnext(marker); last = Paragraph(marker, p._parent)
        elif block.startswith("[[H]]") or block.startswith("[[H2]]"):
            level = 2 if block.startswith("[[H2]]") else 1
            hp = _heading_para(doc, level) or p
            np_ = _new_para_after(p, last, style_from=hp)
            r = np_.add_run(block.split("]]", 1)[1].strip()); r.font.color.rgb = RED; last = np_
        else:
            np_ = _new_para_after(p, last); np_.paragraph_format.space_after = Pt(6)
            r = np_.add_run(block); r.font.color.rgb = RED; last = np_
    return last

# ---- post-passes on red runs ---------------------------------------------------------------
CHEM = re.compile(r"(CH4|C2H6|CO2|NO2|H2O)")
def subscript_chem(paras):
    n = 0
    for p in paras:
        for r in list(p.runs):
            try:
                if r.font.color.rgb != RED or not CHEM.search(r.text): continue
            except Exception: continue
            pieces = []
            for tok in CHEM.split(r.text):
                if not tok: continue
                if CHEM.fullmatch(tok):
                    for ch in tok: pieces.append((ch, ch.isdigit()))
                else: pieces.append((tok, False))
            prev = r._r
            for txt, sub_ in pieces:
                el = copy.deepcopy(r._r); prev.addnext(el); prev = el
                nr = Run(el, p); nr.text = txt
                if sub_: nr.font.subscript = True
            r._r.getparent().remove(r._r); n += 1
    return n
EXP = re.compile(r"(?:(?<=\byr)|(?<=\bmol)|(?<=\bkm)|(?<=\bh)|(?<=\bs)|(?<=\bm))(-1|−1|-2|−2)(?![\d])|(?<=\bkm)(2)(?![\d,.])")
def exponent_fix(paras):
    n = 0
    for p in paras:
        for r in list(p.runs):
            try:
                if r.font.color.rgb != RED or r.font.superscript or not EXP.search(r.text): continue
            except Exception: continue
            pieces, last = [], 0
            for m in EXP.finditer(r.text):
                pieces.append((r.text[last:m.start()], False)); pieces.append((m.group(0).replace("-", "−"), True)); last = m.end()
            pieces.append((r.text[last:], False))
            prev = r._r
            for txt, sup in pieces:
                if not txt: continue
                el = copy.deepcopy(r._r); prev.addnext(el); prev = el
                nr = Run(el, p); nr.text = txt
                if sup: nr.font.superscript = True
            r._r.getparent().remove(r._r); n += 1
    return n
CITE = re.compile(r"\[\[\^([0-9,–-]+)\]\]")
def cite_fix(paras):
    n = 0
    for p in paras:
        for r in list(p.runs):
            try:
                if r.font.color.rgb != RED or not CITE.search(r.text): continue
            except Exception: continue
            prev = r._r
            for tok in CITE.split(r.text):
                if not tok: continue
                el = copy.deepcopy(r._r); prev.addnext(el); prev = el
                nr = Run(el, p)
                if re.fullmatch(r"[0-9,–-]+", tok): nr.text = tok; nr.font.superscript = True; n += 1
                else: nr.text = tok
            r._r.getparent().remove(r._r)
    return n

# ---- figure swap by size ---------------------------------------------------------------------
def swap_pictures(docx_path, swaps, label):
    """swaps: list of (key, png_path); key = media file name or (w, h) of the existing PNG."""
    import zipfile, shutil, struct
    def png_size(b):
        if b[:8] != b"\x89PNG\r\n\x1a\n": return None
        return struct.unpack(">II", b[16:24])
    tmp = Path(str(docx_path) + ".tmp"); n = 0
    with zipfile.ZipFile(docx_path) as zin, zipfile.ZipFile(tmp, "w", zipfile.ZIP_DEFLATED) as zout:
        for item in zin.infolist():
            data = zin.read(item.filename); name = item.filename.split("/")[-1]
            if item.filename.startswith("word/media/"):
                sz = png_size(data)
                for key, png in swaps:
                    hit = (name == key) if isinstance(key, str) else (sz is not None and abs(sz[0] - key[0]) <= 2 and abs(sz[1] - key[1]) <= 2)
                    if hit:
                        new = Path(png).read_bytes(); nsz = png_size(new)
                        if sz and nsz and abs((sz[0] / sz[1]) / (nsz[0] / nsz[1]) - 1) > 0.05:
                            sys.exit(f"{label}: {name} {sz} vs {Path(png).name} {nsz}: aspect ratio differs")
                        data = new; n += 1; print(f"  {label}: {name} {sz} <- {Path(png).name} {nsz}")
            zout.writestr(item, data)
    shutil.move(str(tmp), str(docx_path)); print(f"{label}: {n} picture(s) replaced")

# ---- driver ---------------------------------------------------------------------------------
def apply(doc_path, out_path, which, label):
    edits = [e for e in E if e[2] == which]; cells = [t for t in TC if t[0] == which]
    d = docx.Document(str(doc_path)); paras = all_paragraphs(d)
    whole = "\n".join(runs_text(p) for p in paras)
    bad = [(a, c) for a, _, _, _, c in edits if whole.count(a) != c]
    if bad:
        sys.exit(f"{label}: these anchors do not occur the expected number of times; nothing written:\n  " +
                 "\n  ".join(f"found {whole.count(a)}x, expected {c}x  {a[:80]!r}" for a, c in bad))
    n = 0
    for _, ti, ri, ci, new, expect in cells:
        cell = d.tables[ti].rows[ri].cells[ci]; cp = cell.paragraphs[0]; old = runs_text(cp)
        if expect is not None and old.strip() != expect:
            sys.exit(f"{label}: table {ti} row {ri} col {ci} holds {old!r}, expected {expect!r}; nothing written")
        if old.strip() != new.strip(): rewrite_paragraph(cp, new); n += 1
    for a, b, _, kind, c in edits:
        if a == b: continue
        hits = [p for p in paras if a in runs_text(p)]
        for p in hits:
            if kind == "replace":
                pos = 0
                while True:
                    pos = replace_in_paragraph(p, a, b, pos)
                    if pos < 0: break
                    n += 1
            elif kind == "rewrite": rewrite_paragraph(p, b); n += 1
            elif kind == "insert_after": insert_after(p, b, d); n += 1
    for doc_, pre, new in [r for r in RP if r[0] == which]:
        hits = [p for p in d.paragraphs if p.text.startswith(pre)]
        if len(hits) != 1: sys.exit(f"{label}: paragraph starting {pre!r} found {len(hits)}x, expected 1; nothing written")
        p = hits[0]
        for child in list(p._p):
            if child.tag != qn("w:pPr"): p._p.remove(child)
        r = p.add_run(new); r.font.color.rgb = RED; n += 1
    after = "\n".join(runs_text(p) for p in all_paragraphs(d))
    def _probe(b, kind):
        if kind != "insert_after": return b
        blocks = [x.strip() for x in b.split("\n\n") if x.strip() and not x.strip().startswith("[[")]
        return blocks[0] if blocks else ""
    missing = [b for a, b, _, kind, _ in edits if b and a != b and _probe(b, kind) and _probe(b, kind) not in after]
    if missing:
        sys.exit(f"{label}: {len(missing)} replacement(s) did not land:\n  " + "\n  ".join(repr(b[:90]) for b in missing))
    m = subscript_chem(all_paragraphs(d)); e = exponent_fix(all_paragraphs(d)); c = cite_fix(all_paragraphs(d))
    d.save(str(out_path))
    print(f"{label}: {n} edit(s) written to {out_path} (red; {m} chemical subscripts, {e} unit exponents, {c} citation numbers)")
    swaps = [(k, png) for doc_, k, png in FS if doc_ == which]
    if swaps: swap_pictures(out_path, swaps, label)
