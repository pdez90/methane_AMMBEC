"""docx_ops.py -- citation- and comment-safe edit operations for the manuscript .docx files
(factored out of script 89 for the later edit scripts).

  do_replace(d, old, new, count, label)   replace text inside direct runs; an anchor may not
                                          cross a hyperlinked citation or tracked insertion
  do_rewrite(d, anchor, text, label)      rewrite a whole paragraph; {{h0}} {{x0}} place its
                                          hyperlinks / insertions, {{c5}}..{{/c5}} its comments,
                                          **bold** a bold run; everything new is red
  do_table(d, ti, rows, header, label, widths)   rebuild a table in place (comments kept)
  swap_figures(path, mapping, resize, label)     replace pictures; resize frames if asked
"""
import sys, re, copy, zipfile, shutil, struct
from pathlib import Path
from docx.oxml.ns import qn
from docx.text.run import Run
from docx.shared import Pt
import docx_edit_lib as L
RED = L.RED

W_ = lambda tag: qn("w:" + tag)
def vis_text(p):          # everything the reader sees in the paragraph, hyperlinks and insertions included
    return "".join(t.text or "" for t in p._p.iter(W_("t")))
def sentinel(p):          # direct-run text; a hyperlinked citation or tracked insertion is a hard boundary
    out = []
    for ch in p._p:
        if ch.tag == W_("r"): out.append("".join(t.text or "" for t in ch.iter(W_("t"))))
        elif ch.tag in (W_("hyperlink"), W_("ins")): out.append("\x00")
    return "".join(out)
def find_para(d, anchor, label):
    hits = [p for p in L.all_paragraphs(d) if anchor in vis_text(p)]
    if len(hits) != 1: sys.exit(f"{label}: paragraph anchor found {len(hits)}x, expected 1: {anchor[:90]!r}")
    return hits[0]

def do_replace(d, old, new, count, label):
    paras = L.all_paragraphs(d)
    n = sum(sentinel(p).count(old) for p in paras)
    if n != count: sys.exit(f"{label}: anchor found {n}x in run text (citation-safe), expected {count}x: {old[:90]!r}")
    for p in paras:
        k = sentinel(p).count(old)
        if not k: continue
        if L.runs_text(p).count(old) != k: sys.exit(f"{label}: anchor spans a citation: {old[:80]!r}")
        pos = 0
        for _ in range(k): pos = L.replace_in_paragraph(p, old, new, pos)

TOK = re.compile(r"(\{\{/?c\d+\}\}|\{\{[hx]\d+\}\}|\*\*.+?\*\*)")
def do_rewrite(d, anchor, text, label, drop_h=()):
    p = find_para(d, anchor, label); P = p._p
    if P.find(".//" + qn("m:oMath")) is not None: sys.exit(f"{label}: paragraph holds an equation; use R: {anchor[:60]!r}")
    kids = list(P); pPr = P.find(W_("pPr"))
    H = [k for k in kids if k.tag == W_("hyperlink")]; X = [k for k in kids if k.tag == W_("ins")]
    CS = {k.get(W_("id")): k for k in kids if k.tag == W_("commentRangeStart")}
    CE = {k.get(W_("id")): k for k in kids if k.tag == W_("commentRangeEnd")}
    CR = {}
    for k in kids:
        if k.tag == W_("r") and k.find(W_("commentReference")) is not None: CR[k.find(W_("commentReference")).get(W_("id"))] = k
    BM = [k for k in kids if k.tag in (W_("bookmarkStart"), W_("bookmarkEnd"))]
    textruns = [k for k in kids if k.tag == W_("r") and k.find(W_("commentReference")) is None and "".join(t.text or "" for t in k.iter(W_("t"))).strip()]
    tmpl = max(textruns, key=lambda k: len("".join(t.text or "" for t in k.iter(W_("t"))))) if textruns else None
    for k in kids:
        if k is not pPr: P.remove(k)
    for b in BM: P.append(b)
    def textrun(s, bold=False):
        r = copy.deepcopy(tmpl) if tmpl is not None else P.makeelement(W_("r"), {})
        for ch in list(r):
            if ch.tag != W_("rPr"): r.remove(ch)
        P.append(r); run = Run(r, p); run.text = s; run.font.color.rgb = RED
        rp = r.find(W_("rPr"))
        if rp is not None:
            for t in ("vertAlign", "i", "iCs", "u"):
                e = rp.find(W_(t))
                if e is not None: rp.remove(e)
        if bold: run.font.bold = True
    usedH, usedX, usedC = set(), set(), set()
    for tok in TOK.split(text):
        if not tok: continue
        m = re.fullmatch(r"\{\{(/?c|h|x)(\d+)\}\}", tok)
        if m:
            kind, i = m.group(1), m.group(2)
            if kind == "h": P.append(H[int(i)]); usedH.add(int(i))
            elif kind == "x": P.append(X[int(i)]); usedX.add(int(i))
            elif kind == "c":                   # a marker nested in a hyperlink or insertion travels with it
                if i in CS: P.append(CS[i]); usedC.add(i)
            else:
                if i in CE: P.append(CE[i])
                if i in CR: P.append(CR[i])
        elif tok.startswith("**"): textrun(tok[2:-2], bold=True)
        else: textrun(tok)
    if usedH != set(range(len(H))) - set(drop_h) or usedX != set(range(len(X))):
        sys.exit(f"{label}: rewrite of {anchor[:50]!r} places hyperlinks {sorted(usedH)} of {len(H)}, insertions {sorted(usedX)} of {len(X)}")
    for cid in CS:                                   # comments not placed explicitly span the whole paragraph
        if cid not in usedC:
            (pPr.addnext(CS[cid]) if pPr is not None else P.insert(0, CS[cid]))
            if cid in CE: P.append(CE[cid])
            if cid in CR: P.append(CR[cid])
    for cid in list(CE) + list(CR):
        for dct in (CE, CR):
            if cid in dct and dct[cid].getparent() is None: P.append(dct[cid])

def set_cell(cell, text):
    paras = cell.paragraphs; p = paras[0]
    for q in paras[1:]: q._p.getparent().remove(q._p)
    old = vis_text(p)
    if old.strip() == text.strip(): return 0
    runs = [r for r in p.runs if r._r.find(W_("commentReference")) is None]
    if runs:
        for r in runs[1:]: r._r.getparent().remove(r._r)
        base = runs[0]
    else:
        base = p.add_run()
    base.text = text; base.font.color.rgb = RED
    return 1

def strip_comments(el):
    for t in ("commentRangeStart", "commentRangeEnd"):
        for e in list(el.iter(W_(t))): e.getparent().remove(e)
    for e in list(el.iter(W_("commentReference"))):
        r = e.getparent(); r.getparent().remove(r)

def resize_table(tbl, nrows, ncols, widths=None):
    t = tbl._tbl; grid = t.find(W_("tblGrid")); cols = grid.findall(W_("gridCol"))
    total = sum(int(c.get(W_("w"))) for c in cols)
    if widths: total = max(total, 9360)                # 6.5 in
    wl = [int(total * w / sum(widths)) for w in widths] if widths else [total // ncols] * ncols
    while len(cols) < ncols: c = copy.deepcopy(cols[-1]); cols[-1].addnext(c); cols.append(c)
    while len(cols) > ncols: grid.remove(cols.pop())
    for c, w_ in zip(cols, wl): c.set(W_("w"), str(w_))
    if widths:
        tp = t.find(W_("tblPr")); tw = tp.find(W_("tblW")) if tp is not None else None
        if tw is not None: tw.set(W_("w"), str(total)); tw.set(W_("type"), "dxa")
    trs = t.findall(W_("tr"))
    while len(trs) < nrows: n = copy.deepcopy(trs[-1]); strip_comments(n); trs[-1].addnext(n); trs.append(n)
    while len(trs) > nrows: t.remove(trs.pop())
    for tr in trs:
        tcs = tr.findall(W_("tc"))
        while len(tcs) < ncols: n = copy.deepcopy(tcs[-1]); strip_comments(n); tcs[-1].addnext(n); tcs.append(n)
        while len(tcs) > ncols: tr.remove(tcs.pop())
        for tc, w_ in zip(tcs, wl):
            pr = tc.find(W_("tcPr"))
            w = pr.find(W_("tcW")) if pr is not None else None
            if w is not None: w.set(W_("w"), str(w_)); w.set(W_("type"), "dxa")

def do_table(d, ti, rows, header, label, widths=None):
    tbl = d.tables[ti]; ncols = len(rows[0]); assert all(len(r) == ncols for r in rows), f"{label} table {ti}: ragged rows"
    resize_table(tbl, len(rows) + 1, ncols, widths)
    n = 0
    if header:
        for j, h in enumerate(header):
            if h is not None: n += set_cell(tbl.rows[0].cells[j], h)
    for i, row in enumerate(rows, start=1):
        for j, v in enumerate(row): n += set_cell(tbl.rows[i].cells[j], str(v))
    if widths:                                            # a wide table: 7 pt throughout
        for row in tbl.rows:
            for c in row.cells:
                for q in c.paragraphs:
                    for r in q.runs: r.font.size = Pt(7)
    return n

def renumber_sections(d, label):
    """the original text's 'section 3.2' (source apportionment) is section 3.3 now; red text is already new"""
    n = 0
    pat = re.compile(r"(sections? )3\.2(?!\d)")
    for p in L.all_paragraphs(d):
        for r in list(p.runs):
            try:
                if r.font.color.rgb == RED: continue
            except Exception: pass
            if not pat.search(r.text): continue
            pieces, last = [], 0
            for m in pat.finditer(r.text):
                pieces += [(r.text[last:m.end(1)], False), ("3.3", True)]; last = m.end()
            pieces.append((r.text[last:], False))
            prev = r._r
            for txt, red in pieces:
                if not txt: continue
                el = copy.deepcopy(r._r); prev.addnext(el); prev = el
                nr = Run(el, p); nr.text = txt
                if red: nr.font.color.rgb = RED
            r._r.getparent().remove(r._r); n += 1
    for p in L.all_paragraphs(d):
        if pat.search(L.runs_text(p).replace("3.3", "")) and re.search(r"sections? 3\.2(?!\d)", sentinel(p)):
            sys.exit(f"{label}: a 'section 3.2' reference spans runs: {L.runs_text(p)[:100]!r}")
    print(f"{label}: {n} run(s) renumbered section 3.2 -> 3.3")

def move_section_31(d):
    body = d.element.body
    paras = list(d.paragraphs)
    start = next(p for p in paras if vis_text(p).startswith("3.1. Constant-altitude flight transects"))
    stop = next(p for p in paras if vis_text(p).startswith("3.4. Emission estimate"))
    dest = next(p for p in paras if vis_text(p).strip() == "3. Statistical Analysis")
    block, el = [], start._p
    while el is not stop._p: block.append(el); el = el.getnext()
    anchor = dest._p
    for e in block: anchor.addnext(e); anchor = e
    print(f"main text: section 3.1 (transects) moved ahead of 3.2 ({len(block)} elements)")

def gra2pes_superscript(d):
    n = 0
    for p in L.all_paragraphs(d):
        for r in list(p.runs):
            if "GRA2PES" not in r.text: continue
            pieces = []
            for k, part in enumerate(r.text.split("GRA2PES")):
                if k: pieces += [("GRA", False), ("2", True), ("PES", False)]
                pieces.append((part, False))
            prev = r._r
            for txt, sup in pieces:
                if not txt: continue
                el = copy.deepcopy(r._r); prev.addnext(el); prev = el
                nr = Run(el, p); nr.text = txt
                if sup: nr.font.superscript = True; nr.font.color.rgb = RED
            r._r.getparent().remove(r._r); n += 1
    return n

# ---- figures: replace the embedded pictures; resize the frame where the aspect ratio changed ----
def png_size(b): return struct.unpack(">II", b[16:24]) if b[:8] == b"\x89PNG\r\n\x1a\n" else None
def swap_figures(path, mapping, resize, label):
    tmp = Path(str(path) + ".tmp"); newsz = {}
    with zipfile.ZipFile(path) as zin:
        rels = zin.read("word/_rels/document.xml.rels").decode()
        rid = {m.group(2).split("/")[-1]: m.group(1) for m in re.finditer(r'Id="(rId\d+)"[^>]*Target="([^"]+)"', rels)}
        rid.update({m.group(1).split("/")[-1]: m.group(2) for m in re.finditer(r'Target="([^"]+)"[^>]*Id="(rId\d+)"', rels)})
        docxml = zin.read("word/document.xml").decode()
        with zipfile.ZipFile(tmp, "w", zipfile.ZIP_DEFLATED) as zout:
            for item in zin.infolist():
                data = zin.read(item.filename); name = item.filename.split("/")[-1]
                if item.filename.startswith("word/media/") and name in mapping:
                    new = Path(mapping[name]).read_bytes(); so, sn = png_size(data), png_size(new)
                    if name not in resize and abs((so[0] / so[1]) / (sn[0] / sn[1]) - 1) > 0.05:
                        sys.exit(f"{label}: {name} {so} vs {Path(mapping[name]).name} {sn}: aspect ratio differs")
                    data = new; newsz[name] = sn; print(f"  {label}: {name} {so} <- {Path(mapping[name]).name} {sn}")
                if item.filename == "word/document.xml": continue
                zout.writestr(item, data)
            for name in resize:                       # keep the frame width, set the height from the new aspect ratio
                r_ = rid[name]; w, h = newsz[name]
                m = re.search(r'<w:drawing>(?:(?!</w:drawing>).)*?r:embed="%s"(?:(?!</w:drawing>).)*?</w:drawing>' % r_, docxml, re.S)
                blk = m.group(0); cx = int(6.5 * 914400); cy = int(round(cx * h / w))
                if cy > 8.4 * 914400:                     # keep inside the page: limit height to 8.4 in
                    cy = int(8.4 * 914400); cx = int(round(cy * w / h))
                nb = re.sub(r'(<wp:extent cx=")\d+(" cy=")\d+(")', rf'\g<1>{cx}\g<2>{cy}\g<3>', blk)
                nb = re.sub(r'(<a:ext cx=")\d+(" cy=")\d+(")', rf'\g<1>{cx}\g<2>{cy}\g<3>', nb)
                docxml = docxml.replace(blk, nb); print(f"  {label}: {name} frame set to {cx/914400:.2f} x {cy/914400:.2f} in")
            zout.writestr("word/document.xml", docxml)
    missing = set(mapping) - set(newsz)
    if missing: sys.exit(f"{label}: media not found: {missing}")
    shutil.move(str(tmp), str(path))


def delete_para(d, anchor, label, comments_to=None):
    """Remove a whole paragraph. Its comment markers move to the paragraph whose text contains
    `comments_to` (they then span that whole paragraph), so no comment is orphaned."""
    p = find_para(d, anchor, label); P = p._p
    ids = [e.get(W_("id")) for e in P.iter(W_("commentRangeStart"))]
    if ids:
        if comments_to is None: sys.exit(f"{label}: paragraph to delete carries comments {ids}; give comments_to")
        q = find_para(d, comments_to, label); Q = q._p; qpr = Q.find(W_("pPr"))
        for cid in ids:
            s_ = next(e for e in P.iter(W_("commentRangeStart")) if e.get(W_("id")) == cid)
            e_ = next((e for e in P.iter(W_("commentRangeEnd")) if e.get(W_("id")) == cid), None)
            r_ = next((e.getparent() for e in P.iter(W_("commentReference")) if e.get(W_("id")) == cid), None)
            (qpr.addnext(s_) if qpr is not None else Q.insert(0, s_))
            if e_ is not None: Q.append(e_)
            if r_ is not None: Q.append(r_)
    prv, nxt = P.getprevious(), P.getnext()
    P.getparent().remove(P)
    blank = lambda e: e is not None and e.tag == W_("p") and not "".join(t.text or "" for t in e.iter(W_("t"))).strip() and e.find(".//" + W_("drawing")) is None
    if blank(prv) and blank(nxt): nxt.getparent().remove(nxt)      # no double blank line where the paragraph was
