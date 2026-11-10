"""docx_tc.py -- edit a .docx that still carries other people's tracked changes and comments.

The earlier helpers (docx_edit_lib, docx_ops) treat a tracked insertion as a hard boundary.
A draft that has come back from co-authors has insertions and deletions in most paragraphs,
so the functions here work on the text a reader sees with the changes accepted (deleted text
is ignored) and leave every w:ins / w:del wrapper and every comment marker in place:

  tc_text(p)                      the paragraph's text with tracked changes accepted
  tc_find(d, anchor, label)       the one paragraph whose accepted text contains the anchor
  tc_replace(d, old, new, label)  replace `old` (unique in the document) by `new`, in red;
                                  `old` may run across tracked insertions; a run inside an
                                  insertion is split with its wrapper, so the co-author's
                                  change stays a tracked change on either side of the new text
  tc_insert_at(d, before, new, label)       put `new` in front of the text `before`, removing nothing
  tc_insert_after(d, anchor, text, label)   new red paragraph(s) after the anchor's paragraph
"""
import sys, copy, itertools
from docx.oxml.ns import qn
from docx.text.run import Run
from docx.text.paragraph import Paragraph
import docx_edit_lib as L
RED = L.RED
W_ = lambda t: qn("w:" + t)
_ids = itertools.count(900000)

def _atoms(p):
    """(w:t element, its run) for every text node a reader sees, in order; deleted text has w:delText and is skipped"""
    out = []
    for t in p._p.iter(W_("t")):
        r = t.getparent()
        anc = [a.tag for a in t.iterancestors()]
        if W_("del") in anc or W_("moveFrom") in anc: continue
        out.append((t, r))
    return out
def tc_text(p): return "".join(t.text or "" for t, _ in _atoms(p))

def tc_find(d, anchor, label):
    hits = [p for p in L.all_paragraphs(d) if anchor in tc_text(p)]
    if len(hits) != 1: sys.exit(f"{label}: anchor found in {len(hits)} paragraphs, expected 1: {anchor[:90]!r}")
    return hits[0]

def _set(t, s):
    t.text = s
    if s != s.strip(): t.set("{http://www.w3.org/XML/1998/namespace}space", "preserve")

def _split_after(run, t, tail):
    """Text `tail` is to follow `run` as its own run with the same formatting; returns the element
    after which new content can be placed (the run itself, or its tracked-change wrapper)."""
    par = run.getparent(); wrapped = par.tag in (W_("ins"), W_("moveTo"))
    later = []
    if wrapped:
        sib = run.getnext()
        while sib is not None: later.append(sib); sib = sib.getnext()
    if tail:
        tr = copy.deepcopy(run)
        for ch in list(tr):
            if ch.tag not in (W_("rPr"), W_("t")): tr.remove(ch)
        ts = tr.findall(W_("t"))
        for x in ts[1:]: tr.remove(x)
        _set(ts[0], tail)
    else: tr = None
    if not wrapped:
        if tr is not None: run.addnext(tr)
        return run, tr
    if tr is not None or later:                       # split the wrapper: [.. run] new [tail, later ..]
        w2 = copy.copy(par)
        for ch in list(w2): w2.remove(ch)
        w2.set(W_("id"), str(next(_ids)))
        if tr is not None: w2.append(tr)
        for x in later: w2.append(x)
        par.addnext(w2)
        return par, w2
    return par, None

def tc_replace(d, old, new, label, count=1):
    paras = [p for p in L.all_paragraphs(d) if old in tc_text(p)]
    n = sum(tc_text(p).count(old) for p in paras)
    if n != count: sys.exit(f"{label}: anchor found {n}x, expected {count}x: {old[:90]!r}")
    for p in paras:
        for _ in range(tc_text(p).count(old)): _replace_once(p, old, new, label)

def tc_insert_at(d, before, new, label):
    """put `new` (red) immediately in front of the text `before` (unique in the document) and remove nothing,
    so a tracked insertion that ends or starts there is left whole"""
    paras = [p for p in L.all_paragraphs(d) if before in tc_text(p)]
    if len(paras) != 1 or tc_text(paras[0]).count(before) != 1: sys.exit(f"{label}: anchor not unique: {before[:90]!r}")
    _replace_once(paras[0], "", new, label, at=tc_text(paras[0]).find(before))

def _replace_once(p, old, new, label, at=None):
    atoms = _atoms(p); full = "".join(t.text or "" for t, _ in atoms); i = full.find(old) if at is None else at; j = i + len(old)
    pos = 0; first = None
    for t, r in atoms:
        s = t.text or ""; a, b = pos, pos + len(s); pos = b
        if j == i:
            if first is not None or not (a <= i < b): continue
        elif b <= i or a >= j: continue
        if any(x.tag == W_("hyperlink") for x in r.iterancestors()): sys.exit(f"{label}: anchor runs into a hyperlinked citation: {old[:80]!r}")
        if first is None:
            first = (t, r, s[: i - a], s[j - a:] if j < b else "")
        else:
            _set(t, s[j - a:] if j < b else "")
    t, r, head, tail = first
    _set(t, head)
    after, _ = _split_after(r, t, tail)
    if new:
        nr = copy.deepcopy(r)
        for ch in list(nr):
            if ch.tag != W_("rPr"): nr.remove(ch)
        rp = nr.find(W_("rPr"))
        if rp is not None:
            for tag in ("vertAlign", "rStyle", "highlight"):
                e = rp.find(W_(tag))
                if e is not None: rp.remove(e)
        after.addnext(nr); run = Run(nr, p); run.text = new; run.font.color.rgb = RED
    # drop runs left with no content, and wrappers left with no runs
    for t2, r2 in atoms:
        if r2.getparent() is None: continue
        if not "".join(x.text or "" for x in r2.iter(W_("t"))) and all(ch.tag in (W_("rPr"), W_("t")) for ch in r2):
            par = r2.getparent(); par.remove(r2)
            if par.tag in (W_("ins"), W_("moveTo")) and len(par) == 0: par.getparent().remove(par)

def tc_insert_after(d, anchor, text, label):
    p = tc_find(d, anchor, label)
    return L.insert_after(p, text, d)
