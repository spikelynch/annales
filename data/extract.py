#!/usr/bin/env python3

import sys, re
from nltk.corpus import wordnet as wn



DIVISION = [
    ( re.compile(r"\sOf\s"), "artifacts", True),
    ( re.compile(r"[^ ]{3,}(ns|rs|es|ds|es)$"), "phenomena", False),
    ( re.compile(r"^[^ ]+(sion|ions|nces|sts|als)$"), "festivities", False),
    ( re.compile(r"^[^ ]+(ity|ness|ing|age|nce|dom|ry|ment|hood|ure)$"), "abstractions", False),
    ( re.compile(r"^[^ ]{5,}(ible|ant|ish|ary|ic|ical|like|ous)$"), "adjectives", False),
    ( re.compile(r"(men|ards|ors|ons|eeps|ins|sts|ers|ins|ings|lves|orcs|edes)$"), "allies", False),
    ( re.compile(r"^[^ ]{3,}ly$"), "adverbs", False),
    ( re.compile(r"^\w{1,7}(sh|is|ne|ch|th|us|om|gh|or|rg|rh|b)$"), "gods", True),
    ( re.compile(r"^[^ ]{1,19}(et|ave|ade|eon|axe|ife|bow|shot)$"), "weapons", False),
    ( re.compile(r"(sour|sweet|bitter|cheese|fruit|apple|berry|meat|pie|bean|leaf|chip|flesh|shell|cake|bread)"), "foods", False),
    ( re.compile(r"(mead|honey|ale|beer|wine|juice|water|milk|syrup|cream)"), "drinks", False),
    ( re.compile(r"^([^ ]+ )?[^ ]{5,}(sy|ps|gue|ver|th)$"), "diseases", False ),
    ( re.compile(r"(oint|crem|otion|musk|fume|ohol|nth)"), "cosmetics", False),
    ( re.compile(r"(ism|ity)$"), "religions", True ),
    ( re.compile(r"^\w{1,10}(zh|gh|were|ant|sph|gon|chi|ore|saur|oul|oup)"), "monsters", False ),
    ( re.compile(r"(eum|ium|ple|rch|ad|se|ce)$"), "buildings", True ),
    ( re.compile(r"(a|ne|ie|ey|il|ty)$"), "women", True),
    ( re.compile(r"(us|on|ses|ix|an)$"), "men", True ),
    ( re.compile(r"(es|i|ae)$"), "tribes", True ),
    ( re.compile(r".*"), "places", True )
]





def is_real_word(w):
    synset = w.lower().replace(' ', '_')
    synsets = wn.synsets(synset)
    if synsets:
        return True
    return False

TERMS_RE = re.compile("^([A-Z- ]+):")


def by_terms(line):
    m = TERMS_RE.search(line)
    if m:
        return [ m.group(1) ]
    else:
        return []

WORDS_RE = re.compile("\w{4,}")

def by_splitting(line):
    return WORDS_RE.findall(line)

divisions = {}

for r, d, c in DIVISION:
    divisions[d] = []




for line in sys.stdin:
    for term in by_terms(line):
        if not is_real_word(term):
            for r, d, caps in DIVISION:
                if caps:
                    t2 = term.title()
                else:
                    t2 = term.lower()
                m = r.search(t2)
                if m:
                    t2 = re.sub(r"\bOf\b", "of", t2)
                    t2 = re.sub(r"\bThe\b", "the", t2)
                    divisions[d].append(t2)
                    break


for d in divisions.keys():
    fn = d + ".txt"
    with open(fn, "w") as f:
        for n in divisions[d]:
            f.write(n + "\n")
    print("Wrote {} names to {}".format(len(divisions[d]), fn))
                
