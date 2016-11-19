#!/usr/bin/env python3

import sys, re
from nltk.corpus import wordnet as wn

# do a dictionary lookup on people and if they're in WordNet, exclude them

DIVISION = [
    ( re.compile("^\w{1,7}(sh|is|ne|ch|th|us)$"), "gods" ),
    ( re.compile("(ism|ity)$"), "religions" ),
    ( re.compile("(us|a|on|ian)$"), "people" ),
    ( re.compile("(es|i|ae)$"), "tribes" ),
    ( re.compile(".*"), "places" )
]



divisions = {
    "gods": [],
    "religions": [],
    "people": [],
    "tribes": [],
    "places": []
    }


def is_real_word(w):
    synset = w.lower().replace(' ', '_')
    synsets = wn.synsets(synset)
    if synsets:
        return True
    return False



words = re.compile("^([A-Z ]+):")

for line in sys.stdin:
    m = words.search(line)
    if m:
        n = m.group(1).title()
        if not is_real_word(n):
            for r, d in DIVISION:
                m = r.search(n)
                if m:
                    divisions[d].append(n)
                    break

for d in divisions.keys():
    fn = d + ".txt"
    with open(fn, "w") as f:
        for n in divisions[d]:
            f.write(n + "\n")
    print("Wrote {} names to {}".format(len(divisions[d]), fn))
                
