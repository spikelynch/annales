#!/usr/bin/env python

from nltk.corpus import wordnet as wn

for s in wn.all_synsets('n'):
    n = s.name().split('.')[0]
    n = n.replace('_', ' ').upper()
    print("{}: {}".format(n, s.definition()))
