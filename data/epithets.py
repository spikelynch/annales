#!/usr/bin/env python3.5

from nltk.corpus import wordnet as wn
import random, sys, re
from nltk.corpus import framenet as fn

def random_word(synsets):
    synset = random.choice(synsets)
    lemma = random.choice(synset.lemmas())
    name = lemma.name()
    return name.replace('_', ' ')

def is_transitive(lemma):
    ln = lemma.name()
    for fs in lemma.frame_strings():
        frame = fs.split(' ')
        if frame[-1] != ln and frame[-1] != 'PP':
            return True
    return False

def random_transitive_verb(verbs):
    lemma = None
    while not lemma:
        v = random.choice(verbs)
        lemmas = [ l for l in v.lemmas() if is_transitive(l) ]
        if lemmas:
            lemma = random.choice(lemmas) 
    name = lemma.name()
    return name.replace('_', ' ')



adjectives = list(wn.all_synsets(pos=wn.ADJ))

for a in wn.all_synsets(pos=wn.ADJ): 
    for l in a.lemmas():
        w = l.name()
        w = w.replace('_', ' ')
        print(w)
            
            

