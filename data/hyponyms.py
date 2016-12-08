#!/usr/bin/env python3

from nltk.corpus import wordnet as wn
import sys, argparse, inflect


def explode_hyponyms(ss):
    hs = ss.hyponyms()
    l = []
    if hs:
        for h in hs:
            l += explode_hyponyms(h)
        return l
    else:
        return [ ss ]


def wordlist(synsets, plurals=False):
    names = []
    pl = inflect.engine()

    for s in synsets:
        name = s.lemmas()[0].name()
        if plurals:
            name = pl.plural(name)
        name = name.replace('_', ' ')
        names.append(name)
    names = list(set(names))
    return names

parser = argparse.ArgumentParser()
parser.add_argument("-s", "--synset", type=str, default="animal", help="Synset to dump hyponyms of")
parser.add_argument("-l", "--list", action='store_true', help="List synsets")
parser.add_argument("-p", "--plurals", action='store_true', help="Generate plurals", default=False)
parser.add_argument("-t", "--title", action='store_true', help="Title Case")


args = parser.parse_args()

synsets = []

if '.' in args.synset:
    synset = wn.synset(args.synset)
    if not synset:
        print("Couldn't find synset with ID {}".format(args.synset));
        sys.exit(-1)
    synsets = [ synset ]
else:
    synsets = wn.synsets(args.synset)
    if not synsets:
        print("Couldn't find synsets matching {}".format(args.synset));
        sys.exit(-1)

if not synsets:
    print("No synsets found for {}".format(args.synset))
    sys.exit(-1)
        
if args.list:
    print(synsets)
    sys.exit(0)
    
sets = []
    
for ss in synsets:
    sets += explode_hyponyms(ss)
    
words = wordlist(sets, args.plurals)



for word in words:
    if args.title:
        word = word.title()
    print(word)
