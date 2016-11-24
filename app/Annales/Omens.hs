module Annales.Omens (omen) where

import TextGen (
  TextGen
  ,word
  ,choose
  ,list
  ,randrep
  ,rep
  ,perhaps
  ,smartjoin
  )

import Annales.Empire (
  TextGenCh
  ,Empire
  ,vocabGet
  ,inc
  )
       





omen :: Empire -> IO ( Empire, TextGenCh )
omen e = return ( e, inc [ collective, phenom, word "in", place ] )
  where phenom = vocabGet e "phenomena"
        place = vocabGet e "places"


collective :: TextGenCh
collective = choose $ map word [ "Outbreak of", "Panic caused by", "Great", "Reports of", "Rumours of" ]
