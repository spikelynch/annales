module Annales.Tribes ( tribe ) where

import TextGen (TextGen, runTextGen, word, choose, remove, list, randrep, rep, perhaps, smartjoin)

import Annales.Empire ( TextGenCh, Empire, vocabGet, generate)
  
import System.Random

tribe :: Empire -> IO ( Empire, TextGenCh )
tribe e = return ( e, tribeGen ( vocabGet e "tribes.txt" ) )


tribeGen :: TextGenCh -> TextGenCh
tribeGen t = list [ word "The", t, werebad ]
  where werebad = choose $ map word [ "caused trouble", "were restless", "made incursions", "were repelled" ]
