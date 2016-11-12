module Annales.Courtiers (
  newCourtier
  ,deadCourtier
  ) where

import Annales.Empire (
  TextGenCh
  ,Empire
  ,court
  ,vocabGet
  ,generate
  ,dumbjoin
  ,randn
  )

import TextGen (
  TextGen
  ,word
  ,choose
  ,remove
  ,list
  )


import Annales.Deaths ( deathOf )
import Annales.Omens ( omen )
  

arrived = choose $ map word [ "was born", "rose to prominence", "won favour" ]



newCourtier :: Empire -> IO ( Empire, TextGenCh )
newCourtier e = do
  new  <- generate $ vocabGet e "people.txt"
  newc <- return $ word $ dumbjoin new
  e'   <- return $ e { court = newc:(court e) }
  return ( e', list [ newc, arrived ] )

deadCourtier :: Empire -> IO ( Empire, TextGenCh )
deadCourtier e = do
  ( mdc, court' ) <- generate $ remove $ court e
  case mdc of
    Nothing -> omen e
    Just courtier -> do
      e' <- return $ e { court = court' }
      return ( e', deathOf $ word $ dumbjoin courtier ) 

