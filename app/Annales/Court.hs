module Annales.Court (
  newCourtier
  ,goneCourtier
  ) where

import Text.Numeral.Roman (toRoman)

import Annales.Empire (
  TextGenCh
  ,Empire
  ,court
  ,vocabGet
  ,personGet
  ,generate
  ,dumbjoin
  ,cap
  ,randn
  ,chooseW
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
  

-- things courtiers can do: write poems and plays and histories,
-- intrigue, win triumphs, be exiled to PLACE, retire to their
-- villa/etc in PLACE, sponsor games, projects



arrived = chooseW [ "rose to prominence", "won favour", "was first heard of", "rose through the ranks", "was promoted" ]



newCourtier :: Empire -> IO ( Empire, TextGenCh )
newCourtier e = do
  new  <- generate $ personGet e
  newc <- return $ word $ dumbjoin new
  e'   <- return $ e { court = newc:(court e) }
  return ( e', list [ newc, arrived ] )

goneCourtier :: Empire -> IO ( Empire, TextGenCh )
goneCourtier e = do
  ( mdead, court' ) <- generate $ remove $ court e
  case mdead of
    Nothing                  -> omen e
    Just dead -> do
      e' <- return $ e { court = court' }
      return ( e', deathOf e $ word $ dumbjoin dead ) 





