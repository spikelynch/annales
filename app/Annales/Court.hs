module Annales.Court (
  newCourtier
  ,goneCourtier
  ) where

import Text.Numeral.Roman (toRoman)

import Annales.Empire (
  TextGenCh
  ,Empire
  ,Person(..)
  ,court
  ,vocabGet
  ,personGet
  ,pName
  ,generate
  ,dumbjoin
  ,cap
  ,inc
  ,randn
  ,randRemove
  ,elemPerson
  ,chooseW
  )

import Annales.Emperor (
  newPerson
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
  p <- newPerson e
  dup <- elemPerson p $ court e
  case dup of
    False -> do
      e' <- return $ e { court = p:(court e) }
      return ( e', inc [ pName p, arrived ] )
    True -> do
      return ( e, inc [ word "Fearful omen of a doppleganger of", pName p, word "at court" ] )

goneCourtier :: Empire -> IO ( Empire, TextGenCh )
goneCourtier e = do
  ( mdead, court' ) <- randRemove $ court e
  case mdead of
    []     -> omen e
    dead:d -> do
      e' <- return $ e { court = court' }
      return ( e', deathOf e $ pName dead ) 





