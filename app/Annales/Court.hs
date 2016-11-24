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

import Annales.Descriptions (
  descCourtier
  ,descCourtDouble
  ,descCourtierGo
  )

import TextGen (
  TextGen
  ,word
  ,choose
  ,remove
  ,list
  )

import Annales.Descriptions ( descDeathOf )
import Annales.Omens ( omen )
  




newCourtier :: Empire -> IO ( Empire, TextGenCh )
newCourtier e = do
  p <- newPerson e
  dup <- elemPerson p $ court e
  case dup of
    False -> do
      e' <- return $ e { court = p:(court e) }
      return ( e', descCourtier e p )
    True -> do
      return ( e, descCourtDouble e p )

goneCourtier :: Empire -> IO ( Empire, TextGenCh )
goneCourtier e = do
  ( mgone, court' ) <- randRemove $ court e
  case mgone of
    []     -> omen e
    gone:c -> do
      e' <- return $ e { court = court' }
      return ( e', descCourtierGo e gone ) 





