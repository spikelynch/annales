module Annales.Buildings (
  buildBuilding
  ,destroyBuilding
  ) where

import TextGen (
  TextGen
  ,word
  ,choose
  ,remove
  ,list
  )

import Annales.Empire (
  TextGenCh
  ,Empire
  ,buildings
  ,generate
  ,randRemove
  )

import Annales.Descriptions (
  descNewBuilding
  ,descBuildingName
  ,descModifyBuilding
  ,descBuildingGone
  )

import Annales.Omens (
  omen
  )

import Data.List (isInfixOf)
  
buildBuilding :: Empire -> IO ( Empire, TextGenCh )
buildBuilding e = do
  b <- return $ descBuildingName e
  dup <- existsBuilding e b
  case dup of
    False -> do
      e' <- return $ e { buildings = b:(buildings e) }
      return ( e', descNewBuilding e b )
    True -> do
      return ( e, descModifyBuilding e b )

existsBuilding :: Empire -> TextGenCh -> IO Bool
existsBuilding e b = do
  bname <- generate b
  bnames <- generate $ list $ buildings e
  return ( bname `isInfixOf` bnames )


destroyBuilding :: Empire -> IO ( Empire, TextGenCh )
destroyBuilding e = do
  ( bgone, buildings' ) <- randRemove $ buildings e
  case bgone of
    []       -> omen e
    (gone:b) -> do
      e' <- return $ e { buildings = buildings' }
      return ( e', descBuildingGone e gone ) 

