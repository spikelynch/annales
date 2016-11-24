module Annales.Emperor (
  royalWedding
  ,royalBirth
  ,probBirth
  ,newPerson
  ) where


import Annales.Empire (
  TextGenCh
  ,Empire
  ,Gender (..)
  ,Person (..)
  ,emperor
  ,lineage
  ,consort
  ,heirs
  ,court
  ,year
  ,vocabGet
  ,personGet
  ,pName
  ,pAge
  ,pGender
  ,generate
  ,dumbjoin
  ,wordjoin
  ,cap
  ,inc
  ,randn
  ,chooseW
  ,phrase
  )

import Annales.Names ( newName, newPerson, royalBabyName )

import TextGen (
  TextGen
  ,word
  ,choose
  ,remove
  ,perhaps
  ,list
  )

import Annales.Deaths ( deathOf )
import Annales.Omens ( omen )



royalWedding :: Empire -> IO ( Empire, TextGenCh )
royalWedding e = do
  consortGender <- return $ case emperor e of
    (Just emp) -> if (pGender emp) == Male then Female else Male
    Nothing -> Female
  c <- newName e consortGender 
  cg <- return $ word c
  age <- randn 5
  e' <- return $ e { consort = Just (Person cg (16 + age) consortGender) }
  return ( e', desc e' cg )
    where desc e cg = let me = emperor e
                          eg = case me of
                            Just emp -> pName emp
                            Nothing -> word "ERROR"
                          w = word
                          v = vocabGet e
                          waswed = w "was wedded to"
                          celebrated = list [ w "with", v "festivities" ]
                      in inc [ eg, waswed, cg, celebrated ]


royalBirth :: Empire -> IO ( Empire, TextGenCh )
royalBirth e = do
  case consort e of
    Nothing -> do
      putStrLn "ERROR royal birth without consort"
      return ( e, word "ERROR royal birth without consort" )
    Just cons -> do
      baby <- birth e
      mmother <- return $ femaleParent e
      case mmother of
        (Just mother) -> do
          e' <- return $ e { heirs = (heirs e) ++ [ baby ] }
          return ( e', birthDesc e' mother baby )
        Nothing ->
          return ( e, word "ERROR couldn't resolve parent" )

femaleParent :: Empire -> Maybe Person
femaleParent e = case consort e of
  Nothing -> Nothing
  (Just c) -> case pGender c of
                Female -> Just c
                Male -> emperor e


birthDesc e mother baby = let (Person pg _ g) = baby
                              (Person mg _ _) = mother
                              w = word
                              v = vocabGet e
                              child = if g == Male then w "son" else w "daughter"
                              star = perhaps ( 3, 5 ) $ list [ w "under the star", v "stars" ]
                          in inc [ mg, w "was brought to bed of a", child, phrase pg, star ]
                     
maybeConsort :: Empire -> Person
maybeConsort e = case consort e of
  Just c -> c
  Nothing -> Person (word "FIXME") 1 Female


probBirth :: Empire -> Int
probBirth e = case consortAge e of
  Nothing    -> 0
  (Just x) | x > 49 -> 0
  (Just x) | x < 16 -> 0
  otherwise -> if isBaby e then 0 else 35


consortAge :: Empire -> Maybe Int
consortAge e = case consort e of
  Nothing             -> Nothing
  Just (Person _ a _) -> Just a


isBaby :: Empire -> Bool
isBaby e = case heirs e of
  [] -> False
  hs -> case pAge $ last hs of
    x | x < 3 -> True
    otherwise -> False


birth :: Empire -> IO Person
birth e = do
  r <- randn 2
  g <- return $ if r == 0 then Female else Male
  n <- royalBabyName e g
  return $ Person (word n) 1 g





