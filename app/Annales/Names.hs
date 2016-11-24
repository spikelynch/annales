module Annales.Names (
  newName
  ,newPerson
  ,royalBabyName
  ) where

import Text.Numeral.Roman (toRoman)

import Annales.Empire (
  TextGenCh
  ,Empire
  ,Gender (..)
  ,Person (..)
  ,Forebear(..)
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
  ,randn
  ,chooseW
  ,phrase
  ,elemPerson
  )

import TextGen (
  TextGen
  ,word
  ,choose
  ,remove
  ,perhaps
  ,list
  )


-- A Note on Regnal Names

-- at birth - pick a random name, biased towards forebears, but never
-- one which is unsucceedable

-- at succession - take the heirs name, and generate the Forebear with
-- regnal number: (with an option to make it unsucceedable)



-- Returns a name of the correct gender, with a bias towards
-- forebears, avoiding unsucceedable forebears

royalBabyName :: Empire -> Gender -> IO [ Char ]
royalBabyName e g = do
  r <- randn 4
  case r of
    0         -> do
      ln <- lineageName e g
      already <- elemPerson (Person (word ln) 1 Male) (heirs e)
      case already of
        True -> newName e g
        False -> return ln
    otherwise -> newName e g





-- Returns a random new person

newPerson :: Empire -> IO Person
newPerson e = do
  rg <- randn 2
  gender <- return $ case rg of
    0 -> Male
    1 -> Female
  age <- randn 20
  name <- newName e gender
  return $ Person (word name) (age + 15) gender




-- Get a random name of the right gender which is not in the
-- lineage as 'unsucc'

newName :: Empire -> Gender -> IO [ Char ]
newName e g = do
  vocab <- return $ if g == Male then "men" else "women"
  unsucc <- return $ map (\(Forebear x _ _) -> x) $ filter isunsucc $ lineage e
  nn <- excludeGet unsucc (vocabGet e vocab)
  return nn


-- This could loop forever if lineage > names

excludeGet :: [ [ Char ] ] -> TextGenCh -> IO [ Char ]
excludeGet ex g = do
  nn <- generate g
  n <- return $ dumbjoin nn
  if n `elem` ex then excludeGet ex g else return n




-- TODO: get rid of the !! and head in this
-- now respects the Gender

lineageName :: Empire -> Gender -> IO [ Char ]
lineageName e g = do
  case filter (\(Forebear _ gg _) -> g == gg ) $ lineage e of
    []  -> newName e g
    fbs -> do
      succ <- return $ filter ( not . isunsucc ) fbs
      case null succ of
        True -> newName e g
        False -> do
          k <- randn 2
          case k of
            0 -> do
              i <- randn $ length succ
              (Forebear name _ _) <- return $ succ !! i
              return name
            otherwise -> do
              (Forebear name _ _) <- return $ head succ
              return name

isunsucc :: Forebear -> Bool
isunsucc (Forebear c _ Nothing) = True
isunsucc _                      = False

