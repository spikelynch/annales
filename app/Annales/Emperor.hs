module Annales.Emperor (
  newEmperor
  ,deadEmperor
  ,royalWedding
  ,royalBirth
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
  ,vocabGet
  ,personGet
  ,pGen
  ,pAge
  ,generate
  ,dumbjoin
  ,wordjoin
  ,cap
  ,randn
  ,chooseW
  ,phrase
  )

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
  c <- generate $ vocabGet e "women" 
  cg <- return $ wordjoin c
  age <- randn 5
  e' <- return $ e { consort = Just (Person cg (16 + age) Female) }
  return ( e', desc e' cg )
    where desc e cg = let (Person eg _ _) = emperor e
                          w = word
                          v = vocabGet e
                          waswed = w "was wedded to"
                          celebrated = list [ w "with", v "festivities" ]
                      in list [ eg, waswed, cg, celebrated ]


royalBirth :: Empire -> IO ( Empire, TextGenCh )
royalBirth e = do
  case consort e of
    Nothing -> omen e
    Just mother -> do
      baby <- birth e
      e' <- return $ e { heirs = (heirs e) ++ [ baby ] }
      return ( e', birthDesc e' mother baby )

birthDesc e mother baby = let (Person pg _ g) = baby
                              (Person mg _ _) = mother
                              w = word
                              v = vocabGet e
                              child = if g == Male then w "son" else w "daughter"
                              star = perhaps ( 3, 5 ) $ list [ w "under the star", v "stars" ]
                          in list [ mg, w "was brought to bed of a", child, phrase pg, star ]
                     
maybeConsort :: Empire -> Person
maybeConsort e = case consort e of
  Just c -> c
  Nothing -> Person (word "FIXME") 1 Female

birth :: Empire -> IO Person
birth e = do
  r <- randn 2
  case r of
    1 -> do
      n <- generate $ vocabGet e "men"
      return $ Person (wordjoin n) 1 Male
    otherwise -> do
      n <- generate $ vocabGet e "women"
      return $ Person (wordjoin n) 1 Female


deadEmperor :: Empire -> IO ( Empire, TextGenCh )
deadEmperor e = do
  ( newe, forebear ) <- newEmperor e
  e' <- return $ e { emperor = newe, lineage = forebear:(lineage e), consort = Nothing }
  death <- return $ deathOf e $ pGen $ emperor e
  return ( e', list [ death, word "\n", word "Succession of", pGen newe ] ) 


-- newEmperor returns the new emperor's Person And Forebear

newEmperor :: Empire -> IO ( Person, Forebear )
newEmperor e = do
  newe <- do
    r <- randn 4
    case r of
      0          -> forebear e
      otherwise  -> newName e
  r <- randn 4
  case r of
    3 -> do
      epithet <- generate $ vocabGet e "epithets"
      (Forebear name _) <- return newe
      longname <- return $ name ++ " the " ++ ( cap $ dumbjoin epithet)
      return ( (Person (choose [ rgen newe, word longname ]) 1 Male), newe )
    otherwise -> return ( (Person (rgen newe) 1 Male), newe )



rgen :: Forebear -> TextGenCh
rgen (Forebear n Nothing)  = word n
rgen (Forebear n (Just i)) = list [ word n, word $ toRoman i ]




forebear :: Empire -> IO Forebear
forebear e = do
  l <- return $ lineage e
  name <- lineageName e l
  return $ nextRegnal l name


-- this should sometimes return Forebears with a Nothing regnal number
-- who will never have successors
-- it also needs to filter the vocab names so that they don't duplicate
-- a Nothing name which is already in the lineage

newName :: Empire -> IO Forebear
newName e = do
  n <- getNewName e
  r <- randn 2
  case r of
    0 -> return $ nextRegnal (lineage e) n
    otherwise -> return $ Forebear n Nothing




getNewName :: Empire -> IO [ Char ]
getNewName e = do
  unsucc <- return $ map (\(Forebear x _) -> x) $ filter isunsucc $ lineage e
  nn <- excludeGet unsucc (vocabGet e "men")
  return nn

isunsucc :: Forebear -> Bool
isunsucc (Forebear c Nothing) = True
isunsucc _                    = False

-- This could loop forever if lineage > names

excludeGet :: [ [ Char ] ] -> TextGenCh -> IO [ Char ]
excludeGet ex g = do
  nn <- generate g
  n <- return $ dumbjoin nn
  if n `elem` ex then excludeGet ex g else return n




-- get rid of the !! and head in this
-- filter the forebears so that ones with regnal number = Nothing
-- don't have successors

lineageName :: Empire -> [ Forebear ] -> IO [ Char ]
lineageName e []   = do
  (Forebear name _) <- newName e
  return name
lineageName e fbs  = do
  succ <- return $ filter ( not . isunsucc ) fbs
  case null succ of
    True -> do
      (Forebear name _) <- newName e
      return name
    False -> do
      k <- randn 2
      case k of
        0 -> do
          i <- randn $ length succ
          (Forebear name _) <- return $ succ !! i
          return name
        otherwise -> do
          (Forebear name _) <- return $ head succ
          return name
      




nextRegnal :: [ Forebear ] -> [ Char ] -> Forebear
nextRegnal l n = let r = 1 + (length $ filter (\(Forebear m _) -> m == n) l)
                 in (Forebear n (Just r))




