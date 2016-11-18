module Annales.Court (
  newCourtier
  ,goneCourtier
  ,deadEmperor
  ) where

import Text.Numeral.Roman (toRoman)

import Annales.Empire (
  TextGenCh
  ,Empire
  ,Person (..)
  ,Forebear(..)
  ,emperor
  ,lineage
  ,court
  ,vocabGet
  ,pGen
  ,pAge
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
  


deadEmperor :: Empire -> IO ( Empire, TextGenCh )
deadEmperor e = do
  ( newe, forebear ) <- newEmperor e
  e' <- return $ e { emperor = newe, lineage = forebear:(lineage e) }
  death <- return $ deathOf e $ pGen $ emperor e
  return ( e', list [ death, word "\n", word "Succession of", pGen newe ] ) 


-- newEmperor returns the new emperor's Textgen, name and regnal number

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
      return ( Person (choose [ rgen newe, word longname ]) 1, newe )
    otherwise -> return ( Person (rgen newe) 1, newe )



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
  nn <- excludeGet unsucc (vocabGet e "people")
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





arrived = chooseW [ "was born", "rose to prominence", "won favour", "was first heard of", "rose through the ranks", "was promoted" ]



newCourtier :: Empire -> IO ( Empire, TextGenCh )
newCourtier e = do
  new  <- generate $ vocabGet e "people"
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



-- things courtiers can do: write poems and plays and histories,
-- intrigue, win triumphs, be exiled to PLACE, retire to their
-- villa/etc in PLACE, sponsor games


