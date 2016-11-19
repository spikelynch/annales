import TextGen (TextGen, runTextGen, word,  choose, remove, list, randrep, rep, perhaps, smartjoin)

import Annales.Empire ( TextGenCh, Empire, incrementYear, yearDesc, yearAbbrev, court, emperor, lineage, pAge, initialiseEmpire, vocabGet, generate, dumbjoin, randn, paragraph, sentence)

import Annales.Court ( newCourtier, goneCourtier, newEmperor, deadEmperor )
import Annales.Tribes ( newTribe, goneTribe )
import Annales.Omens ( omen )

import System.Environment (getArgs)
import Text.Read (readMaybe)
import System.Random
import Control.Monad (forM)
import Data.Maybe (catMaybes)

probmap = [
  ( (\e -> 5 + (pAge $ emperor e)), deadEmperor )
  ,( (\_ -> 10), newTribe )
  ,( (\_ -> 10), goneTribe )
  ,( (\_ -> 10), newCourtier )
  ,( (\_ -> 10), goneCourtier )
  ,( (\_ -> 40), omen )
  ]


-- generate a year's worth of incidents and string them together as
-- a list

year :: Empire -> IO ( Empire, Maybe TextGenCh )
year e = do
  mis <- forM probmap $ \(p, incident) -> do
    r <- randn 100
    case r < (p e) of
      True ->  return $ Just incident
      False -> return $ Nothing
  is <- return $ catMaybes mis
  case null is of
    True -> return ( incrementYear e, Nothing )
    False -> do
      ( e', incidents ) <- chain e (catMaybes mis)
      return ( incrementYear e', Just $ list [ yearAbbrev e', incidents ] )

-- feel like I'm reinventing a wheel here
-- This should be done with a stateT
  
chain :: Empire -> [ Empire -> IO ( Empire, TextGenCh ) ] -> IO ( Empire, TextGenCh )
chain e []     = return ( e, tgempty )
chain e (i:is) = do
  ( e', g ) <- i e
  ( e'', gs ) <- chain e' is
  return ( e'', list [ paragraph $ sentence g, gs ] )


tgempty :: (RandomGen g) => TextGen g [[Char]]
tgempty = return [ ]


-- generateAnnales until we exceed len

generateAnnals :: Int -> Empire -> IO [ Char ]
generateAnnals len e = do
  ( e', mincidents ) <- year e
  case mincidents of
    Nothing -> generateAnnals len e'
    Just incidents -> do
      words <- generate incidents
      text <- return $ concat words
      lp <- return $ length $ text
      case lp > len of
        True -> return text
        otherwise -> do
          rest <- generateAnnals (len - lp) e'
          return $ text ++ rest

getLength :: [ String ] -> Int
getLength [] = 1000
getLength (a:as) = case readMaybe a of
  Nothing -> 1000
  Just i -> i




main :: IO ()
main = do
  args <- getArgs
  length <- return $ getLength args
  e0 <- initialiseEmpire "./data/"
  ( emp0, forebear ) <- newEmperor e0
  empire <- return $ e0 { emperor = emp0, lineage = [ forebear ] }
  annales <- generateAnnals length empire
  putStrLn annales
