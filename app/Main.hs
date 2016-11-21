import TextGen (TextGen, runTextGen, word,  choose, remove, list, randrep, rep, perhaps, smartjoin)

import Annales.Empire ( TextGenCh, Empire, Person(..), incrementYear, yearDesc, yearAbbrev, court, emperor, lineage, consort, pAge, initialiseEmpire, vocabGet, generate, dumbjoin, randn, paragraph, sentence)

import Annales.Emperor ( newEmperor, deadEmperor, royalWedding, royalBirth )
import Annales.Court ( newCourtier, goneCourtier )
import Annales.Tribes ( newTribe, goneTribe )
import Annales.Omens ( omen )

import System.Environment (getArgs)
import Text.Read (readMaybe)
import System.Random
import Control.Monad (forM)
import Data.Maybe (catMaybes)

-- Note: the callbacks here provide a nice way to model contingent
-- events.
-- chance of an heir is 0  if there is no consort (but bastardy)
-- no marriages if there is already a consort
-- etc

-- Remember that I'm fudging empire state here: see
-- https://github.com/spikelynch/annales/issues/14

probmap = [
  ( probBirth, royalBirth )
  ,( probWedding, royalWedding )
  ,( (\e -> 5 + (pAge $ emperor e)), deadEmperor )
  ,( (\_ -> 20), newTribe )
  ,( (\_ -> 20), goneTribe )
  ,( (\_ -> 10), newCourtier )
  ,( (\_ -> 10), goneCourtier )
  ,( (\_ -> 20), omen )
  ]
  where probWedding e = case consort e of
                          (Just _) -> 0
                          Nothing -> 65
        probBirth e = case consort e of
                          (Just _) -> 30
                          Nothing -> 0

mcons :: Empire -> IO [ Char ]
mcons e = case consort e of
  (Just (Person g _ _)) -> do
    n <- generate g
    return $ dumbjoin n
  Nothing -> return "no consort"

-- generate a year's worth of incidents and string them together as
-- a list

-- FIXME - the loop of probabilities and the loop of running and
-- chaining incidents have to be the same, so the probability of an
-- incident is based on the correct state - the "missing mother bug"
-- where a royal birth got invalidated because the emperor died,
-- removing the consort.  But that should be allowed to happen.


year :: Empire -> IO ( Empire, Maybe TextGenCh )
year e = do
  c <- mcons e
  mis <- forM probmap $ \(p, incident) -> do
    r <- randn 100
    x <- return $ p e
    case r < x of
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
