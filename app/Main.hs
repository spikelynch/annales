import TextGen (TextGen, runTextGen, word,  choose, remove, list, randrep, rep, perhaps, smartjoin)

import Annales.Empire ( TextGenCh, Empire, incrementYear, yearDesc, court, emperor, pAge, initialiseEmpire, vocabGet, generate, dumbjoin, randn, paragraph, sentence)

import Annales.Court ( newCourtier, goneCourtier, deadEmperor )
import Annales.Tribes ( newTribe, goneTribe )
import Annales.Omens ( omen )

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
      return ( incrementYear e', Just $ list [ paragraph $ sentence $ yearDesc e', incidents ] )

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
    




main :: IO ()
main = do
  empire <- initialiseEmpire "./data/"
  annales <- generateAnnals 50000 empire
  putStrLn annales
