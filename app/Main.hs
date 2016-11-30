import TextGen (
  TextGen
  ,runTextGen
  ,word
  ,choose
  ,remove
  ,list
  ,randrep
  ,rep
  ,perhaps
  ,smartjoin
  )

import Annales.Empire (
  TextGenCh
  ,Empire
  ,Person(..)
  ,Gender(..)
  ,Forebear(..)
  ,renderEmpire,
   incrementYear
  ,yearDesc
  ,yearAbbrev
  ,court
  ,emperor
  ,lineage
  ,consort
  ,heirs
  ,claimants
  ,year
  ,pAge
  ,initialiseEmpire
  ,vocabGet
  ,generate
  ,dumbjoin
  ,randn
  ,paragraph
  ,sentence
  )

import Annales.Emperor ( royalWedding, royalBirth, probBirth )
import Annales.Succession ( probSuccession, succession )
import Annales.Court ( newCourtier, goneCourtier )
import Annales.Tribes ( newTribe, goneTribe )
import Annales.Buildings ( buildBuilding, destroyBuilding )
import Annales.Deaths ( deathProbs )
import Annales.Omens ( omen )

import System.Environment (getArgs)
import Text.Read (readMaybe)
import System.Random
import Data.Maybe (catMaybes)
import Data.List (intercalate)
import Control.Monad (forM)
import Text.Numeral.Roman (toRoman)

-- Note: the callbacks here provide a nice way to model contingent
-- events.
-- chance of an heir is 0  if there is no consort (but bastardy)
-- no marriages if there is already a consort
-- etc

-- Remember that I'm fudging empire state here: see
-- https://github.com/spikelynch/annales/issues/14



probmap :: [ ( (Empire -> Int), (Empire -> IO (Empire, TextGenCh )) )  ]
probmap = [
  ( probSuccession, succession )
  ,( probBirth, royalBirth )
  ,( probWedding, royalWedding )
  ,( (\_ -> 10), goneTribe )
  ,( (\_ -> 10), newTribe )
  ,( (\_ -> 20), goneCourtier )
  ,( (\_ -> 40), newCourtier )
  ,( (\_ ->  5), destroyBuilding )
  ,( (\_ ->  5), buildBuilding )
  ,( (\_ -> 10), omen )
  ]
  where probWedding e = case consort e of
                          (Just _) -> 0
                          Nothing -> case emperor e of
                                       (Just em) -> if pAge em > 15 then 65 else 0
                                       Nothing -> 0
        probWar e = if null $ claimants e then 0 else 75


mcons :: Empire -> IO [ Char ]
mcons e = case consort e of
  (Just (Person g _ _)) -> do
    n <- generate g
    return $ dumbjoin n
  Nothing -> return "no consort"

-- generate a year's worth of incidents and string them together as
-- a list



makeYear :: Empire -> IO ( Empire, Maybe TextGenCh )
makeYear e = do
  allprobmap <- return $ (deathProbs e) ++ probmap
  ( e', minc ) <- chain (incrementYear e) allprobmap
  case minc of
    Nothing -> return ( e', Nothing )
    Just inc -> case year e' of
      1 -> return ( e', Just $ list [ yearDesc e', inc ] )
      otherwise -> return ( e', Just $ list [ yearAbbrev e', inc ] )

      

chain :: Empire -> [ ( Empire -> Int, Empire -> IO ( Empire, TextGenCh ) )  ] -> IO ( Empire, Maybe TextGenCh )
chain e []     = return ( e, Nothing )
chain e (p:ps) = do
  ( e', mg ) <- perhapsIncident e p
  ( e'', mgs ) <- chain e' ps
  return ( e'', link mg mgs )

link :: Maybe TextGenCh -> Maybe TextGenCh -> Maybe TextGenCh
link Nothing Nothing = Nothing
link (Just g) Nothing = Just g
link Nothing (Just g) = Just g
link (Just g) (Just h) = Just (list [ g, h ]) 

perhapsIncident :: Empire -> ( Empire -> Int, Empire -> IO ( Empire, TextGenCh ) ) -> IO ( Empire, Maybe TextGenCh )
perhapsIncident e (probf, incident) = do
  r <- randn 100
  case r < (probf e) of
    True -> do
      (e', g) <- incident e
      return $ ( e', Just g )
    False -> return ( e, Nothing )



-- generateAnnales until we exceed len words

generateAnnals :: Int -> Empire -> IO (Empire, [ Char ])
generateAnnals len e = do
  ( e', mincidents ) <- makeYear e
  case mincidents of
    Nothing -> generateAnnals len e'
    Just incidents -> do
      words <- generate incidents
      text <- return $ concat words
      state <- renderEmpire e'
      lp <- return $ wordCount $ text
      case lp > len of
        True -> return (e', text)
        otherwise -> do
          (e'', rest ) <- generateAnnals (len - lp) e'
          return $ ( e'', text ++ "\n\n" ++ rest )
          --return $ text ++ "\n\n--\n" ++ state ++ "\n--\n\n" ++ rest

wordCount :: [ Char ] -> Int
wordCount t = 1 + (length $ filter (== ' ') t)




getLength :: [ String ] -> Int
getLength [] = 50000
getLength (a:as) = case readMaybe a of
  Nothing -> 50000
  Just i -> i



  
generateTitle :: Empire -> IO [ Char ]
generateTitle e = do
  l <- return $ map pname $ reverse $ lineage e
  mends <- return $ ends l
  subtitle <- return $ case mends of
    Just ( k1, k2 ) -> mkTitle k1 k2
    Nothing         -> "Not enough rulers to make a title"
  nation <- generate $ vocabGet e "places"
  return $ "# Annales " ++ (dumbjoin nation) ++ "\n\n" ++ subtitle ++ "\n\n"

mkTitle :: [ Char ] -> [ Char ] -> [ Char ]
mkTitle k1 k2 = "From " ++ k1 ++ " to " ++ k2
  
ends :: [ a ] -> Maybe ( a, a )
ends (a:b:bs) = Just ( a, last (b:bs) )
ends _        = Nothing

pname (Forebear n _ Nothing) = n
pname (Forebear n _ (Just i)) = n ++ " " ++ (toRoman i)


main :: IO ()
main = do
  args <- getArgs
  length <- return $ getLength args
  e0 <- initialiseEmpire "./data/"
  (e', annales) <- generateAnnals length e0
  title <- generateTitle e'
  putStrLn title
  putStrLn annales
