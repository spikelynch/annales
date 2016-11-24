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
  ,renderEmpire,
   incrementYear
  ,yearDesc
  ,yearAbbrev
  ,court
  ,emperor
  ,lineage
  ,consort
  ,heirs
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
import Annales.Succession ( succession )
import Annales.Court ( newCourtier, goneCourtier )
import Annales.Tribes ( newTribe, goneTribe )
import Annales.Deaths ( deathProbs )
import Annales.Omens ( omen )

import System.Environment (getArgs)
import Text.Read (readMaybe)
import System.Random
import Data.Maybe (catMaybes)

-- Note: the callbacks here provide a nice way to model contingent
-- events.
-- chance of an heir is 0  if there is no consort (but bastardy)
-- no marriages if there is already a consort
-- etc

-- Remember that I'm fudging empire state here: see
-- https://github.com/spikelynch/annales/issues/14



probmap :: [ ( (Empire -> Int), (Empire -> IO (Empire, TextGenCh )) )  ]
probmap = [
  ( probBirth, royalBirth )
  ,( probWedding, royalWedding )
  ,( probSuccession, succession )
  ,( (\_ -> 10), newTribe )
  ,( (\_ -> 10), goneTribe )
  ,( (\_ -> 40), newCourtier )
  ,( (\_ -> 10), omen )
  ]
  where probWedding e = case consort e of
                          (Just _) -> 0
                          Nothing -> case emperor e of
                                       (Just em) -> if pAge em > 15 then 65 else 0
                                       Nothing -> 0
        probSuccession e = case emperor e of
                             (Just _) -> 0
                             Nothing -> 100


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
    Just inc -> case year e of
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
link (Just g) Nothing = Just $ paragraph $ sentence g
link Nothing (Just g) = Just g
link (Just g) (Just h) = Just (list [ paragraph $ sentence g, h ]) 

perhapsIncident :: Empire -> ( Empire -> Int, Empire -> IO ( Empire, TextGenCh ) ) -> IO ( Empire, Maybe TextGenCh )
perhapsIncident e (probf, incident) = do
  r <- randn 100
  case r < (probf e) of
    True -> do
      (e', g) <- incident e
      return $ ( e', Just g )
    False -> return ( e, Nothing )



-- generateAnnales until we exceed len words

generateAnnals :: Int -> Empire -> IO [ Char ]
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
        True -> return text
        otherwise -> do
          rest <- generateAnnals (len - lp) e'
          return $ text ++ "\n\n" ++ rest
--          return $ text ++ "\n\n--\n" ++ state ++ "\n--\n\n" ++ rest

wordCount :: [ Char ] -> Int
wordCount t = 1 + (length $ filter (== ' ') t)




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
  annales <- generateAnnals length e0
  putStrLn annales
