import TextGen ( TextGen, runTextGen, word, choose, remove, list, randrep, rep, perhaps, smartjoin, upcase, loadOptions, loadList)

import Control.Monad (forM)
import Control.Monad.Loops (iterateUntil)
import Data.List (intercalate)
import System.Random 
import qualified Data.Text as T
import qualified Data.Text.IO as Tio



-- Two problems to work on

-- 1) a combinator like the 'rep' which stops when the text is long enough,
-- not based on reps. Note that TextGen works on any type so there'll need
-- to be a metric function or condition

-- Type would be ( [ a ] -> Bool ) -> TextGen g [ a ] -> TextGen g [ a ]
-- where ( [ a ] -> Bool ) is the criterion for finishing

-- Note that this can't be a combinator, it has to be in IO to have rendered
-- the TextGens

type TextGenCh = [ TextGen StdGen [[Char]] ]

name = choose $ map word [ "Arnold", "Betty", "Charles", "Davina", "Edgar", "Felicity", "George", "Hortense", "Ignatz", "Jenny", "Karl", "Lorelei", "Martin", "Nina", "Oliver", "Penelope", "Quentin", "Rose", "Stephen", "Tarin", "Umberto", "Veronica", "Wayne", "Xanthippe", "Yorick", "Zuleika" ]

arrival = choose $ map word [ "rocked up", "appeared", "joined the party", "arrived", "showed up", "manifested" ]

-- generate = getStdRandom runTextGen

dumbjoin :: [ [ Char ] ] -> [ Char ]
dumbjoin s = intercalate " " s


newcomer :: [ TextGen StdGen [[Char]] ] -> IO ( [ TextGen StdGen [[Char]] ], TextGen StdGen [ [Char ] ] )
newcomer cast = do
  new <- getStdRandom $ runTextGen name
  newgen <- return $ word $ dumbjoin new
  newcast <- return $ (newgen:cast)
  return ( newcast, list [ newgen, arrival ] )
      
      


incidents :: Int -> [ TextGen StdGen [ [ Char ] ] ] -> IO [ Char ]
incidents l cast = do
  ( cast1, incident ) <- newcomer cast
  words <- getStdRandom $ runTextGen incident
  text <- return $ smartjoin words
  lp <- return $ length $ text
  case lp > l of
    True -> return text
    otherwise -> do
      rest <- incidents (l - lp) cast1
      return $ text ++ " " ++ rest



maybejoin (Just s) = smartjoin s
maybejoin Nothing  = ""

mountains = "./data/mountains.txt"


main :: IO ()
main = do
  results <- incidents 1000 []
  putStrLn results
