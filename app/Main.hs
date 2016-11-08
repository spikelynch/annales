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

type TextGenCh = TextGen StdGen [[Char]]

choiceGen l = choose $ map word l 

names = choiceGen [ "Arnold", "Betty", "Charles", "Davina", "Edgar", "Felicity", "George", "Hortense", "Ignatz", "Jenny", "Karl", "Lorelei", "Martin", "Nina", "Oliver", "Penelope", "Quentin", "Rose", "Stephen", "Tarin", "Umberto", "Veronica", "Wayne", "Xanthippe", "Yorick", "Zuleika" ]

arrived = choiceGen [ "appeared", "rose to prominence", "won favour" ]

death = choiceGen [ "disappeared", "was assassinated", "drowned in the baths", "choked on a chicken bone" ]

disasters = choiceGen [ "Great storms", "Winds", "Nightmares", "Evil omens" ]










generate g = getStdRandom $ runTextGen g

dumbjoin :: [ [ Char ] ] -> [ Char ]
dumbjoin s = intercalate " " s


randn :: Int -> IO Int
randn n = do
  r <- getStdRandom $ randomR ( 0, n - 1 )
  return r


-- a TextGenCh can return random versions of a name

data Empire = Empire { emperor :: TextGenCh
                     , forebears :: [ TextGenCh ]
                     , court :: [ TextGenCh ]
                     , tribes :: [ TextGenCh ]
                     , enemies :: [ TextGenCh ]
                     , projects :: [ TextGenCh ]
                     }


disaster :: Empire -> IO ( Empire, TextGenCh )
disaster e = return ( e, disasters )


newCourtier :: Empire -> IO ( Empire, TextGenCh )
newCourtier e = do
  new  <- generate names
  newc <- return $ word $ dumbjoin new
  e'   <- return $ e { court = newc:(court e) }
  return ( e', list [ newc, arrived ] )

deadCourtier :: Empire -> IO ( Empire, TextGenCh )
deadCourtier e = do
  ( mdc, court' ) <- generate $ remove $ court e
  case mdc of
    Nothing -> disaster e
    Just left -> do
      e' <- return $ e { court = court' }
      return ( e', list [ word $ dumbjoin left, death ] ) 


incident :: Empire -> IO ( Empire, TextGenCh )
incident e = do
  r <- randn 3
  case r of
    0         -> newCourtier e
    1         -> deadCourtier e
    otherwise -> disaster e


showL :: [ TextGenCh ] -> IO [ Char ]
showL []     = return ""
showL (g:gs) = do
  gt <- generate g
  gtr <- showL gs
  return $ (smartjoin gt) ++ ", " ++ gtr



incidents :: Int -> Empire -> IO [ Char ]
incidents l e = do
  ( e', desc ) <- incident e
  words <- generate desc
  text <- return $ smartjoin words
  lp <- return $ length $ text
  case lp > l of
    True -> return text
    otherwise -> do
      rest <- incidents (l - lp) e'
      return $ text ++ "\n" ++ rest



maybejoin (Just s) = smartjoin s
maybejoin Nothing  = ""

initialE = Empire { emperor = word "Fred the great"
                  , forebears = []
                  , court = []
                  , tribes =  []
                  , enemies = []
                  , projects = []
                  }




main :: IO ()
main = do
  annals <- incidents 1000 initialE
  putStrLn annals
