import TextGen (TextGen, runTextGen, word, choose, remove, list, randrep, rep, perhaps, smartjoin)

import Annales.Empire ( TextGenCh, Empire, court, emperor, initialiseEmpire, vocabGet, generate, dumbjoin, randn)

import Annales.Court ( newCourtier, deadCourtier, deadEmperor )
import Annales.Tribes ( newTribe, goneTribe )
import Annales.Omens ( omen )

import System.Random





incident :: Empire -> IO ( Empire, TextGenCh )
incident e = do
  r <- randn 6
  case r of
    0         -> newTribe e
    1         -> goneTribe e
    2         -> newCourtier e
    3         -> deadCourtier e
    4         -> deadEmperor e
    otherwise -> omen e


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
      return $ text ++ "\n\n" ++ rest



maybejoin (Just s) = smartjoin s
maybejoin Nothing  = ""









main :: IO ()
main = do
  empire <- initialiseEmpire "./data/"
  annales <- incidents 100000 empire
  putStrLn annales
