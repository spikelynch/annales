import TextGen (TextGen, runTextGen, word, choose, remove, list, randrep, rep, perhaps, smartjoin)

import Annales.Empire ( TextGenCh, Empire, court, emperor, initialiseEmpire, vocabGet, generate, dumbjoin, randn)

import Annales.Tribes ( newTribe, goneTribe )
import Annales.Courtiers ( newCourtier, deadCourtier )
import Annales.Deaths ( deathOf )
import Annales.Omens ( omen )

import System.Random



genEmperor :: Empire -> IO TextGenCh
genEmperor e = do
  epithet <- generate $ vocabGet e "epithets.txt"
  name <- generate $ vocabGet e "people.txt"
  longname <- return $ (dumbjoin name) ++ " the " ++ (dumbjoin epithet)
  return $ choose [ word (dumbjoin name), word longname ]



deadEmperor :: Empire -> IO ( Empire, TextGenCh )
deadEmperor e = do
  newe <- genEmperor e
  e' <- return $ e { emperor = newe }
  return ( e', list [ deathOf (emperor e), word "succeeded by", newe ] ) 



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
      return $ text ++ "\n" ++ rest



maybejoin (Just s) = smartjoin s
maybejoin Nothing  = ""









main :: IO ()
main = do
  empire <- initialiseEmpire "./data/"
  annales <- incidents 100000 empire
  putStrLn annales
