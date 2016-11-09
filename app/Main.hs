import TextGen (TextGen, runTextGen, word, choose, remove, list, randrep, rep, perhaps, smartjoin)

import Annales.Empire ( TextGenCh, Empire, court, emperor, initialiseEmpire, vocabGet, generate, dumbjoin, randn)
  

import System.Random



choiceGen l = choose $ map word l 


arrived = choiceGen [ "appeared", "rose to prominence", "won favour" ]

death = choiceGen [ "disappeared", "was assassinated", "drowned in the baths", "choked on a chicken bone" ]

disasters = choiceGen [ "Great storms", "Winds", "Nightmares", "Evil omens" ]




disaster :: Empire -> IO ( Empire, TextGenCh )
disaster e = return ( e, disasters )

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
  return ( e', list [ emperor e, death, word "succeeded by", newe ] ) 


newCourtier :: Empire -> IO ( Empire, TextGenCh )
newCourtier e = do
  new  <- generate $ vocabGet e "people.txt"
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
    2         -> deadEmperor e
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









main :: IO ()
main = do
  empire <- initialiseEmpire "./data/"
  annales <- incidents 1000 empire
  putStrLn annales
