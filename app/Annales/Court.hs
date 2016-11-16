module Annales.Court (
  newCourtier
  ,deadCourtier
  ,deadEmperor
  ) where

import Text.Numeral.Roman (toRoman)

import Annales.Empire (
  TextGenCh
  ,Empire
  ,emperor
  ,lineage
  ,court
  ,vocabGet
  ,generate
  ,dumbjoin
  ,cap
  ,randn
  ,chooseW
  )

import TextGen (
  TextGen
  ,word
  ,choose
  ,remove
  ,list
  )

import Annales.Deaths ( deathOf )
import Annales.Omens ( omen )
  


deadEmperor :: Empire -> IO ( Empire, TextGenCh )
deadEmperor e = do
  ( newe, name, regnal ) <- newEmperor e
  e' <- return $ e { emperor = newe, lineage = (name, regnal):(lineage e) }
  return ( e', list [ deathOf e (emperor e), word ": succession of", newe ] ) 


-- newEmperor returns the new emperor's Textgen, name and regnal number

newEmperor :: Empire -> IO ( TextGenCh, [ Char ], Int )
newEmperor e = do
  ( name, regnal ) <- do
    r <- randn 2
    case r of
      0          -> forebear e
      otherwise  -> do
        n <- randomName e
        return ( n, nextRegnal (lineage e) n )
  r <- randn 4
  case r of
    0 -> do
      epithet <- generate $ vocabGet e "epithets"
      longname <- return $ name ++ " the " ++ ( cap $ dumbjoin epithet)
      return ( choose [ rgen name regnal, word longname ], name, regnal )
    otherwise -> return ( rgen name regnal, name, regnal )


-- need to figure out how to 
rgen :: [ Char ] -> Int -> TextGenCh
rgen n i = list [ word n, word $ toRoman i ]

forebear :: Empire -> IO ( [ Char ], Int )
forebear e = do
  l <- return $ lineage e
  name <- lineageName e l
  return ( name, nextRegnal l name )

lineageName :: Empire -> [ ( [ Char ], Int ) ] -> IO [ Char ]
lineageName e []     = randomName e
lineageName e names  = do
  k <- randn 3
  case k of
    0 -> do
      i <- randn $ length names
      ( n, r ) <- return $ names !! i
      return n
    otherwise -> do
      ( n, r ) <- return $ head names
      return n

nextRegnal :: [ ( [ Char ], Int ) ] -> [ Char ] -> Int
nextRegnal l n = 1 + (length $ filter (\(x, _) -> x == n ) l)

randomName :: Empire -> IO [ Char ]
randomName e = do
  n <- generate $ vocabGet e "people"
  return $ dumbjoin n







arrived = chooseW [ "was born", "rose to prominence", "won favour", "was first heard of", "rose through the ranks", "was promoted" ]



newCourtier :: Empire -> IO ( Empire, TextGenCh )
newCourtier e = do
  new  <- generate $ vocabGet e "people"
  newc <- return $ word $ dumbjoin new
  e'   <- return $ e { court = newc:(court e) }
  return ( e', list [ newc, arrived ] )

deadCourtier :: Empire -> IO ( Empire, TextGenCh )
deadCourtier e = do
  ( mdc, court' ) <- generate $ remove $ court e
  case mdc of
    Nothing -> omen e
    Just courtier -> do
      e' <- return $ e { court = court' }
      return ( e', deathOf e $ word $ dumbjoin courtier ) 






