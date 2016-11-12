module Annales.Court (
  newCourtier
  ,deadCourtier
  ,deadEmperor
  ) where

import Annales.Empire (
  TextGenCh
  ,Empire
  ,emperor
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
      return ( e', deathOf $ word $ dumbjoin courtier ) 









deadEmperor :: Empire -> IO ( Empire, TextGenCh )
deadEmperor e = do
  newe <- newEmperor e
  e' <- return $ e { emperor = newe }
  return ( e', list [ deathOf (emperor e), word "succeeded by", newe ] ) 

newEmperor :: Empire -> IO TextGenCh
newEmperor e = do
  epithet <- generate $ vocabGet e "epithets"
  name <- generate $ vocabGet e "people"
  longname <- return $ (dumbjoin name) ++ " the " ++ (cap $ dumbjoin epithet)
  return $ choose [ word (dumbjoin name), word longname ]
