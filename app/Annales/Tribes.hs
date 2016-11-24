module Annales.Tribes ( newTribe, goneTribe ) where

import Annales.Empire (
  TextGenCh
  ,Empire
  ,tribes
  ,vocabGet
  ,generate
  ,wordjoin
  ,phrase
  ,chooseW
  ,inc
  )

import TextGen (
  TextGen
  ,word
  ,aan
  ,choose
  ,remove
  ,perhaps
  ,list
  )

import Annales.Descriptions ( descTribe, descTribeGo )

import Annales.Omens ( omen )
 

newTribe :: Empire -> IO ( Empire, TextGenCh )
newTribe e = do
  tribe  <- generate $ vocabGet e "tribes"
  tribeg <- return $ wordjoin tribe
  e'   <- return $ e { tribes = tribeg:(tribes e) }
  return ( e', descTribe e tribeg )


goneTribe :: Empire -> IO ( Empire, TextGenCh )
goneTribe e = do
  ( maybetribe, tribes' ) <- generate $ remove $ tribes e
  case maybetribe of
    Nothing -> omen e
    Just tribe -> do
      e' <- return $ e { tribes = tribes' }
      return ( e', descTribeGo e (wordjoin tribe) ) 

