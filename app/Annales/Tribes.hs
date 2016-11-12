module Annales.Tribes ( newTribe, goneTribe ) where

import Annales.Empire (
  TextGenCh
  ,Empire
  ,tribes
  ,vocabGet
  ,generate
  ,wordjoin
  )

import TextGen (
  TextGen
  ,word
  ,choose
  ,remove
  ,list
  )


import Annales.Omens ( omen )
 

newTribe :: Empire -> IO ( Empire, TextGenCh )
newTribe e = do
  tribe  <- generate $ vocabGet e "tribes.txt"
  tribeg <- return $ wordjoin tribe
  e'   <- return $ e { tribes = tribeg:(tribes e) }
  return ( e', tribeDescribe e tribeg )


tribeDescribe :: Empire -> TextGenCh -> TextGenCh
tribeDescribe e tribe = list [ word "The", tribe, word ",", nation, description, arose ]
  where nation = word "a new nation"
        description = list [ word ",", word "given to", vocabGet e "immorality.txt", word "," ]
        arose = list [ word "arose in", vocabGet e "places.txt" ]


goneTribe :: Empire -> IO ( Empire, TextGenCh )
goneTribe e = do
  ( maybetribe, tribes' ) <- generate $ remove $ tribes e
  case maybetribe of
    Nothing -> omen e
    Just tribe -> do
      e' <- return $ e { tribes = tribes' }
      return ( e', tribeGo tribe ) 

tribeGo :: [[Char]] -> TextGenCh
tribeGo tc = list [ word "The", wordjoin tc, went ]
  where went = choose $ map word [ "dwindled", "migrated south", "were cursed" ]
