module Annales.Omens (omen) where

import Annales.Empire (
  TextGenCh
  ,Empire
  )

import Annales.Descriptions ( descOmen )





omen :: Empire -> IO ( Empire, TextGenCh )
omen e = return ( e, descOmen e )
