module Annales.Deaths ( deathOf ) where

import TextGen (word, choose, list)

import Annales.Empire ( TextGenCh )
  


deathOf :: TextGenCh -> TextGenCh
deathOf person = list [ person, death ]

death :: TextGenCh
death = word "died"

