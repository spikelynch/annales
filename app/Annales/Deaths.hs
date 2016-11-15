module Annales.Deaths ( deathOf ) where

import TextGen (word, aan, choose, list)

import Annales.Empire ( TextGenCh, Empire, chooseW, vocabGet )
  


deathOf :: Empire -> TextGenCh -> TextGenCh
deathOf e person = list [ person, death e ]

death :: Empire -> TextGenCh
death e = choose [ weapon e, choke e, beast e, disease e, poison e, witchcraft e ]

weapon e = choose [ accident e, battle e ]

accident e = list [ word "was nicked with", aweapon, word "and the wound festered" ]
  where aweapon = aan $ vocabGet e "weapons"


battle e = list [ word "was wounded in battle by", aan $ vocabGet e "weapons", word "through the", vocabGet e "body_parts" ]

choke e = list [ word "choked on", aan $ choose [ bone, other ] ]
  where bone = list [ vocabGet e "animals", word "bone" ]
        other = vocabGet e "foods"

beast e = list [ verbedby, aan $ vocabGet e "animals" ]
  where verbedby = chooseW [ "was stung by", "was bitten by", "was allergic to", "swallowed", "was eaten by" ]


disease e = list [ chooseW [ "succumbed to", "died of", "was taken by" ], vocabGet e "diseases" ]

poison e = list [ word "ate poisoned", vocabGet e "foods" ]

witchcraft e = chooseW [ "was ensorcelled", "was beguiled", "was spellbound", "succumbed to a geas" ] 


