module Annales.Deaths ( deathOf, deathProbs ) where

import TextGen (word, aan, choose, list)

import Annales.Empire (
  TextGenCh
  ,Empire
  ,Person(..)
  ,chooseW
  ,vocabGet
  ,emperor
  ,consort
  ,heirs
  ,court
  ,pName
  ,pAge
  ,generate
  ,dumbjoin
  ,removePerson
  )

import Annales.Omens ( omen )

-- mortality table from the Roman Empire
-- http://www.richardcarrier.info/lifetbl.html

mortality :: Int -> Int
mortality a
  | a == 0  = 36
  | a == 1  = 24
  | a <= 5  = 6
  | a <= 10 = 5
  | a <= 15 = 7
  | a <= 20 = 8
  | a <= 25 = 9
  | a <= 30 = 11
  | a <= 35 = 12
  | a <= 40 = 14
  | a <= 45 = 17
  | a <= 50 = 21
  | a <= 55 = 25
  | a <= 60 = 33
  | a <= 65 = 41
  | a <= 70 = 53
  | a <= 75 = 68
  | otherwise = 90



deathOf :: Empire -> TextGenCh -> TextGenCh
deathOf e person = list [ person, death e ]

death :: Empire -> TextGenCh
death e = choose [ choke e, beast e, disease e, poison e, witchcraft e ]


choke e = list [ word "choked on", aan $ choose [ bone, other ] ]
  where bone = list [ vocabGet e "animals", word "bone" ]
        other = vocabGet e "foods"

beast e = list [ verbedby, aan $ vocabGet e "animals" ]
  where verbedby = chooseW [ "was stung by", "was bitten by", "was allergic to", "swallowed", "was eaten by" ]


disease e = list [ chooseW [ "succumbed to", "died of", "was taken by" ], vocabGet e "diseases" ]

poison e = list [ word "ate poisoned", vocabGet e "foods" ]

witchcraft e = chooseW [ "was ensorcelled", "was beguiled", "was spellbound", "succumbed to a geas" ] 



-- calculate a death probability for everyone at court, and give them
-- each an incident to be run

deathProbs :: Empire -> [ ( (Empire -> Int), (Empire -> IO (Empire, TextGenCh )) )  ]
deathProbs e = emp ++ cons ++ hs ++ cs
  where emp     = emperorD e
        cons    = consortD e
        hs      = groupD e (heirs e) updateHeirs 
        cs      = groupD e (court e) updateCourt 

-- FIXME - heirs and court need to have the dead people removed

emperorD :: Empire -> [ ( (Empire -> Int), (Empire -> IO (Empire, TextGenCh )) )  ]
emperorD e = case emperor e of
  Nothing  -> []
  Just emp -> [ ( deathProb emp, deadEmperor ) ] 

consortD :: Empire -> [ ( (Empire -> Int), (Empire -> IO (Empire, TextGenCh )) )  ]
consortD e = case consort e of
  Nothing -> []
  Just cons -> [ ( deathProb cons, deadConsort ) ]


-- here follows the worst type signature I have ever written. *coughs*:

groupD :: Empire -> [ Person ] -> (Empire -> [ Person ] -> Empire ) -> [ ( (Empire -> Int), (Empire -> IO (Empire, TextGenCh )) )  ]
groupD e ps update = map gD ps 
  where gD p = ( deathProb p, inc p )
        inc p = \e -> do
          ( remaining, desc )  <- deathRemove e ps p
          e' <- return $ update e remaining
          return ( e', desc )

-- record update is syntax sugar and this is the only way I know
-- to parametrise it

updateHeirs :: Empire -> [ Person ] -> Empire
updateHeirs e ps = e { heirs = ps }

updateCourt :: Empire -> [ Person ] -> Empire
updateCourt e ps = e { court = ps }


deathRemove :: Empire -> [ Person ] -> Person -> IO ( [ Person ], TextGenCh )
deathRemove e ps p = do
  name <- return $ pName p
  remain <- removePerson p ps
  return ( remain, deathOf e name )


  
-- take a Person and return a death probability
-- LATER; we can use this to increase mortality in war, plague

deathProb :: Person -> (Empire -> Int)
deathProb p = (\_ -> mortality (pAge p))

-- deadEmperor used to do the new emperor selection as well, but now
-- that's its own incident, triggered by an absence of emperor

deadEmperor :: Empire -> IO ( Empire, TextGenCh )
deadEmperor e = do
  case emperor e of
    (Just olde) -> do
      e' <- return $ e { emperor = Nothing, consort = Nothing }
      death <- return $ deathOf e $ pName olde
      return ( e', death )
    Nothing -> omen e

deadConsort :: Empire -> IO ( Empire, TextGenCh )
deadConsort e = do
  case consort e of
    (Just cons) -> do
      e' <- return $ e { consort = Nothing }
      death <- return $ deathOf e $ pName cons
      return ( e', death )
    Nothing -> omen e
