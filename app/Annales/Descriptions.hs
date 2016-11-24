module Annales.Descriptions (
  descSuccession
  , descAcclamation
  , descWar
  , descBattle
  , descWinWar
  , descWedding
  , descBirth
  , descDeathOf
  , descTribe
  , descTribeGo
  , descCourtier
  , descCourtDouble
  , descCourtierGo
  , descOmen
  ) where
  
import Annales.Empire (
  TextGenCh
  ,Empire
  ,Person(..)
  ,Gender(..)
  ,claimants
  ,emperor
  ,generate
  ,vocabGet
  ,pName
  ,dumbjoin
  ,wordjoin
  ,nicelist
  ,inc
  ,paragraph
  ,sentence
  ,cap
  ,randn
  ,randRemove
  ,chooseW
  ,phrase
  )

import TextGen (
  TextGen
  ,word
  ,choose
  ,remove
  ,perhaps
  ,list
  ,aan
  )


--
--
-- Successions, wars and battles
--
--

descSuccession :: TextGenCh -> TextGenCh
descSuccession style = inc [ word "Succession of", style ]

descWar :: Empire -> TextGenCh
descWar e = inc [ word "Now began the War of", wname, word "in which", cs, word "contended" ]
  where wname = vocabGet e "places"
        cs = nicelist $ map pName $ claimants e

descAcclamation :: Empire -> TextGenCh -> TextGenCh
descAcclamation e style = inc [ style, vocabGet e "enthroned", vocabGet e "acclamations" ]


-- these two are not wrapped in inc because they will be returned as separate
-- paragraphs

descBattle :: Empire -> Person -> Person -> TextGenCh
descBattle e a b = list [ pName a, word "and", pName b, word "contended in battle: ", pName a, word "was the victor" ]

descWinWar :: Empire -> TextGenCh -> TextGenCh
descWinWar e style = list [ style, vocabGet e "enthroned", word "triumph" ]

--
--
--  Marriages and births
--
--

descWedding :: Empire -> TextGenCh -> TextGenCh
descWedding e cg = let me = emperor e
                       eg = case me of
                         Just emp -> pName emp
                         Nothing -> word "ERROR"
                       w = word
                       v = vocabGet e
                       waswed = w "was wedded to"
                       celebrated = list [ w "with", v "festivities" ]
                   in inc [ eg, waswed, cg, celebrated ]


descBirth e mother baby = let (Person pg _ g) = baby
                              (Person mg _ _) = mother
                              w = word
                              v = vocabGet e
                              child = if g == Male then w "son" else w "daughter"
                              star = perhaps ( 3, 5 ) $ list [ w "under the star", v "stars" ]
                          in inc [ mg, w "was brought to bed of a", child, phrase pg, star ]

--
--
-- Deaths (other than in battle)
--
--


descDeathOf :: Empire -> TextGenCh -> TextGenCh
descDeathOf e person = inc [ person, death e ]

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

--
--
-- Courtiers
--
--

-- things courtiers can do: write poems and plays and histories,
-- intrigue, win triumphs, be exiled to PLACE, retire to their
-- villa/etc in PLACE, sponsor games, projects



descCourtier :: Empire -> Person -> TextGenCh
descCourtier e p = let arrived = chooseW [ "rose to prominence", "won favour", "was first heard of", "rose through the ranks", "was promoted" ]
                   in inc [ pName p, arrived ]


descCourtDouble :: Empire -> Person -> TextGenCh
descCourtDouble e p = inc [ word "Fearful omen of a doppleganger of", pName p, word "at court" ]


descCourtierGo :: Empire -> Person -> TextGenCh
descCourtierGo e p = let gone = chooseW [ "was exiled", "fell into disgrace", "Fell under a shadow", "became unfashionable", "was banished" ]
                   in inc [ pName p, gone ]



--
--
-- Tribes
--
--

descTribe :: Empire -> TextGenCh -> TextGenCh
descTribe e t = let v = vocabGet e
                    w = word
                    nation = phrase $ aan $ list [ v "epithets", v "nations" ]
                    givento = list [ v "proneto", v "immorality" ]
                    worship = list [ v "worshipping", perhaps (1, 2) $ v "divine", v "gods" ]
                    clause = perhaps (2, 3) $ phrase $ choose [ givento, worship ]
                    arose = list [ w "arose in", v "places" ]
                in inc [ w "The", t, nation, clause, arose ]

descTribeGo :: Empire -> TextGenCh -> TextGenCh
descTribeGo e tribe = let v = vocabGet e
                          w = word
                          went = choose [ dwindled, conquered, migrated, fled ]
                          dwindled = chooseW [ "dwindled", "dissolved", "failed" ]
                          conquered = list [ w "were conquered by the", vocabGet e "tribes" ]
                          migrated = list [ w "migrated to the", chooseW [ "north", "west", "east", "south" ] ]
                          fled = list [ cursed, vocabGet e "phenomena" ]
                          cursed = chooseW [ "were cursed with", "fled the", "fled in the face of" ]
                      in inc [ w "The", tribe, went ]



descOmen :: Empire -> TextGenCh
descOmen e = inc [ collective, phenom, word "in", place ]
  where phenom = vocabGet e "phenomena"
        place = vocabGet e "places"


collective :: TextGenCh
collective = choose $ map word [ "Outbreak of", "Panic caused by", "Great", "Reports of", "Rumours of" ]
