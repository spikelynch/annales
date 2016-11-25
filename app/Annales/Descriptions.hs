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
  ,capg
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

w :: [ Char ] -> TextGenCh
w = word

ch :: [ TextGenCh ] -> TextGenCh
ch = choose

chw :: [[ Char ]] -> TextGenCh
chw = chooseW
--
--
-- Successions, wars and battles
--
--

descSuccession :: TextGenCh -> TextGenCh
descSuccession style = inc [ w "Succession of", style ]

descWar :: Empire -> TextGenCh
descWar e = let v = vocabGet e
                name = ch [ woq, adjw ]
                woq = list [ w "War of", capg $ v "qualities" ]
                adjw = list [ capg $ v "epithets", w "War" ]
                forces = nicelist $ map pName $ claimants e
                met = chw [ "were joined", "battled", "clashed", "disagreed", "contended", "disputed" ]
                s1 = inc [ forces, met, w "in the", name ]
                s2 = inc [ w "Now began the", name, w "in which", forces, met ]
            in ch [ s1, s2 ]

descAcclamation :: Empire -> TextGenCh -> TextGenCh
descAcclamation e style = inc [ style, vocabGet e "enthroned", vocabGet e "acclamations" ]


-- these two are not wrapped in inc because they will be returned as separate
-- paragraphs

descBattle :: Empire -> Person -> Person -> TextGenCh
descBattle e a b = list [ pName a, w "and", pName b, w "contended in battle: ", pName a, w "was the victor" ]

descWinWar :: Empire -> TextGenCh -> TextGenCh
descWinWar e style = list [ style, vocabGet e "enthroned", w "triumph" ]

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
                       v = vocabGet e
                       waswed = w "was wedded to"
                       celebrated = list [ w "with", v "festivities" ]
                   in inc [ eg, waswed, cg, celebrated ]


descBirth e mother baby = let (Person pg _ g) = baby
                              (Person mg _ _) = mother
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
death e = ch [ choke e, beast e, disease e, poison e, witchcraft e ]


choke e = list [ w "choked on", aan $ ch [ bone, other ] ]
  where bone = list [ vocabGet e "animals", w "bone" ]
        other = vocabGet e "foods"

beast e = list [ verbedby, aan $ vocabGet e "animals" ]
  where verbedby = chw [ "was stung by", "was bitten by", "was allergic to", "swallowed", "was eaten by" ]


disease e = list [ chw [ "succumbed to", "died of", "was taken by" ], vocabGet e "diseases" ]

poison e = list [ chw [ "ate poisoned", "ate rotten", "ate spoiled" ], vocabGet e "foods" ]

witchcraft e = chw [ "was ensorcelled", "was beguiled", "was spellbound", "succumbed to a geas" ] 

--
--
-- Courtiers
--
--

-- things courtiers can do: write poems and plays and histories,
-- intrigue, win triumphs, be exiled to PLACE, retire to their
-- villa/etc in PLACE, sponsor games, projects



descCourtier :: Empire -> Person -> TextGenCh
descCourtier e p = let arrived = chw [ "rose to prominence", "won favour", "was first heard of", "rose through the ranks", "was promoted" ]
                   in inc [ pName p, arrived ]


descCourtDouble :: Empire -> Person -> TextGenCh
descCourtDouble e p = inc [ w "Fearful omen of a doppleganger of", pName p, w "at court" ]


descCourtierGo :: Empire -> Person -> TextGenCh
descCourtierGo e p = let gone = chw [ "was exiled", "fell into disgrace", "Fell under a shadow", "became unfashionable", "was banished" ]
                   in inc [ pName p, gone ]



--
--
-- Tribes
--
--

descTribe :: Empire -> TextGenCh -> TextGenCh
descTribe e t = let v = vocabGet e
                    nation = phrase $ aan $ list [ v "epithets", v "nations" ]
                    givento = list [ v "proneto", v "immorality" ]
                    worship = list [ v "worshipping", perhaps (1, 2) $ v "divine", v "gods" ]
                    clause = perhaps (2, 3) $ phrase $ ch [ givento, worship ]
                    arose = list [ w "arose in", v "places" ]
                in inc [ w "The", t, nation, clause, arose ]

descTribeGo :: Empire -> TextGenCh -> TextGenCh
descTribeGo e tribe = let v = vocabGet e
                          went = ch [ dwindled, conquered, migrated, fled ]
                          dwindled = chw [ "dwindled", "dissolved", "failed" ]
                          conquered = list [ w "were conquered by the", vocabGet e "tribes" ]
                          migrated = list [ w "migrated to the", chw [ "north", "west", "east", "south" ] ]
                          fled = list [ cursed, vocabGet e "phenomena" ]
                          cursed = chw [ "were cursed with", "fled the", "fled in the face of" ]
                      in inc [ w "The", tribe, went ]



descOmen :: Empire -> TextGenCh
descOmen e = inc [ collective, phenom, w "in", place ]
  where phenom = vocabGet e "phenomena"
        place = vocabGet e "places"


collective :: TextGenCh
collective = chw [ "Outbreak of", "Panic caused by", "Great", "Reports of", "Rumours of" ]
