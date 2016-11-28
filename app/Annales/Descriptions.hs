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
  ,possessive
  )

import TextGen (
  TextGen
  ,word
  ,choose
  ,weighted
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

-- add: war of N things

descWar :: Empire -> TextGenCh
descWar e = let v = vocabGet e
                name = ch [ woq, adjw ]
                woq = list [ w "War of", capg $ v "abstractions" ]
                adjw = list [ capg $ v "adjectives", w "War" ]
                forces = nicelist $ map pName $ claimants e
                met = chw [ "were joined", "battled", "clashed", "disagreed", "contended", "disputed", "sought mastery" ]
                began = chw [ "Now began the", "In this year was begun the", "Beginning of the" ]
                s1 = inc [ forces, met, w "in the", name ]
                s2 = inc [ began, name, w ",", w "in which", forces, met ]
            in ch [ s1, s2 ]

descAcclamation :: Empire -> TextGenCh -> TextGenCh
descAcclamation e style = inc [ style, vocabGet e "enthroned", vocabGet e "acclamations" ]


-- these two are not wrapped in inc because they will be returned as separate
-- paragraphs.

-- DescBattle should return a list of generators, each of which will be
-- an incident (wrapped in a sentence / paragraph )

descBattle :: Empire -> Person -> Person -> Maybe Person -> [ TextGenCh ]
descBattle e a b mv = let battle = choose [ ambush e a b ]
                      in case mv of
                           Nothing -> [ battle ]
                           Just _  -> [ battle, battleLoss e b ]

ambush :: Empire -> Person -> Person -> TextGenCh
ambush e a b = list [ pName a, perhaps ( 1, 3 ) $ aid e, ambushed ]
  where ambushed = ch [
          list [ chw [ "ambushed the", "surprised the"], forces, w "of", pName b ], 
          list [ w "took the", forces, w "of", pName b, w "all unawares" ]
          ]


          

forces = chw [ "legions", "armies", "forces", "warriors", "soldiers", "men" ]

aid :: Empire -> TextGenCh
aid e = phrase $ list [ aided, allies ]
  where v = vocabGet e
        aided = chw [ "with the aid of", "in league with", "allied with", "calling on" ]
        allies = list [ certain, v "adjectives", v "allies" ]
        certain = perhaps ( 1, 3 ) $ chw [ "certain", "some" ]
                  



battleLoss :: Empire -> Person -> TextGenCh
battleLoss e d = list [ pName d, died ]
  where died = chw [ "died", "was slain", "gave up the ghost", "left the field" ]












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
death e = weighted [ ( 5, choke e), (4, beast e), (71, disease e), (10, poison e), (10, witchcraft e) ]


choke e = list [ w "choked on", aan $ ch [ bone, other ] ]
  where bone = list [ vocabGet e "monsters", chw [ "bone", "shell"]  ]
        other = vocabGet e "foods"

beast e = list [ verbedby, aan $ vocabGet e "monsters" ]
  where verbedby = chw [ "was stung by", "was bitten by", "was allergic to", "swallowed", "was eaten by" ]


disease e = list [ chw [ "succumbed to", "died of", "was taken by" ], vocabGet e "diseases" ]

poison e = choose [ chaumas e, chaumurky e, chausmetics e ]

chaumas e = list [ w "ate", bad, vocabGet e "foods" ]
  where bad = chw [ "poisoned", "rotten", "bad", "spoiled", "tainted" ]

chaumurky e = list [ w "drank", bad, vocabGet e "drinks" ]
  where bad = chw [ "poisoned", "new", "sour", "tainted" ]

chausmetics e = list [ w "was poisoned with", vocabGet e "cosmetics" ]



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
                    nation = phrase $ aan $ list [ v "adjectives", v "nations" ]
                    givento = list [ v "proneto", v "abstractions" ]
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
