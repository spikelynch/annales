module Annales.Succession ( succession ) where

import Text.Numeral.Roman (toRoman)

import Annales.Empire (
  TextGenCh
  ,Empire
  ,Gender (..)
  ,Person (..)
  ,Forebear(..)
  ,emperor
  ,lineage
  ,consort
  ,heirs
  ,court
  ,year
  ,vocabGet
  ,personGet
  ,pName
  ,pAge
  ,pGender
  ,generate
  ,dumbjoin
  ,wordjoin
  ,cap
  ,randn
  ,chooseW
  ,phrase
  )

import Annales.Names ( newName, newPerson, royalBabyName )

import TextGen (
  TextGen
  ,word
  ,choose
  ,remove
  ,perhaps
  ,list
  )

import Annales.Deaths ( deathOf )
import Annales.Omens ( omen )





-- succession is triggered in the main incident loop by the absence
-- of an emperor.

-- Try to select an heir based on the succession rules

-- If there are claimants, it's a war of succession incident, which
-- may resolve to a new emperor

-- Notes on rewriting this:

-- Keep the "unsucceedable names" logic but mediated through heirship,
-- or abandon it and start from scratch (better)

-- Creating an heir: use the 'forebear or random' logic and add them
-- to the heir list

-- When the emperor dies, determine the heir using a successor rule
-- (male first, female first, etc)

-- Then add the heir to the Forebears list - maybe at this stage, if
-- they're new, decide if they're unsucceedable?

-- A mess to sort out: the old heirs need to stick around until they
-- die

-- heirs is [ [ Person ] ]

-- new Emperor: push an empty [] onto heirs
-- new heir: append to the head of heirs
-- pull heirs off

-- every year, increment all heir ages by one
-- every year, loop through all heirs and do a probability check to see
-- if they died

-- If the emperor dies and there are no living heirs:
-- one of the Court becomes emperor by a variety of means
-- Note this is part of a generic succession incident now,

-- A Note on Regnal Names

-- at birth - pick a random name, biased towards forebears, but never
-- one which is unsucceedable

-- at succession - take the heirs name, and generate the Forebear with
-- regnal number: (with an option to make it unsucceedable)




succession :: Empire -> IO ( Empire, TextGenCh )
succession e = do
  ( mheir, newheirs ) <- return $ getHeir e
  case mheir of
    (Just newe) -> do
      (e', style ) <- makeEmperor e newe newheirs
      return ( e', list [ word "Succession of", style ] )
    Nothing -> do
      (e', style ) <- acclamation e
      return ( e', acclamationDesc e style )



makeEmperor :: Empire -> Person -> [ Person ] -> IO ( Empire, TextGenCh )
makeEmperor e newe newheirs = do
  (Person hg _ eg) <- return newe
  newename <- generate hg
  forebear <- makeEmpName e eg $ dumbjoin newename 
  style <- return $ regnalStyle forebear
  ( intro, embellished ) <- embellishName e style
  e' <- return $ e {
    emperor = Just (Person embellished (pAge newe) (pGender newe)),
    year = 1,
    lineage = forebear:(lineage e),
    heirs = newheirs
    }
  return ( e', intro )


-- This makes some forebears unsucceedable

makeEmpName :: Empire -> Gender -> [ Char ] -> IO Forebear
makeEmpName e gender name = do
  (Forebear ng ge mr) <- return $ nextRegnal e gender name
  case mr of
    Nothing -> return $ Forebear ng ge Nothing
    (Just 1) -> do
      r <- randn 5
      case r of
        0 -> return $ Forebear ng ge Nothing
        otherwise -> return $ Forebear ng ge (Just 1)
    (Just i) -> return $ Forebear ng ge (Just i)


-- TODO: make this be one of the courtiers, after a possible
-- war of succession

acclamation :: Empire -> IO (Empire, TextGenCh )
acclamation e = do
  claimant <- newPerson e
  makeEmperor e claimant [] 

acclamationDesc :: Empire -> TextGenCh -> TextGenCh
acclamationDesc e style = list [ style, vocabGet e "enthroned", vocabGet e "acclamations" ]






nextRegnal :: Empire -> Gender -> [ Char ] -> Forebear
nextRegnal e g n = let match = (\(Forebear m _ _) -> m == n)
                       r = 1 + (length $ filter match $ lineage e)
                   in (Forebear n g (Just r))


regnalStyle :: Forebear -> TextGenCh
regnalStyle (Forebear n _ Nothing)  = word n
regnalStyle (Forebear n _ (Just i)) = list [ word n, word $ toRoman i ]

embellishName :: Empire -> TextGenCh -> IO ( TextGenCh, TextGenCh )
embellishName e style = do
  r <- randn 4
  case r of
    3 -> do
      epithet <- generate $ vocabGet e "epithets"
      stext <- generate style
      sname <- return $ ( dumbjoin stext )
      ep <- return $ ( cap $ dumbjoin epithet )
      longname <- return $ sname ++ " the " ++ ep
      nstyle <- return $ choose [ style, word longname ]
      phrases <- return $ choose [ word "later called the", word "surnamed the" ]
      intro <- return $ list [ word sname, phrase $ list [ phrases, word ep ] ]
      return ( intro, nstyle )
    otherwise -> return ( style, style )





      

startCivilWar :: Empire -> IO ( Empire, TextGenCh )
startCivilWar e = return ( e, word "Eternal war of succession" )



      
-- TODO: multiple levels of heirs

-- Default: favouring males

getHeir :: Empire -> ( Maybe Person, [ Person ] )
getHeir = successorM




successorM :: Empire -> ( Maybe Person, [ Person ] )
successorM e = case heir Male (heirs e) of
  (Just p, ps) -> ( Just p, ps )
  (Nothing, _) -> heir Female (heirs e)


successorF :: Empire -> ( Maybe Person, [ Person ] )
successorF e = case heir Female (heirs e) of
  (Just p, ps) -> ( Just p, ps )
  (Nothing, _) -> heir Male (heirs e)



-- heir finds the first heir of gender g, and returns
-- them and the modified list, or Nothing and the original list

heir :: Gender -> [ Person ] -> ( Maybe Person, [ Person ] )
heir hg hs = let notgender = (\(Person _ _ g) -> g /= hg)
                 h1 = takeWhile notgender hs
                 h2 = dropWhile notgender hs
             in case h2 of
                  []    -> ( Nothing, hs )
                  hh:hs -> ( Just hh, h1 ++ hs )
                


