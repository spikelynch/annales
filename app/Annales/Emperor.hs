module Annales.Emperor (
  deadEmperor
  ,succession
  ,royalWedding
  ,royalBirth
  ,probBirth
  ) where

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



royalWedding :: Empire -> IO ( Empire, TextGenCh )
royalWedding e = do
  consortGender <- return $ case emperor e of
    (Just emp) -> if (pGender emp) == Male then Female else Male
    Nothing -> Female
  c <- newName e consortGender 
  cg <- return $ word c
  age <- randn 5
  e' <- return $ e { consort = Just (Person cg (16 + age) Female) }
  return ( e', desc e' cg )
    where desc e cg = let me = emperor e
                          eg = case me of
                            Just emp -> pName emp
                            Nothing -> word "ERROR"
                          w = word
                          v = vocabGet e
                          waswed = w "was wedded to"
                          celebrated = list [ w "with", v "festivities" ]
                      in list [ eg, waswed, cg, celebrated ]


royalBirth :: Empire -> IO ( Empire, TextGenCh )
royalBirth e = do
  case consort e of
    Nothing -> do
      putStrLn "ERROR royal birth without consort"
      return ( e, word "ERROR royal birth without consort" )
    Just cons -> do
      baby <- birth e
      mmother <- return $ femaleParent e
      case mmother of
        Just mother -> do
          e' <- return $ e { heirs = (heirs e) ++ [ baby ] }
          return ( e', birthDesc e' mother baby )
        Nothing ->
          return ( e, word "ERROR couldn't resolve parent" )

femaleParent :: Empire -> Maybe Person
femaleParent e = case consort e of
  Nothing -> Nothing
  (Just c) -> case pGender c of
                Female -> Just c
                Male -> emperor e


birthDesc e mother baby = let (Person pg _ g) = baby
                              (Person mg _ _) = mother
                              w = word
                              v = vocabGet e
                              child = if g == Male then w "son" else w "daughter"
                              star = perhaps ( 3, 5 ) $ list [ w "under the star", v "stars" ]
                          in list [ mg, w "was brought to bed of a", child, phrase pg, star ]
                     
maybeConsort :: Empire -> Person
maybeConsort e = case consort e of
  Just c -> c
  Nothing -> Person (word "FIXME") 1 Female


probBirth :: Empire -> Int
probBirth e = case consortAge e of
  Nothing    -> 0
  (Just x) | x > 49 -> 0
  (Just x) | x < 16 -> 0
  otherwise -> if isBaby e then 0 else 35

consortAge :: Empire -> Maybe Int
consortAge e = case consort e of
  Nothing             -> Nothing
  Just (Person _ a _) -> Just a

isBaby :: Empire -> Bool
isBaby e = case heirs e of
  [] -> False
  hs -> case pAge $ last hs of
    x | x < 3 -> True
    otherwise -> False


birth :: Empire -> IO Person
birth e = do
  r <- randn 2
  g <- return $ if r == 0 then Female else Male
  n <- royalBabyName e g
  return $ Person (word n) 1 g




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
  forebear <- return $ nextRegnal e eg $ dumbjoin newename
  style <- return $ regnalStyle forebear
  e' <- return $ e {
    emperor = Just (Person style (pAge newe) (pGender newe)),
    year = 1,
    lineage = forebear:(lineage e),
    heirs = newheirs
    }
  return ( e', style )


-- TODO: make this be one of the courtiers, after a possible
-- war of succession

acclamation :: Empire -> IO (Empire, TextGenCh )
acclamation e = do
  rg <- randn 2
  gender <- return $ case rg of
    0 -> Male
    1 -> Female
  age <- randn 20
  nclaimant <- newName e gender
  newe <- return $ Person (word nclaimant) (age + 10) gender
  makeEmperor e newe [] 

acclamationDesc :: Empire -> TextGenCh -> TextGenCh
acclamationDesc e style = list [ style, word "was made Emperor by", vocabGet e "acclamations" ]


nextRegnal :: Empire -> Gender -> [ Char ] -> Forebear
nextRegnal e g n = let match = (\(Forebear m _ _) -> m == n)
                       r = 1 + (length $ filter match $ lineage e)
                   in (Forebear n g (Just r))


regnalStyle :: Forebear -> TextGenCh
regnalStyle (Forebear n _ Nothing)  = word n
regnalStyle (Forebear n _ (Just i)) = list [ word n, word $ toRoman i ]



-- Leave name embellishments till later
--
-- embellishName e = do
--   r <- randn 4
--   case r of
--     3 -> do
--       epithet <- generate $ vocabGet e "epithets"
--       (Forebear name _) <- return newe
--       longname <- return $ name ++ " the " ++ ( cap $ dumbjoin epithet)
--       return ( (Person (choose [ rgen newe, word longname ]) 1 Male), newe )
--     otherwise -> return ( (Person (rgen newe) 1 Male), newe )


      

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
                

-- newEmperor returns the new emperor's Person And Forebear

-- newEmperor :: Empire -> IO ( Person, Forebear )
-- newEmperor e = do
--   newe <- do
--     r <- randn 4
--     case r of
--       0          -> forebear e
--       otherwise  -> newName e
--   r <- randn 4
--   case r of
--     3 -> do
--       epithet <- generate $ vocabGet e "epithets"
--       (Forebear name _) <- return newe
--       longname <- return $ name ++ " the " ++ ( cap $ dumbjoin epithet)
--       return ( (Person (choose [ rgen newe, word longname ]) 1 Male), newe )
--     otherwise -> return ( (Person (rgen newe) 1 Male), newe )


-- Returns a name of the correct gender, with a bias towards
-- forebears, avoiding unsucceedable forebears

royalBabyName :: Empire -> Gender -> IO [ Char ]
royalBabyName e g = do
  r <- randn 4
  case r of
    0         -> lineageName e g
    otherwise -> newName e g







-- Get a random name of the right gender which is not in the
-- lineage as 'unsucc'

newName :: Empire -> Gender -> IO [ Char ]
newName e g = do
  vocab <- return $ if g == Male then "men" else "women"
  unsucc <- return $ map (\(Forebear x _ _) -> x) $ filter isunsucc $ lineage e
  nn <- excludeGet unsucc (vocabGet e vocab)
  return nn


-- This could loop forever if lineage > names

excludeGet :: [ [ Char ] ] -> TextGenCh -> IO [ Char ]
excludeGet ex g = do
  nn <- generate g
  n <- return $ dumbjoin nn
  if n `elem` ex then excludeGet ex g else return n




-- TODO: get rid of the !! and head in this
-- now respects the Gender

lineageName :: Empire -> Gender -> IO [ Char ]
lineageName e g = do
  case filter (\(Forebear _ gg _) -> g == gg ) $ lineage e of
    []  -> newName e g
    fbs -> do
      succ <- return $ filter ( not . isunsucc ) fbs
      case null succ of
        True -> newName e g
        False -> do
          k <- randn 2
          case k of
            0 -> do
              i <- randn $ length succ
              (Forebear name _ _) <- return $ succ !! i
              return name
            otherwise -> do
              (Forebear name _ _) <- return $ head succ
              return name

isunsucc :: Forebear -> Bool
isunsucc (Forebear c _ Nothing) = True
isunsucc _                      = False


-- The next two are used at succession, not birth

-- forebear :: Empire -> IO Forebear
-- forebear e = do
--   l <- return $ lineage e
--   name <- lineageName e l
--   return $ nextRegnal l name





