module Annales.Succession (
  probSuccession
  ,succession
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
  ,claimants
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

import Annales.Descriptions (
  descSuccession
  , descAcclamation
  , descWar
  , descBattle
  , descWinWar
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

import Annales.Omens ( omen )






-- succession: if there's an Emperor, do nothing
-- If there's not, see if there are claimants: this means there's
-- a civil war in progress
-- If there's no claimants, try to find an heir
-- if there are no heirs, start a civil war

probSuccession :: Empire -> Int
probSuccession e = case emperor e of
  Nothing -> 100
  _       -> 0


succession :: Empire -> IO ( Empire, TextGenCh )
succession e = do
  case claimants e of
    [] -> do
      ( mheir, newheirs ) <- return $ getHeir e
      case mheir of
        (Just newe) -> do
          (e', style ) <- makeEmperor e newe newheirs
          return ( e', descSuccession style )
        Nothing -> startWar e
    cs -> civilWar e



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



-- war logic

-- Think of a name (the War of X)
-- select 2-4 claimants from courtiers
-- every year there is a Battle of PLACE
-- each of the claimants has a chance of dying
-- if there is one claimant left, they become emperor
-- if there are none, pick a random one

-- If there is < 2 courtiers, avoid war
-- Otherwise start it

-- startWar is called from succession when there are no heirs
-- if there are 0 or 1 courtiers, it acclaims the only courtier
-- or picks a random acclamation.
-- otherwise it starts a war with 2+ claimants

startWar :: Empire -> IO ( Empire, TextGenCh )
startWar e = do
  case court e of
    [] -> randomAcclamation e
    c:cs -> do
      case null cs of
        True -> do
          e' <- return $ e { court = [] }
          acclamation e' c
        False -> do
          e' <- chooseClaimants e
          return ( e', descWar e' )

-- Move 2+ courtiers to claimants

chooseClaimants :: Empire -> IO Empire
chooseClaimants e = do
  n <- randn 3
  n1 <- return $ n + 2
  cls <- return $ take n1 (court e)
  cos <- return $ drop n1 (court e)
  return $ e { court = cos, claimants = cls } 





-- civilWar is called by succession until there
-- are no more claimants

-- This returns a TextGenCh of the kind list [ paragraph ]

civilWar :: Empire -> IO ( Empire, TextGenCh )
civilWar e = do
  ( ma, mb, remain ) <- pickCombatants $ claimants e
  case ma of
    Nothing -> randomAcclamation e
    Just a -> case mb of
      Nothing -> victory e a
      Just b -> do
        ( mv, bdescraw ) <- doBattle e a b
        bdesc <- return $ map (\p -> paragraph $ sentence p) bdescraw
        case mv of
          Nothing -> do
            return ( e, list bdesc )
          Just v -> do
            case remain of
              [] -> do
                ( e', vdesc ) <- victory e v
                e'' <- return $ e' { claimants = [] }
                return ( e'', list $ bdesc ++ [ paragraph $ sentence vdesc ]) 
              otherwise -> do
                e' <- return $ e { claimants = v:remain }
                return ( e', list bdesc )


pickCombatants :: [ Person ] -> IO ( Maybe Person, Maybe Person, [ Person ] )
pickCombatants ps = do
  ( al, ps' ) <- randRemove ps
  ( bl, ps'' ) <- randRemove ps'
  return ( l2m al, l2m bl, ps'' )
    where l2m []     = Nothing
          l2m (a:as) = Just a

-- Maybe person because some battles don't end the war

doBattle :: Empire -> Person -> Person -> IO ( Maybe Person, [ TextGenCh ] )
doBattle e a b = do
  w <- randn 4
  case w of
    0 -> return ( Just a, descBattle e a b (Just a) )
    1 -> return ( Just b, descBattle e b a (Just b) )
    otherwise -> return ( Nothing, descBattle e a b Nothing )


victory :: Empire -> Person -> IO ( Empire, TextGenCh )
victory e p = do
  ( e', style ) <- makeEmperor e p []
  return ( e', descWinWar e' style )


randomAcclamation :: Empire -> IO (Empire, TextGenCh )
randomAcclamation e = do
  p <- newPerson e
  acclamation e p

  
acclamation :: Empire -> Person -> IO (Empire, TextGenCh )
acclamation e p = do
  (e', style ) <- makeEmperor e p [] 
  return ( e', descAcclamation e' style )
  






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
                


