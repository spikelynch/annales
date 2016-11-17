module Annales.Court (
  newCourtier
  ,deadCourtier
  ,deadEmperor
  ) where

import Text.Numeral.Roman (toRoman)

import Annales.Empire (
  TextGenCh
  ,Empire
  ,Forebear(..)
  ,emperor
  ,lineage
  ,court
  ,vocabGet
  ,generate
  ,dumbjoin
  ,cap
  ,randn
  ,chooseW
  )

import TextGen (
  TextGen
  ,word
  ,choose
  ,remove
  ,list
  )

import Annales.Deaths ( deathOf )
import Annales.Omens ( omen )
  


deadEmperor :: Empire -> IO ( Empire, TextGenCh )
deadEmperor e = do
  ( newe, forebear ) <- newEmperor e
  e' <- return $ e { emperor = newe, lineage = forebear:(lineage e) }
  return ( e', list [ deathOf e (emperor e), word ": succession of", newe ] ) 


-- newEmperor returns the new emperor's Textgen, name and regnal number

newEmperor :: Empire -> IO ( TextGenCh, Forebear )
newEmperor e = do
  newe <- do
    r <- randn 4
    case r of
      0          -> forebear e
      otherwise  -> newName e
  r <- randn 4
  case r of
    0 -> do
      epithet <- generate $ vocabGet e "epithets"
      (Forebear name _) <- return newe
      longname <- return $ name ++ " the " ++ ( cap $ dumbjoin epithet)
      return ( choose [ rgen newe, word longname ], newe )
    otherwise -> return ( rgen newe, newe )



rgen :: Forebear -> TextGenCh
rgen (Forebear n Nothing)  = word n
rgen (Forebear n (Just i)) = list [ word n, word $ toRoman i ]


-- make list selections safe


forebear :: Empire -> IO Forebear
forebear e = do
  l <- return $ lineage e
  name <- lineageName e l
  return $ nextRegnal l name


-- get rid of the !! and head in this
-- filter the forebears so that ones with regnal number = Nothing
-- don't have successors

lineageName :: Empire -> [ Forebear ] -> IO [ Char ]
lineageName e []   = do
  (Forebear name _) <- newName e
  return name
lineageName e fbs  = do
  k <- randn 2
  case k of
    0 -> do
      i <- randn $ length fbs
      (Forebear name _) <- return $ fbs !! i
      return name
    otherwise -> do
      (Forebear name _) <- return $ head fbs
      return name
      

-- this should sometimes return Forebears with a Nothing regnal number
-- who will never have successors

newName :: Empire -> IO Forebear
newName e = do
  n <- generate $ vocabGet e "people"
  return $ nextRegnal (lineage e) (dumbjoin n)



nextRegnal :: [ Forebear ] -> [ Char ] -> Forebear
nextRegnal l n = let r = 1 + (length $ filter (\(Forebear m _) -> m == n) l)
                 in (Forebear n (Just r))





arrived = chooseW [ "was born", "rose to prominence", "won favour", "was first heard of", "rose through the ranks", "was promoted" ]



newCourtier :: Empire -> IO ( Empire, TextGenCh )
newCourtier e = do
  new  <- generate $ vocabGet e "people"
  newc <- return $ word $ dumbjoin new
  e'   <- return $ e { court = newc:(court e) }
  return ( e', list [ newc, arrived ] )

deadCourtier :: Empire -> IO ( Empire, TextGenCh )
deadCourtier e = do
  ( mdc, court' ) <- generate $ remove $ court e
  case mdc of
    Nothing -> omen e
    Just courtier -> do
      e' <- return $ e { court = court' }
      return ( e', deathOf e $ word $ dumbjoin courtier ) 






