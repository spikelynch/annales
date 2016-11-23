module Annales.Empire (
  Empire
  ,TextGenCh
  ,Gender(..)
  ,Person(..)
  ,Forebear(..)
  ,emperor
  ,court
  ,consort
  ,claimants
  ,heirs
  ,tribes
  ,lineage
  ,year
  ,pName
  ,pAge
  ,pGender
  ,vocabGet
  ,personGet
  ,initialiseEmpire
  ,incrementYear
  ,agePerson
  ,yearDesc
  ,yearAbbrev
  ,generate
  ,dumbjoin
  ,wordjoin
  ,sentence
  ,cap
  ,phrase
  ,randn
  ,randPick
  ,randRemove
  ,chooseW
  ,showL
  ,paragraph
  ) where


import TextGen (
  TextGen(..)
  ,runTextGen
  ,word
  ,choose
  ,remove
  ,list
  ,randrep
  ,rep
  ,perhaps
  ,smartjoin
  ,loadOptions
  )

import Data.Map (Map)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Char (toUpper)
import qualified Data.Map as Map
import System.Directory (getDirectoryContents)
import Text.Regex.Posix
import System.Random (StdGen, getStdRandom, randomR)
import Text.Numeral.Roman (toRoman)



type TextGenCh = TextGen StdGen [[Char]]

data Forebear = Forebear [ Char ] Gender (Maybe Int)
  deriving Show

data Gender = Male | Female
  deriving (Eq, Show)

data Person = Person TextGenCh Int Gender
    
data Empire = Empire { emperor :: Maybe Person
                     , consort :: Maybe Person
                     , year :: Int
                     , lineage :: [ Forebear ]
                     , heirs :: [ Person ]
                     , claimants :: [ Person ]
                     , court :: [ Person ]
                     , tribes :: [ TextGenCh ]
                     , projects :: [ TextGenCh ]
                     , vocab :: Map String TextGenCh
                     }


pName :: Person -> TextGenCh
pName (Person t _ _) = t

pAge :: Person -> Int
pAge (Person _ a _) = a

pGender :: Person -> Gender
pGender (Person _ _ g) = g

agePerson :: Person -> Person
agePerson p = (Person (pName p) ((pAge p) + 1) (pGender p)) 

nullGen :: TextGenCh
nullGen = word "-"

generate g = getStdRandom $ runTextGen g

chooseW :: [ [ Char ] ] -> TextGenCh
chooseW l = choose (map word l)

dumbjoin :: [ [ Char ] ] -> [ Char ]
dumbjoin s = intercalate " " s

cap :: [ Char ] -> [ Char ]
cap [] = []
cap (x:xs) = (toUpper x):xs 

wordjoin :: [ [ Char ] ] -> TextGenCh
wordjoin = word . dumbjoin

phrase :: TextGenCh -> TextGenCh
phrase g = list [ comma, g, comma ]
  where comma = word ","

paragraph :: TextGenCh -> TextGenCh
paragraph g = list [ word "¶", g, word "\n\n" ]

-- A combinator which wraps a generator in the sentence formatter

sentence :: TextGenCh -> TextGenCh
sentence g = TextGen $ \s -> let (TextGen gf) = g
                                 ( raw, s' ) = gf s
                             in ( [ smartjoin raw ], s' )

  
randn :: Int -> IO Int
randn n = do
  r <- getStdRandom $ randomR ( 0, n - 1 )
  return r

showL :: [ TextGenCh ] -> IO [ Char ]
showL []     = return ""
showL (g:gs) = do
  gt <- generate g
  gtr <- showL gs
  return $ (smartjoin gt) ++ ", " ++ gtr





vocabGet :: Empire -> String -> TextGenCh
vocabGet e name = case Map.lookup (name ++ ".txt") (vocab e) of
  Nothing -> nullGen
  Just gen -> gen


isTextFile :: String -> Bool
isTextFile f = f =~ ".txt$"


loadVocab :: String -> IO ( Map String TextGenCh )
loadVocab dir = do
  files <- getDirectoryContents dir
  list <- mapM loadFile $ filter isTextFile files
  return $ Map.fromList list
    where loadFile f = do
            gen <- loadOptions ( dir ++ f )
            return ( f, gen )
  
sampleVocab :: Map String TextGenCh -> String -> IO ( [ Char ] )
sampleVocab vocab name = do
  mgen <- return $ Map.lookup name vocab
  case mgen of
    (Just gen) -> do
      genc <- generate gen
      return $ dumbjoin genc
    Nothing -> return ( "Vocab file not found: " ++ name )


personGet :: Empire -> TextGenCh
personGet e = choose [ vocabGet e "men", vocabGet e "women" ]




initialE = Empire { emperor = Nothing
                  , consort = Nothing
                  , year = 0
                  , lineage = []
                  , heirs = []
                  , claimants = []
                  , court = []
                  , tribes =  []
                  , projects = []
                  , vocab = Map.empty
                  }


incrementYear :: Empire -> Empire
incrementYear e = e {
  emperor = agee
  , year = year'
  , heirs = ageheirs
  , court = agecourt
  }
  where year' = 1 + (year e)
        agee = case emperor e of
          Nothing -> Nothing
          (Just emp) -> Just $ agePerson emp
        ageheirs = map agePerson $ heirs e
        agecourt = map agePerson $ court e
        -- add courtiers 

yearDesc :: Empire -> TextGenCh
yearDesc e = paragraph $ sentence $ yearof $ emperor e
  where yearof Nothing = word "Interregnum"
        yearof (Just em) = list [ word "Year", a, word "in the reign of", g ]
          where a = word $ show $ year e
                g = pName $ em

yearAbbrev :: Empire -> TextGenCh
yearAbbrev e = paragraph $ word $ yearcode $ emperor e
  where yearcode Nothing = "INT." ++ (show $ year e)
        yearcode (Just emp) = (show $ year e) ++ "." ++ ecode
          where ecode = case lineage e of
                  []   -> ""
                  (Forebear n _ i):ls -> (initials n) ++ (roman i)
                    where roman Nothing = ""
                          roman (Just i) = "." ++ toRoman i
                          initials n = concat $ map initial $ splitOn " " n
                          initial []   = ""
                          initial (c:cs) = [ c ]
          

initialiseEmpire :: String -> IO ( Empire )
initialiseEmpire dir = do
  v <- loadVocab dir
  return $ initialE { vocab = v }
  




removel :: Int -> [ a ] -> (  [ a ], [ a ] )
removel i l = let ( h, t ) = splitAt i l
          in case ( h, t ) of
               ( _, [] ) -> ( [], l )
               ( h, t:ts ) -> ( [ t ], h ++ ts )


randPick :: [ a ] -> IO (Maybe a)
randPick [] = return Nothing
randPick as = do
  r <- randn $ length as
  ( p, _ ) <- return $ removel r as
  case p of
    [] -> return Nothing
    p:ps -> return $ Just p


randRemove :: [ a ] -> IO ( [ a ], [ a ] )
randRemove [] = return ( [], [] )
randRemove as = do
   r <- randn $ length as
   return $ removel r as
