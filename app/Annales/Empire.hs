module Annales.Empire (
  Empire
  ,TextGenCh
  ,emperor
  ,court
  ,tribes
  ,vocabGet
  ,initialiseEmpire
  ,generate
  ,dumbjoin
  ,wordjoin
  ,phrase
  ,randn
  ) where


import TextGen (
  TextGen
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
import qualified Data.Map as Map
import System.Directory (getDirectoryContents)
import Text.Regex.Posix
import System.Random (StdGen, getStdRandom, randomR)



type TextGenCh = TextGen StdGen [[Char]]

  
data Empire = Empire { emperor :: TextGenCh
                     , forebears :: [ TextGenCh ]
                     , court :: [ TextGenCh ]
                     , tribes :: [ TextGenCh ]
                     , enemies :: [ TextGenCh ]
                     , projects :: [ TextGenCh ]
                     , vocab :: Map String TextGenCh
                     }


nullGen :: TextGenCh
nullGen = word "-"


generate g = getStdRandom $ runTextGen g

dumbjoin :: [ [ Char ] ] -> [ Char ]
dumbjoin s = intercalate " " s

wordjoin :: [ [ Char ] ] -> TextGenCh
wordjoin = word . dumbjoin

phrase :: TextGenCh -> TextGenCh
phrase g = list [ comma, g, comma ]
  where comma = word ","

randn :: Int -> IO Int
randn n = do
  r <- getStdRandom $ randomR ( 0, n - 1 )
  return r



vocabGet :: Empire -> String -> TextGenCh
vocabGet e name = case Map.lookup name (vocab e) of
  Nothing -> nullGen
  Just gen -> gen

-- loadVocab :: String -> IO ( Map String TextGenCh )

-- This should scan the directory and load every file mapping it to its
-- filename

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

initialE = Empire { emperor = word "Fred the great"
                  , forebears = []
                  , court = []
                  , tribes =  []
                  , enemies = []
                  , projects = []
                  , vocab = Map.empty
                  }



initialiseEmpire :: String -> IO ( Empire )
initialiseEmpire dir = do
  v <- loadVocab dir
  return $ initialE { vocab = v }
  
