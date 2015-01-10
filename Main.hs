-- | Copyright: (c) 2015, Alexander Vershilov
--   Author:    Alexander Vershilov <alexander.vershilov@gmail.com>
-- 
module Main (main) where

import LCB.Generate
import Data.TrieMap
import Data.TrieMap.Utils

import Options
import Control.Applicative
import Control.Monad (forM_)
import Data.Maybe
import qualified Data.ByteString as B
import Text.PrettyPrint.Leijen.Text (displayT, renderPretty)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy    as Text
import qualified Data.Text.Lazy.IO as Text
import System.Exit

import Language.C.Generate.Parse


data MainOptions = MainOptions
  { moInput  :: String
  , moPrefix :: String
  , moStructName :: String
  , moHeader :: String 
  }

instance Options MainOptions where
  defineOptions = pure MainOptions
    <*> simpleOption "input" "" "input file"
    <*> simpleOption "prefix" "" "prefix in functions and files"
    <*> simpleOption "struct" "" "structure name"
    <*> simpleOption "headers" "" "additional header, comma separated list"

main = runCommand $ \opts args -> do
  let finput = moInput opts
  eny <- parseCSVLike <$> B.readFile finput
  case eny of
    Left s -> do putStrLn $ "Error reading file: " ++ s
                 exitFailure
    Right ny -> do
      let y = buildTrie $ map (\(Conf k v) -> (k, v)) ny
          (y', alphabet)  = recode y
          tests = map (\t -> (t, lookupG y t)) $ fullKeys y
      let xs = generateFiles (Text.pack $ moPrefix opts)
                             (Text.pack $ moStructName opts)
                             (filter (not.Text.null) 
                                        $ Text.split (==',')
                                        $ Text.pack $ moHeader opts)
                             alphabet
                             (promote $ pack y')
                             tests-- y''' r alphabet values tests

      forM_ xs $ \(f,p) ->
         Text.writeFile (Text.unpack f)
                        (displayT (renderPretty 0.6 80 p))

