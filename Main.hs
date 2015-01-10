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
  { moPrefix :: String
  , moStructName :: String
  , moHeader :: String
  , moTests  :: Bool
  }

instance Options MainOptions where
  defineOptions = pure MainOptions
    <*> simpleOption "prefix" "" "prefix in functions and files"
    <*> simpleOption "struct" "" "structure name"
    <*> simpleOption "headers" "" "additional header, comma separated list"
    <*> simpleOption "tests"   False "generate test files"

main = runCommand $ \opts args -> do
  input <- case args of
             [] -> B.getContents
             xs -> B.concat <$> mapM B.readFile xs
  let eny = parseCSVLike input
  case eny of
    Left s -> do putStrLn $ "Error reading file: " ++ s
                 exitFailure
    Right [] -> do putStrLn "Error parsing file, no data read"
                   exitFailure
    Right ny -> do
      let y = buildTrie $ map (\(Conf k v) -> (k, v)) ny
          (y', alphabet)  = recode y
          tests = map (\t -> (t, lookupG y t)) $ fullKeys y
      let xs = generateFiles (Text.pack $ moPrefix opts)
                             (Text.pack $ moStructName opts)
			     (moTests opts)
                             (filter (not.Text.null)
                                        $ Text.split (==',')
                                        $ Text.pack $ moHeader opts)
                             alphabet
                             (pack $ promote y')
                             tests

      forM_ xs $ \(f,p) ->
         Text.writeFile (Text.unpack f)
                        (displayT (renderPretty 0.6 80 p))

