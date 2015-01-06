-- | Copyright: (c) 2015, Alexander Vershilov
--   Author:    Alexander Vershilov <alexander.vershilov@gmail.com>
--
-- TODO:
--   * parse options
--   * generate files
--
module Main (main) where

import LCB.Parse
import LCB.Utils
import LCB.Generate
import Data.TrieMap

import Options
import Control.Applicative
import Control.Monad (forM_)
import Text.PrettyPrint.Leijen.Text
import qualified Data.Text.Lazy.IO as Text


data MainOptions = MainOptions
  { moInput  :: String
  , moPrefix :: String
  -- , moTests  :: Bool
  }

instance Options MainOptions where
  defineOptions = pure MainOptions
    <*> simpleOption "input" "" "input file"
    <*> simpleOption "prefix" "" "prefix in functions and files"
  --  <*> simpleOption "tests"  True "generate tests"

main = runCommand $ \opts args -> do
  let finput = moInput opts
  Right x <- parseFile finput
  let y = buildTrie x
  let tests = fullKeys y
  let (y', alphabet)  = recode y
  let (y'', values)   = normalize y'
  let y'''            = improve 0 y''
  let r               = flatten y'''
  -- output $ generate r alphabet values
  {-
  rnds <- mapM [1..128] $ \i -> do
    ls <- replicateM i (randomRIO (0,255))
  -}
  let xs = generateFiles {-(moPrefix opts)-} y''' r alphabet values tests
  forM_ xs $ \(f,p) ->
     Text.writeFile f (displayT (renderPretty 0.6 80 p))
