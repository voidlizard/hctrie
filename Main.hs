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

import Control.Monad (forM_)
import Text.PrettyPrint.Leijen.Text
import qualified Data.Text.Lazy.IO as Text

main = do
  Right x <- parseFile "1.conf"
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
  let xs = generateFiles y''' r alphabet values tests
  forM_ xs $ \(f,p) ->
     Text.writeFile f (displayT (renderPretty 0.6 80 p))
