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


main = do
  Right x <- parseFile "1.conf"
  let y = buildTrie x
  let (y', alphabet)  = recode y
  let (y'', values)   = normalize y'
  let y'''            = improve 0 y''
  let r               = flatten y'''
  output $ generate r alphabet values