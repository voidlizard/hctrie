-- | Copyright: 2015 (C) Alexander Vershilov <alexander.vershilov@gmail.com>
--   Author:    Alexander Vershilov <alexander.vershilov@gmail.com>
--
--   Provide utility function to work with the trie that contains,
--   data that is interesting for us.
--
{-# LANGUAGE TupleSections #-}
module Data.TrieMap.Utils
  ( recode
  , normalize
  , improve
  , flatten
  , buildTrie
  ) where

import           Data.TrieMap (T(..))
import qualified Data.TrieMap as Trie

import           Data.List
import           Data.Maybe
import qualified Data.Set as Set
import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Word

buildTrie :: [([Word8], a)] -> T Int a
buildTrie = Prelude.foldr (\(f,x) t' -> Trie.insert (map fromIntegral f) x t') Trie.empty

-- | Recode keys, so they use keys from alphabet
recode :: (Ord a) => T a b -> (T Int b, [a])
recode t = (Trie.first (\i -> succ $ fromJust $ i `elemIndex` m) t, m)
  where m = Set.toList $ Trie.keys t

-- | Normalize resuls.
normalize :: (Ord b) => T a b -> (T a Int, [b])
normalize t = (Trie.second (\i -> fromJust $ i `elemIndex` m) t, m)
  where m = Set.toList $ Trie.values t

improve :: (Eq b) => T a b -> T a b
improve (T Nothing m) = T k m'
  where m' = Map.map improve m
        k  | Map.size m' == 1 = (\(T v _) -> v) $ head $ Map.elems m'
	   | otherwise        = Nothing
improve (T v m)  = T v m

flatten :: T a b -> [(Int,(Maybe b, Map a Int))]
flatten = snd . go 0
  where
    go i (T v m) = (i', (i,(v,m')):ls)   -- XXX: rewrite improve so exposing internal structure will not be needed.
      where
        ((i',ls), m') = Map.mapAccumWithKey f (i+1,[]) m
	f (j,ks) _ t = let (j', ks') = go j t
	               in ((j', ks++ks'), j)
