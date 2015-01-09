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
  , flattenPack
  , buildTrie
  , Chunked
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

type Chunked a = Either (Maybe Int, [a]) (Map a Int)

flattenPack :: Int -> T a b -> [(Int, (Maybe b, Chunked a))]
flattenPack sz = snd . go 0
   where
     go i (T v m) = (i', (i, (v, m')):ls)
        where
          ((i',ls), m')
            | Map.size m == 1 =
                let (j, p, z) = go2 i $ head $ Map.toList m
                in ((j, z), Left p)
            | otherwise       = fmap Right $ Map.mapAccumWithKey f (i+1,[]) m
          f (j,ks) _ t  = let (j', ks') = go j t
                          in ((j', ks++ks'), j)
     go2 :: Int -> (a, T a b) -> (Int, (Maybe Int, [a]), [(Int, (Maybe b, Chunked a))])
     go2 i (k, t@(T v m))
           | Map.size m == 0 = (i, (Nothing, [k]), [])
           | Map.size m == 1 = let (i', (next,l), z) = go2 i $ head $ Map.toList m
                               in if length l < sz-2
                                  then (i', (next,k:l), z)
                                  else  (i'+1, (Just i', [k]), (i', (v,  Left (next, l))):z)
           | otherwise       = let (i', z) = go (i+1) t
	                       in (i', (Just (i+1), [k]), z)
