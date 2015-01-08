-- | Copyright: 2015 (C) Alexander Vershilov <alexander.vershilov@gmail.com>
--   Author:    Alexander Vershilov <alexander.vershilov@gmail.com>
--
--   Provide utility function to work with the trie that contains,
--   data that is interesting for us.
--
--   TODO:
--
--     * rewrite improve so exposing internal structure will not be needed.
{-# LANGUAGE TupleSections #-}
module LCB.Utils
  ( recode
  , normalize
  , improve
  , flatten
  , buildFingerprintTrie
  , buildTrie
  , prepareFingerprintValues
  ) where

import           LCB.Parse (Section(..), Ini)
import           Data.TrieMap (T(..))
import qualified Data.TrieMap as Trie

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import           Data.List
import           Data.Maybe
import qualified Data.Set as Set
import           Data.Map ( Map )
import qualified Data.Map as Map

import Language.C.Generate.Parse

buildFingerprintTrie :: Ini -> T Int ByteString
buildFingerprintTrie = go (Trie.singleton "")
 where go t [] = t
       go t ((Section _ vls):xs)
          | Just fp <- "fingerprints" `Prelude.lookup` vls
	  , Just [ds] <- "description"   `Prelude.lookup` vls
	  = go (Prelude.foldr (\f t' -> Trie.insert (prepare f) ds t') t fp) xs
	  where prepare k = read $ "[" ++ (B8.unpack k) ++ "]" :: [Int]
       go t (_:xs) = go t xs

prepareFingerprintValues :: Ini -> [(ByteString, [ParseValue])]
prepareFingerprintValues [] = []
prepareFingerprintValues ((Section _ vls):xs)
   | Just fp <- "fingerprints" `Prelude.lookup` vls
   , Just [ds] <- "description" `Prelude.lookup` vls
   = [(f, [PVBS ds]) | f <- fp] ++ prepareFingerprintValues xs
prepareFingerprintValues (_:xs) = prepareFingerprintValues xs

buildTrie :: [(ByteString, a)] -> T Int a
buildTrie = Prelude.foldr (\(f,x) t' -> Trie.insert (prepare f) x t') Trie.empty
  where prepare k = read $ "[" ++ (B8.unpack k) ++ "]" :: [Int]

        

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
    go i (T v m) = (i', (i,(v,m')):ls)
      where
        ((i',ls), m') = Map.mapAccumWithKey f (i+1,[]) m
	f (j,ks) _ t = let (j', ks') = go j t
	               in ((j', ks++ks'), j)


