-- | Copyright: 2015 (C) Alexander Vershilov <alexander.vershilov@gmail.com>
--   Author:    Alexander Vershilov <alexander.vershilov@gmail.com>
--
--   Provide utility function to work with the trie that contains,
--   data that is interesting for us.
--
--   TODO:
--
--     * rewrite improve so exposing internal structure will not be needed.
module LCB.Utils
  ( recode
  , normalize
  , improve
  , flatten
  , buildFingerprintTrie
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

buildFingerprintTrie :: Ini -> T Int ByteString
buildFingerprintTrie = go (Trie.singleton "")
 where go t [] = t
       go t ((Section _ vls):xs)
          | Just fp <- "fingerprints" `Prelude.lookup` vls
	  , Just [ds] <- "description"   `Prelude.lookup` vls
	  = go (Prelude.foldr (\f t' -> Trie.insert (prepare f) ds t') t fp) xs
	  where prepare k = read $ "[" ++ (B8.unpack k) ++ "]" :: [Int]
       go t (_:xs) = go t xs

-- | Recode keys, so they use keys from alphabet
recode :: (Ord a) => T a b -> (T Int b, [a])
recode t = (Trie.first (\i -> succ $ fromJust $ i `elemIndex` m) t, m)
  where m = Set.toList $ Trie.keys t

-- | Normalize resuls.
normalize :: (Ord b) => T a b -> (T a Int, [b])
normalize t = (Trie.second (\i -> fromJust $ i `elemIndex` m) t, m)
  where m = Set.toList $ Trie.values t

improve :: (Eq b) => b -> T a b -> T a b
improve zero (T x m)
  | x == zero = T k m'
  where m' = Map.map (improve zero) m
        k  | Map.size m' == 1 = (\(T v _) -> v) $ head $ Map.elems m'
	   | otherwise        = zero
improve _ (T v m)  = T v m

flatten :: T a b -> [(Int,(b, Map a Int))]
flatten = snd . go 0
  where
    go i (T v m) = (i', (i,(v,m')):ls)
      where
        ((i',ls), m') = Map.mapAccumWithKey f (i+1,[]) m
	f (j,ks) _ t = let (j', ks') = go j t
	               in ((j', ks++ks'), j)


