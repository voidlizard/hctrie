-- | Copyright: 2015 (C) Alexander Vershilov <alexander.vershilov@gmail.com>
--   Author:    Alexander Vershilov <alexander.vershilov@gmail.com>
--
--   Provide utility function to work with the trie that contains,
--   data that is interesting for us.
--
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.TrieMap.Utils
  ( recode
  , normalize
  , improve
  , flatten
  , flattenPack
  , buildTrie
  , Chunked
  , pack
  , promote
  -- Packed value
  , Packed(..)
  , packedValue
  ) where

import           Data.TrieMap (T(..))
import qualified Data.TrieMap as Trie

import           Control.Applicative
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

{- If no values is assossiated with node and no path is compressed then
 - Nothing is stored in node, otherwise there is a packed value, 
 - when we are combining nodes we should care that we don't compress
 - nodes with different values inside them!
 -}

data Packed a b
      = Packed [a] (Maybe b)
      | NonPacked b
      deriving (Eq, Show)
 
instance Functor (Packed a) where
  fmap f (NonPacked b) = NonPacked (f b)
  fmap f (Packed a b)  = Packed a (fmap f b)

packedValue :: Packed a b -> Maybe b
packedValue (NonPacked b) = Just b
packedValue (Packed _  b) = b

appendPath :: Maybe (Packed a b) -> a -> Maybe (Packed a b)
appendPath Nothing a              = Just $ Packed [a] Nothing
appendPath (Just (NonPacked v)) a = Just $ Packed [a] (Just v)
appendPath (Just (Packed b v))  a = Just $ Packed (b++[a]) v

pack :: (Eq a, Eq b) => T a b -> T a (Packed a b)
pack (T v m) = loop (fmap NonPacked v) m
  where
    loop w m'
      | [(k, T w' m'')] <- Map.toList m' 
      , (w >>= packedValue) == w' = loop (w `appendPath` k) m''
    loop w m' = T w (Map.map pack m')

promote :: forall a b . T a (Packed a b) -> T a (Packed a (Either b b))
promote (T (Just v@(NonPacked _)) m)       = T (Just (fmap Right v)) (Map.map promote m)
promote (T (Just v@(Packed _ (Just _))) m) = T (Just (fmap Right v)) (Map.map promote m)
promote (T v m)  = T (f v closest) m'
  where
    m' = Map.map promote m
    closest :: Maybe b
    closest = fmap (either id id) $ Map.foldl (\mx (T l _) -> mx <|> (l >>= packedValue)) Nothing m'
    f :: Maybe (Packed a b) -> Maybe b -> Maybe (Packed a (Either b b))
    f Nothing w                   = fmap (NonPacked . Left) w
    f (Just (Packed l Nothing)) w = Just (Packed l (fmap Left w))
    f _ _ = error "impossible happened"


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
            | Map.size m == 1 && sz > 3 =
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
                                  else  (i'+1, (Just (i'+1), [k]), (i', (v,  Left (next, l))):z)
           | otherwise       = let (i', z) = go i t
	                       in (i'+1, (Just (i'+1), [k]), z)
