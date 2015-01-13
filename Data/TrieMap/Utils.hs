-- | Copyright: 2015 (C) Alexander Vershilov <alexander.vershilov@gmail.com>
--   Author:    Alexander Vershilov <alexander.vershilov@gmail.com>
--
--   Provide utility function to work with the trie that contains,
--   data that is interesting for us.
--
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.TrieMap.Utils
  ( recode
  , normalize
  , improve
  , buildTrie
  , pack
  , promote
  , lookupG
  -- Packed value
  , Packed(..)
  , packedValue
  , breakBySize
  ) where

import           Data.TrieMap (T(..))
import qualified Data.TrieMap as Trie

import           Control.Applicative
import           Data.List
import           Data.Either
import           Data.Maybe
import qualified Data.Set as Set
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

promote :: T a b -> T a (Either b b)
promote (T v@Just{} m) = T (fmap Right v) (Map.map promote m)
promote (T Nothing m)  = T (fmap Left closest) m'
  where
    m' = Map.map promote m
    closest = fmap (either id id) $ Map.foldl (\mx (T l _) -> mx <|> l) Nothing m'

-- | Break packed chunk so they contain no more then N bytes
breakBySize :: Int -> T a (Packed a b) -> T a (Packed a b)
breakBySize i (T Nothing m)       = T Nothing        $ Map.map (breakBySize i) m
breakBySize i (T (Just (NonPacked b)) m) = T (Just (NonPacked b)) $ Map.map (breakBySize i) m
breakBySize i (T (Just (Packed l mb)) m)  
   | length l < i                 = T (Just $ Packed l mb)  $ Map.map (breakBySize i) m
   | otherwise                    = T (Just $ Packed hs mb) $ Map.singleton t $ breakBySize i
                                                            $ T (Just $ Packed ts mb) m
   where (hs,t:ts) = splitAt (i-1) l

improve :: (Eq b) => T a b -> T a b
improve (T Nothing m) = T k m'
  where m' = Map.map improve m
        k  | Map.size m' == 1 = (\(T v _) -> v) $ head $ Map.elems m'
           | otherwise        = Nothing
improve (T v m)  = T v m

lookupG :: forall a b . Ord a => (T a b) -> [a] -> (Maybe b, Bool, Int)
lookupG z p = go 0 (promote z) p
  where go c (T v _) []
            = (fmap (either id id) v, maybe False isRight v, c)
        go c (T _ m) ((\x -> x `Map.lookup` m -> Just t) :xs)
            = go (c+1) t xs
        go c (T v _) _
            = (fmap (either id id) v, False, c)
