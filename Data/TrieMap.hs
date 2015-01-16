-- |
-- Copyright: (c) 2015, Alexander Vershilov
-- Portability: unportable (ghc extras)
--
--
-- This module provides a generic structure for Trie generation
-- and a set of utility functions.
--
{-# LANGUAGE FlexibleContexts #-}
module Data.TrieMap
  ( T(..)
  -- * Construction
  , empty
  , singleton
  -- * Modification
  , insert
  -- * Queries
  , keys
  , values
  -- * Instance like methods
  -- ** Bifunctor
  , first
  , second
  -- * Utilities
  , depth
  , fullKeys
  ) where

import           Data.List (foldl', nub)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import GHC.Exts

-- | Trie like structure.
data T a b = T (Maybe b) (Map a (T a b)) deriving Show

instance Functor (T a) where fmap = second

insert :: (Ord (Item a), IsList a) => a -> b -> T (Item a) b -> T (Item a) b
insert l = inner (toList l)
  where 
    inner []     v (T _ b)  = T (Just v) b
    inner (x:xs) v (T a bs) = T a (Map.alter go x bs)
      where go Nothing  = Just $ insert xs v (T Nothing Map.empty)
            go (Just t) = Just $ insert xs v t

-- | Create empty Trie
empty :: T a b
empty = T Nothing Map.empty

-- | Create an one element tree with the specified value in
-- the root node.
singleton :: b -> T a b
singleton v = T (Just v) Map.empty

-- | Return all map keys in ascending order.
keys :: Ord a => T a b -> Set a
keys (T _ m) = foldl'  Set.union
                      (Set.fromList (Map.keys m))
                      (Map.elems $ Map.map keys m)

first :: Ord b => (a -> b) -> T a c -> T b c
first f (T a m) = T a (Map.mapKeys f (Map.map (first f) m))

second :: (b -> c) -> T a b -> T a c
second f (T a m) = T (fmap f a) (Map.map (second f) m)

values :: Ord b => T a b -> Set b
values (T v m) = maybe id Set.insert v $ Set.unions $ Map.elems $ Map.map values m

depth :: Ord b => T a b -> Int
depth (T _ m)
   | Map.null m = 0
   | otherwise  = succ $ maximum $ Map.elems $ Map.map depth m

fullKeys :: Ord a => T a b -> [[a]]
fullKeys (T _ m) = nub $ concat $ Map.elems $ Map.mapWithKey (\k v -> [k]:map (k:) (fullKeys v)) m
