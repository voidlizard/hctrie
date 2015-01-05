-- | Copyright: (c) 2015, Alexander Vershilov
--   Author:    Alexander Vershilov <alexander.vershilov@gmail.com>
--
-- This module provides a generic structure for Trie generation
-- and a set of utility functions.
--
-- TODO:
--
--   * allow overloaded lists
--
--   * use smth better then monoid for b, we just need mempty,
--     or rewrite API so monoid will be used in full generality.
--
--   * use Foldable typeclass to express most of the functions
module Data.TrieMap
  ( T(..)
  , singleton
  , insert
  , keys
  , values
  , first
  , second
  ) where

import           Data.List (foldl')
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set

-- | Trie like structure.
data T a b = T b (Map a (T a b)) deriving Show

instance Functor (T a) where fmap = second

insert :: (Ord a, Monoid b) => [a] -> b -> T a b -> T a b
insert [] v (T _ b) = T v b
insert (x:xs) v (T a bs) = T a (Map.alter go x bs)
  where go Nothing  = Just $ insert xs v (T mempty Map.empty)
        go (Just t) = Just $ insert xs v t

-- | Create an one element tree with the specified value in
-- the root node.
singleton :: b -> T a b
singleton v = T v Map.empty

-- | Return all map keys in ascending order.
keys :: Ord a => T a b -> Set a
keys (T _ m) = foldl' (\b a -> b `Set.union` a)
                          (Set.fromList (Map.keys m))
			  (Map.elems $ Map.map keys m)


first :: Ord b => (a -> b) -> T a c -> T b c
first f (T a m) = T a (Map.mapKeys f (Map.map (first f) m))

second :: (b -> c) -> T a b -> T a c
second f (T a m) = T (f a) (Map.map (second f) m)

values :: Ord b => T a b -> Set b
values (T v m) = Set.insert v $ Set.unions $ Map.elems $ Map.map values m