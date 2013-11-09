{-# LANGUAGE NoMonomorphismRestriction,
  TypeFamilies, FlexibleContexts, FlexibleInstances #-}

module Data.SetLike where

import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Semigroup

import Prelude hiding (foldl, foldr, foldl1, foldr1)
import GHC.Exts

type family Elem s :: *

type instance Elem [a] = a
type instance Elem (Set a) = a
type instance Elem (IntMap a) = a
type instance Elem (Map a b) = b

union :: Semigroup s => s -> s -> s
union = (<>)

empty :: Monoid s => s
empty = mempty

-- | Non-polymorphic Foldable, so types like ByteString or IntSet can be
-- its instances
class Foldable s where
  foldr :: (Elem s -> a -> a) -> a -> s -> a

foldl' :: Foldable s => (a -> Elem s -> a) -> a -> s -> a
foldl' f z0 xs = foldr f' id xs z0
  where f' x k z = k $! f z x

toList :: Foldable s => s -> [Elem s]
{-# INLINE toList #-}
toList t = build (\ c n -> foldr c n t)

mapM_ :: (Foldable s, Monad m) => (Elem s -> m a) -> s -> m ()
mapM_ f = foldr ((>>) . f) (return ())

mapReduceTo :: (Foldable s, Insertable a) => (Elem s -> Elem a) -> a -> s -> a
mapReduceTo f = foldr (insert . f)

mapReduce :: (Foldable s, Insertable a, Monoid a) => (Elem s -> Elem a) -> s -> a
mapReduce f = mapReduceTo f mempty

reduceTo :: (Elem a ~ Elem s, Foldable s, Insertable a) => a -> s -> a
reduceTo = mapReduceTo id

reduce :: (Elem a ~ Elem s, Foldable s, Insertable a, Monoid a) => s -> a
reduce = reduceTo mempty

-- * Insertable

class Insertable s where
  insert :: Elem s -> s -> s

insertMany :: (Elem c ~ Elem b, Foldable b, Insertable c) => b -> c -> c
insertMany = flip $ foldl' (flip insert)

fromList :: (Elem b ~ Elem c, Foldable b, Insertable c, Monoid c) => b -> c
fromList = (`insertMany` mempty)

-- * Filterable

class Filterable s where
  filter :: (Elem s -> Bool) -> s -> s
  -- TODO: partition

-- * Deletable

class Deletable s where
  delete :: Elem s -> s -> s

-- * Sized

class Sized s where
  size :: s -> Int

-- * SetLike

-- | Intersection of all previous classes
class (
  Monoid s,
  Foldable s, Insertable s, Deletable s, Filterable s, Sized s
  ) => SetLike s

-- * [] instances

instance Foldable [a] where
  foldr = List.foldr
instance Insertable [a] where
  insert = (:)
instance Filterable [a] where
  filter = List.filter
instance Eq a => Deletable [a] where
  delete = List.delete
instance Sized [a] where
  size = List.length
instance Eq a => SetLike [a]

-- * Set instances

instance Ord a => Foldable (Set a) where
  foldr = Set.fold
instance Ord a => Insertable (Set a) where
  insert = Set.insert
instance Ord a => Filterable (Set a) where
  filter = Set.filter
instance Ord a => Deletable (Set a) where
  delete = Set.delete
instance Sized (Set a) where
  size = Set.size
instance Ord a => SetLike (Set a)

-- * IntMap instances

instance Foldable (IntMap a) where
  foldr = IntMap.fold
instance Filterable (IntMap a) where
  filter = IntMap.filter
instance Sized (IntMap a) where
  size = IntMap.size

-- * Map instances

instance Ord a => Foldable (Map a b) where
  foldr = Map.fold
instance Ord k => Filterable (Map k a) where
  filter = Map.filter
instance Sized (Map a b) where
  size = Map.size
