{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE GADTs, StandaloneDeriving, ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE ConstraintKinds #-}

-- | Data.Key from "keys" package is so nice, but it needs more methods to work
-- with map-like structures.
module Data.MapLike (
  module Data.MapLike,
  module Data.SetLike
  )where

import Data.SetLike (
  empty, union, mapReduce, foldl', mapM_,
  Elem,
  Foldable(..), Insertable(), Filterable(..), Deletable(), Sized(..),
  )
import qualified Data.SetLike as SetLike

import Prelude (
  ($), (.), Show(..), Ord(..),
  curry, uncurry, fst, const,
  Int, String, Maybe(..),
  )
import qualified Prelude

import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Semigroup
import Data.Monoid
-- | Similar to 'Data.Key.Key' type family from "keys" package. The difference
-- is that type argument to Key is of kind @* -> *@, and for Key it's @*@. So
-- you'll have
-- > type instance Data.Key.Key IntMap = Int
-- > type instance Data.MapLike.Key (IntMap a) = Int
type family Key map :: *

-- | Generalizing 'lookup' function
class Lookup m where
  lookup :: Key m -> m -> Maybe (Elem m)

-- | Generalizing 'index' function
class Lookup m => Indexable m where
  (!) :: m -> Key m -> Elem m

-- | View of a map as a set of (key, value) pairs
newtype Assocs map = Assocs { unAssocs :: map }
type instance Elem (Assocs map) = (Key map, Elem map)
instance Sized map => Sized (Assocs map) where size = size . unAssocs

-- | View of a map as a set of keys, ignoring values
newtype Keys map = Keys { unKeys :: map }
type instance Elem (Keys map) = Key map
instance SetLike.Foldable (Assocs map) => SetLike.Foldable (Keys map) where
  foldr f x = foldr (f . fst) x . Assocs . unKeys
instance Sized map => Sized (Keys map) where size = size . unKeys

-- * IntMap instances
type instance Key (IntMap a) = IntMap.Key
instance Foldable (Assocs (IntMap a)) where
  foldr f x = IntMap.foldWithKey (curry f) x . unAssocs
instance Insertable (Assocs (IntMap a)) where
  insert (k, v) = Assocs . IntMap.insert k v . unAssocs
instance Filterable (Assocs (IntMap a)) where
  filter f = Assocs . IntMap.filterWithKey (curry f) . unAssocs
instance Deletable (Keys (IntMap a)) where
  delete key = Keys . IntMap.delete key . unKeys
instance Lookup (IntMap a) where
  lookup = IntMap.lookup
instance Indexable (IntMap a) where
  (!) = (IntMap.!)

-- * Data.Map instances
type instance Key (Map k a) = k
instance Ord k => Foldable (Assocs (Map k a)) where
  foldr f x = Map.foldrWithKey (curry f) x . unAssocs
instance Ord k => Insertable (Assocs (Map k a)) where
  insert (k, v) = Assocs . Map.insert k v . unAssocs
instance Ord k => Filterable (Assocs (Map k a)) where
  filter f = Assocs . Map.filterWithKey (curry f) . unAssocs
instance Ord k => Deletable (Keys (Map k a)) where
  delete key = Keys . Map.delete key . unKeys
instance Ord k => Lookup (Map k a) where
  lookup = Map.lookup
instance Ord k => Indexable (Map k a) where
  (!) = (Map.!)

-- type MapLike m = (
class (

  Semigroup m,
  Foldable m, Foldable (Assocs m), Foldable (Keys m),
  Insertable (Assocs m),
  Filterable m, Filterable (Assocs m),
  Deletable (Keys m),
  Lookup m,
  Indexable m,
  Monoid m
  -- )
  ) => MapLike m

instance MapLike (IntMap a)
instance Ord k => MapLike (Map k a)

-- -- Functions to work with keys only.

-- mapKeys :: Keyed f => (Key f -> b) -> f a -> f b
-- mapKeys f = mapWithKey (const . f)

-- keys :: Keyed f => f a -> f (Key f)
-- keys = mapWithKey const

insert :: MapLike m => Key m -> Elem m -> m -> m
insert k v = unAssocs . SetLike.insert (k, v) . Assocs

insertMany :: (MapLike m, Foldable assocs, Elem assocs ~ (Key m, Elem m)) =>
  assocs -> m -> m
insertMany assocs = unAssocs . SetLike.insertMany assocs . Assocs

-- fromList :: (MapLike m, Foldable assocs, Elem assocs ~ (Key m, Elem m)) =>
fromList :: (MapLike m, Monoid m) => [(Key m, Elem m)] -> m
fromList = (`insertMany` empty)

toList :: MapLike m => m -> [(Key m, Elem m)]
toList = SetLike.toList . Assocs

mapReduceWithKey f = mapReduce (uncurry f) . Assocs
mapReduceKeys f = mapReduce (const . f) . Assocs

-- | Insert value with key equal to size of the map, return the key and new map.
insertValue v map = let k = size map in (k, insert k v map)

-- | Insert same value at many positions
-- insertSameValue :: (Insertable f, Foldable g) => g (Key f) -> a -> f a -> f a
insertSameValue ks v map = foldl' (\map k -> insert k v map) map ks

delete :: MapLike m => Key m -> m -> m
delete k = unKeys . SetLike.delete k . Keys

-- * Efficient map

-- | Type family to choose optimal map type for given key type
type family EfficientMap key :: * -> *

type instance EfficientMap Int = IntMap
type instance EfficientMap String = Map String

-- synonym to all constraints for EfficientMap
-- type EfMap k a = (
--   MapLike (EfficientMap k a), Key (EfficientMap k a) ~ k, Elem (EfficientMap k a) ~ a,
--   Show (EfficientMap k a)
--   )

-- GADT wrapper of EfficientMap. Needs better name.
data EM k a where
  EM :: (m ~ EfficientMap k a, Key m ~ k, Elem m ~ a, MapLike m) =>
    EfficientMap k a -> EM k a

type instance Key (EM k a) = k
type instance Elem (EM k a) = a

deriving instance (Show (EfficientMap k a)) => Show (EM k a)
instance Semigroup (EM k a) where
  EM m1 <> EM m2 = EM $ m1 <> m2

-- e :: (EfMap k a) => EM k a
-- e = EM mempty -- (mempty :: EfficientMap k a)

instance Foldable (EM k a) where
  foldr f x (EM m) = foldr f x m
instance Foldable (Assocs (EM k a)) where
  foldr f x (Assocs (EM m)) = foldr f x (Assocs m)
instance Insertable (Assocs (EM k a)) where
  insert (k, v) (Assocs (EM m)) = Assocs . EM $ insert k v m
instance Filterable (EM k a) where
  filter f (EM m) = EM $ filter f m
instance Filterable (Assocs (EM k a)) where
  filter f (Assocs (EM m)) = Assocs . EM . unAssocs $ filter f $ Assocs m
instance Deletable (Keys (EM k a)) where
  delete k (Keys (EM m)) = Keys . EM $ delete k m
instance Lookup (EM k a) where
  lookup k (EM m) = lookup k m
instance Indexable (EM k a) where
  EM m ! k = m ! k
instance Monoid (EM k a) where
  mempty = fromList []
  EM m1 `mappend` EM m2 = EM $ m1 `mappend` m2

instance MapLike (EM k a)
