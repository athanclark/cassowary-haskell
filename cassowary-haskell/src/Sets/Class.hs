module Sets.Class where

import qualified Data.Map as Map
import qualified Data.Set as Set


class HasUnion a where
  union :: a -> a -> a

instance ( Ord k
         ) => HasUnion (Map.Map k a) where
  union = Map.union

instance ( Ord a
         ) => HasUnion (Set.Set a) where
  union = Set.union

class HasIntersection a where
  intersection :: a -> a -> a

instance ( Ord k
         ) => HasIntersection (Map.Map k a) where
  intersection = Map.intersection

instance ( Ord a
         ) => HasIntersection (Set.Set a) where
  intersection = Set.intersection

class HasDifference a where
  difference :: a -> a -> a

instance ( Ord k
         ) => HasDifference (Map.Map k a) where
  difference = Map.difference

instance ( Ord a
         ) => HasDifference (Set.Set a) where
  difference = Set.difference
