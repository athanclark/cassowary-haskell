module Linear.Grammar.Types.Utilities where

import qualified Data.Map as Map

-- | Delete any @0@ entries, and union the mappings with addition.
addMap :: (Ord k, Eq a, Num a) => Map.Map k a -> Map.Map k a -> Map.Map k a
addMap xs ys = Map.filter (/= 0) (Map.unionWith (+) xs ys)

subMap :: (Ord k, Eq a, Num a) => Map.Map k a -> Map.Map k a -> Map.Map k a
subMap xs ys = addMap xs (negate <$> ys)
