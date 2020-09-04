{-# LANGUAGE
    KindSignatures
  #-}

module Linear.Grammar.Types.Class where

import Data.Set (Set)
import Data.Map (Map)

class Constraint (e :: * -> * -> * -> *) where
  getKeys :: e k a c -> [k]
  getKeysSet :: e k a c -> Set k
  getConst :: e k a c -> c
  getVars :: e k a c -> Map k a
  mapConst :: (c -> c') -> e k a c -> e k a c'
  mapAllVars :: (Map k a -> Map k' a') -> e k a c -> e k' a' c
  mapVars :: (a -> a') -> e k a c -> e k a' c
