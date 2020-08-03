module Simplescript.SuperSimple.SubstitutionMap where

import Data.Map (Map)
import qualified Data.Map as Map
import Simplescript.Infer.UsedTypes
import Simplescript.SuperSimple.Type

newtype SubstitutionMap
    = SubstitutionMap (Map Int Type)

empty :: SubstitutionMap
empty = SubstitutionMap mempty

insert :: Int -> Type -> SubstitutionMap -> SubstitutionMap
insert id t (SubstitutionMap m) = SubstitutionMap $ Map.insert id t m