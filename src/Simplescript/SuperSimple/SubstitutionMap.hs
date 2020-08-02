module Simplescript.SuperSimple.SubstitutionMap where

import Data.Map
import Simplescript.Infer.UsedTypes
import Simplescript.SuperSimple.Type

data SubstitutionMap
    = SubstitutionMap (Map Int Type)

emptySubMap :: SubstitutionMap
emptySubMap = SubstitutionMap mempty