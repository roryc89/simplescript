module Simplescript.Infer.SubstitutionMap where

import Data.Map
import Simplescript.Infer.UsedTypes
import Simplescript.Infer.Type

data SubstitutionMap
    = SubstitutionMap (Map Int TypeOrId)
