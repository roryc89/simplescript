module Simplescript.Infer.Pattern where 

import Simplescript.Infer.UsedTypes
import Data.Text
import Simplescript.Infer.Type 
    
data Pattern
    = PAnything
    | PVar VarName
    | PRecord [VarName]
    | PAlias Pattern VarName
    | PUnit
    | PTuple Pattern Pattern
    | PList [Pattern]
    | PCons Pattern Pattern
    | PBool Bool
    | PChar Char
    | PString Text
    | PInt Int
    | PFloat Float
    deriving(Eq, Ord, Show)
