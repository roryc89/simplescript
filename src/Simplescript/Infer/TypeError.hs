module Simplescript.Infer.TypeError where 

import Simplescript.Infer.UsedTypes
import Simplescript.Infer.Type 

{-| Errors encountered during [typechecking](Elm.Compiler#inferExpr).
-}
data TypeError
    = TypeMismatch TypeOrId TypeOrId
    | OccursCheckFailed Int TypeOrId