module Simplescript.Infer where


import Data.Map (Map)
import Data.Text (Text)
import Data.List 
import qualified Simplescript.Infer.AssignIds as AssignIds
import Simplescript.Infer.Expr
import Simplescript.Infer.Type
import Simplescript.Infer.TypeError
import Simplescript.Infer.UsedTypes
import Simplescript.Infer.SubstitutionMap



-- inferExpr ::
--     Map ( ModuleName, VarName ) (ConcreteType Qualified)
--     -> Int
--     -> SubstitutionMap
--     -> ExprU
--     -> Either ( TypeError, SubstitutionMap ) ( ExprTyped, SubstitutionMap, Int )
-- inferExpr aliases unusedId substitutionMap located =
--     let
--         ( exprWithIds, unusedId1 ) =
--             AssignIds.assignIds unusedId located

--         ( typeEquations, unusedId2 ) =
--             GenerateEquations.generateEquations unusedId1 exprWithIds

--         {- We have an interesting dilemma:

--            Should we carry the substitution map around, keep the typed expr
--            consisting of type variable IDs and primitive types and substitute for
--            the inferred types at the last possible moment (when reporting errors)?

--            Or should we substitute all possible type variable IDs before
--            returning from this function, and ditch the substitution map?

--            The second option seems like an unnecessary work, but for the purposes
--            of readability and education we go with it.
--         -}
--         newSubstitutionMap :: Either ( TypeError, SubstitutionMap ) SubstitutionMap
--         newSubstitutionMap =
--             Unify.unifyAllEquations typeEquations aliases substitutionMap
--     in
--     newSubstitutionMap
--         <#> (\map ->
--                 ( substituteAllInExpr exprWithIds map
--                 , map
--                 , unusedId2
--                 )
--             )



       --  -- |> Either.mapError substituteAllInError

