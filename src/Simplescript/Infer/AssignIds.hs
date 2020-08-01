module Simplescript.Infer.AssignIds (assignIds) where 

import Simplescript.Infer.Expr
import Simplescript.Infer.Type as Type
import Simplescript.Infer.UsedTypes

assignIds :: Int -> ExprU -> ( ExprTyped, Int )
assignIds currentId exprU = undefined
--     let
--         ( typedExpr, newId ) =
--             assignIdsHelp currentId exprU
--     in
--     {- Keep location, for error context -}
--     ( typedExpr
--     , newId
--     )

-- assignId :: Int -> Expr () -> ( ExprTyped, Int )
-- assignId currentId expr =
--     ( ( fmap (\_ -> (Type.Id currentId, ())) expr, _), currentId + 1 )

-- assignIdsHelp :: Int -> ExprAnd () -> ( ExprTyped, Int )
-- assignIdsHelp currentId exprU@(expr, _) =
--     foldr go ((fmap (\_ -> (Type.Id currentId, ())) expr, (Type.Id currentId, ())), currentId) exprU
--     where 
--         go :: ()
--          -> ((Expr (TypeExt Int, ()), (TypeExt Int, ())), Int)
--          -> ((Expr (TypeExt Int, ()), (TypeExt Int, ())), Int)
--         go _ ((expr, tipe), currentId) = undefined
    --   let
    --     f =
    --         assignIds
    -- in
    -- {- Be careful when dealing with the ids, they all have to be distinct.
    --    Enable the "unused variable" warning from elm-analyze may help you
    --    to detect created but unused ids.
    -- -}
    -- case fst exprU of
    --     {- With literals, we could plug their final type in right here
    --        (no solving needed!) but let's be uniform and do everything through
    --        the constraint solver in stages 2 and 3.
    --     -}
    --     Int int ->
    --         assignId currentId (Int int)

    --     Float float ->
    --         assignId currentId (Float float)

    --     Char char ->
    --         assignId currentId (Char char)

    --     String string ->
    --         assignId currentId (String string)

    --     Bool bool ->
    --         assignId currentId (Bool bool)

    --     -- We remember argument's IDs so that we can later use them in Lambda
    --     Argument name ->
    --         assignId currentId (Argument name)

    --     Var module_ name ->
    --         assignId currentId (Var module_ name)

    --     Lambda argument body ->
    --         let
    --             ( body_, id1 ) =
    --                 f currentId body
    --         in
    --         assignId id1
    --             (Lambda
    --                 argument
    --                 body_
    --             )

    --     Call fn argument ->
    --         let
    --             ( fn_, id1 ) =
    --                 f currentId fn

    --             ( argument_, id2 ) =
    --                 f id1 argument
    --         in
    --         assignId id2
    --             (Call
    --                 { fn = fn_
    --                 , argument = argument_
    --                 }
    --             )

    --     If test then_ else_  ->
    --         let
    --             ( test_, id1 ) =
    --                 f currentId test

    --             ( then__, id2 ) =
    --                 f id1 then_

    --             ( else__, id3 ) =
    --                 f id2 else_
    --         in
    --         assignId id3
    --             (If
    --                 { test = test_
    --                 , then_ = then__
    --                 , else_ = else__
    --                 }
    --             )

    --     Let bindings body -> undefined
    --         {- We don't thread the full (VarName, Binding) thing to Id
    --            as that would bloat the type signatures of IdGenerator too much.

    --            We unwrap the exprs from the bindings and then carefully put them
    --            back together in the same order.
    --         -}
    --         -- let
    --         --     bindingsList =
    --         --         Dict.toList bindings

    --         --     ( body_, id1 ) =
    --         --         f currentId body

    --         --     ( bindingBodiesList, id2 ) =
    --         --         List.foldl
    --         --             (\( name, binding ) ( acc, runningId ) ->
    --         --                 let
    --         --                     ( body__, nextId ) =
    --         --                         f runningId binding.body

    --         --                     newElt =
    --         --                         ( name, { name = name, body = body__ } )
    --         --                 in
    --         --                 ( newElt :: acc
    --         --                 , nextId
    --         --                 )
    --         --             )
    --         --             ( [], id1 )
    --         --             bindingsList
    --         -- in
    --         -- assignId id2
    --         --     (Let
    --         --         { bindings =
    --         --             Dict.fromList bindingBodiesList
    --         --         , body = body_
    --         --         }
    --         --     )

    --     Unit ->
    --         assignId currentId Unit

    --     -- List items ->
    --     --     let
    --     --         ( items_, newId ) =
    --     --             foldr
    --     --                 (\item ( acc, runningId ) ->
    --     --                     let
    --     --                         ( item_, nextId ) =
    --     --                             f runningId item
    --     --                     in
    --     --                     ( item_ :: acc
    --     --                     , nextId
    --     --                     )
    --     --                 )
    --     --                 ( [], currentId )
    --     --                 items
    --     --     in
    --     --     assignId newId (List items_)

    --     Record bindings -> undefined
    --         -- let
    --         --     bindingsList =
    --         --         Dict.toList bindings

    --         --     ( bindingBodiesList, newId ) =
    --         --         List.foldl
    --         --             (\( name, binding ) ( acc, runningId ) ->
    --         --                 let
    --         --                     ( body__, nextId ) =
    --         --                         f runningId binding.body

    --         --                     newElt =
    --         --                         ( name, { name = name, body = body__ } )
    --         --                 in
    --         --                 ( newElt :: acc
    --         --                 , nextId
    --         --                 )
    --         --             )
    --         --             ( [], currentId )
    --         --             bindingsList
    --         -- in
    --         -- assignId newId <|
    --         --     Record (Dict.fromList bindingBodiesList)

    --     Case e branches -> undefined
    --         -- let
    --         --     ( e_, id1 ) =
    --         --         f currentId e

    --         --     ( branches_, newId ) =
    --         --         List.foldr
    --         --             (\{ pattern, body } ( acc, runningId ) ->
    --         --                 let
    --         --                     ( typedPattern, bodyId ) =
    --         --                         assignPatternIds runningId pattern

    --         --                     ( typedBody, nextId ) =
    --         --                         f bodyId body
    --         --                 in
    --         --                 ( { pattern = typedPattern
    --         --                   , body = typedBody
    --         --                   }
    --         --                     :: acc
    --         --                 , nextId
    --         --                 )
    --         --             )
    --         --             ( [], id1 )
    --         --             branches
    --         -- in
    --         -- assignId newId <|
    --         --     Case e_ branches_