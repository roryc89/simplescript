module Simplescript.TypeCheck where 

import Control.Monad.State.Lazy (State)
import Control.Monad.State.Lazy as State
import Data.Maybe (catMaybes)
import Data.IORef (IORef(..), newIORef, readIORef)
import Data.Text (Text)
import Data.Bifunctor (first)
import qualified Data.Map as Map
import qualified Simplescript.Ast as Ast
import qualified Simplescript.Parse as Parse
import Simplescript.TypeChecked
import Simplescript.Error (TypeCheckError(..), SimpleError(..))

-- type CheckError = TypeCheckError Ast.Positions

-- typeCheckStatements :: 
--     [Ast.StatementPos] -> 
--     Either [CheckError] [TopAssignment]
-- typeCheckStatements = traverse typeCheckStatement

-- typeCheckStatement :: 
--     Ast.StatementPos -> 
--     Either [CheckError] TopAssignment
-- typeCheckStatement st = case st of  
--     Ast.TypeAnnotation pos name t -> undefined
--     Ast.VarDeclaration pos name expr -> undefined
--     Ast.TypeDeclaration pos name args ctrs -> undefined 

-- -- GET TYPES 

-- getTypes :: [Ast.StatementPos] -> Either [CheckError] [Type]
-- getTypes = catTraverse getType

-- getType :: Ast.StatementPos -> Either [CheckError] (Maybe Type)
-- getType st = case st of 
--     Ast.TypeDeclaration pos name args ctrs -> 
--         pure undefined 
--     _ -> 
--         Right Nothing 

-- -- GET ANNOTATIONS 

-- getAnnotations ::  [Ast.StatementPos] -> Either [CheckError] [(Text, Type)]
-- getAnnotations = catTraverse getAnnotation 

-- getAnnotation :: Ast.StatementPos -> Either [CheckError] (Maybe (Text, Type))
-- getAnnotation st = case st of 
--     Ast.TypeAnnotation pos name tipe -> do 
--         checked <- checkType tipe
--         Right $ Just (name, checked)
--     _ -> 
--         Right Nothing

-- -- GET ANNOTATIONS 

-- typeCheckVarDeclarations :: Map.Map Text Type -> [Ast.StatementPos] -> Either [CheckError] [Expr]
-- typeCheckVarDeclarations types = catTraverse (typeCheckVarDeclaration types) 

-- typeCheckVarDeclaration :: Map.Map Text Type -> Ast.StatementPos -> Either [CheckError] (Maybe Expr)
-- typeCheckVarDeclaration annotations st = case st of 
--     Ast.VarDeclaration pos name expr -> do 
--         checked <- checkExpr annotations expr
--         Right $ Just checked
--     _ ->
--         Right Nothing

-- -- CHECK EXPR

-- checkExpr :: Map.Map Text Type -> Ast.ExprPos -> Either [CheckError] Expr
-- checkExpr annotations e = case e of 
--     Ast.Var a name typeMay -> 
--         case (Map.lookup name annotations, typeMay) of
--             (Just annotation, Nothing) -> undefined
--             (Just annotation, Just tipe) -> undefined
--     Ast.Apply fn arg -> undefined
--     Ast.Op a op exprL exprR -> undefined
--     Ast.Parens a expr typeMay -> undefined
--     Ast.Let a statments expr -> undefined
--     Ast.If a pred then_ else_ -> undefined
--     Ast.Case a expr branches typeMay -> undefined
--     Ast.Lit lit typeMay -> undefined


-- checkLit :: Map.Map Text Type -> Ast.Literal -> Either [CheckError] Val
-- checkLit annotations lit = case lit of 
--     Ast.IntLit a Int
--     Ast.NumberLit a Double
--     Ast.StringLit a Text 
--     Ast.CharLit a Char 
--     Ast.ListLit a [Expr a]
--     Ast.RecordLit a [(Text, Expr a
--     Ast.FunctionLit a [Destructured a] (Expr a



-- -- CHECK TYPE 

-- checkType :: Ast.TypePos -> Either [CheckError] Type 
-- checkType tipe = case tipe of 
--     Ast.TypeIdentifier a name -> Right $ Constant name []
--     Ast.TypeApply fn arg -> do 
--         fn_ <- Type <$> checkType fn 
--         arg_ <- Type <$> checkType arg 
--         Right $ Function fn_ arg_
--     Ast.TypeOp a name tipeL tipeR -> undefined
--     Ast.TypeParens a tipe -> checkType tipe
--     Ast.TypeLit lit -> undefined

-- -- UTILS 

-- catTraverse :: Applicative f => (a -> f (Maybe b)) -> [a] -> f [b]
-- catTraverse f = fmap catMaybes . traverse f