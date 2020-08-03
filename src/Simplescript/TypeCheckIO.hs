{-# LANGUAGE RecordWildCards #-}
--  TODO : THIS Module can probably be deleted
module Simplescript.TypeCheckIO where 

import Control.Monad.State.Lazy (State)
import Control.Monad.State.Lazy as State
import Data.IORef (IORef(..), newIORef, readIORef)
import Data.Text (Text)
import Data.Bifunctor (first)
import qualified Data.Map as Map
import qualified Simplescript.Ast as Ast
import qualified Simplescript.Parse as Parse
import qualified Simplescript.TypeChecked as Checked
import Simplescript.Error (TypeCheckError(..), SimpleError(..))

type CheckError = TypeCheckError Ast.Positions

data Env = Env  
    { typesRef :: IORef 
        ( Map.Map Text TypeAssign )

    , annotationsRef :: IORef 
        ( Map.Map Text AnnotationAssign )
        
    , exprsRef :: IORef 
        ( Map.Map Text ExprAssign )
    }

nullEnv :: IO Env 
nullEnv = Env <$> emptyRef <*> emptyRef <*> emptyRef
    where
        emptyRef = newIORef Map.empty

type TypeAssign = 
    ( Ast.Positions, [ (Text, Ast.Positions) ], [Ast.CtrPos] )

type AnnotationAssign = 
    (Ast.Positions, Ast.TypePos)

type ExprAssign = 
    (Ast.Positions, Ast.ExprPos)

checkModuleText :: Text -> IO (Either (SimpleError Ast.Positions) [Checked.Expr])
checkModuleText input = case Parse.parseText input of
  Left err -> return $ Left $ ParseOrLexError err 
  Right ast -> first TypeCheckError <$> checkModule ast
    
checkModule :: [Ast.StatementPos] -> IO (Either [CheckError] [Checked.Expr])
checkModule sts = do 
    env <- nullEnv
    checkStatements env sts

checkStatements :: Env -> [Ast.StatementPos] -> IO (Either [CheckError] [Checked.Expr])
checkStatements env sts = do 
    results <- traverse (checkStatement env) sts 
    return $ sequence results

checkStatement :: Env -> Ast.StatementPos -> IO (Either [CheckError] Checked.Expr )
checkStatement env st = case st of 
    Ast.TypeAnnotation pos name t -> undefined
    Ast.VarDeclaration pos name expr -> undefined
    Ast.TypeDeclaration pos name args ctrs -> undefined 

addVarToEnv :: Env -> Ast.Positions -> Text -> Ast.ExprPos -> IO [CheckError]
addVarToEnv env@Env{..} pos name expr = do 
    -- exprs <- readIORef exprsRef
    -- case Map.lookup name exprs of 
    --     Just ()
    undefined  
    