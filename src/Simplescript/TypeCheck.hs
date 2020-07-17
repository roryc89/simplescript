{-# LANGUAGE RecordWildCards #-}
module Simplescript.TypeCheck where 

import Control.Monad.State.Lazy (State)
import Control.Monad.State.Lazy as State
import Data.IORef (IORef(..), newIORef)
import Data.Text (Text)
import Data.Bifunctor (first)
import qualified Data.Map as Map
import qualified Simplescript.Ast as Ast
import qualified Simplescript.Parse as Parse
import qualified Simplescript.TypeChecked as Checked
import Simplescript.Error (TypeCheckError(..), SimpleError(..))

type CheckError = TypeCheckError Ast.Positions

data Env = Env  
    { types :: IORef 
        ( Map.Map Text TypeAssign )

    , annotations :: IORef 
        ( Map.Map Text AnnotationAssign )
        
    , exprs :: IORef 
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
checkStatement env@Env{..} st = undefined 
