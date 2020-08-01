{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE DeriveFoldable  #-}

module Simplescript.Infer.Expr where 

import Data.Map
import Data.Text
import Simplescript.Infer.Pattern 
import Simplescript.Infer.Type 
import Simplescript.Infer.UsedTypes

type ExprTyped = ExprAnd TypeOrId

type ExprU = ExprAnd ()

type ExprId = ExprAnd Int

type ExprAnd a = (Expr a, a)

data Expr a
    = Int Int
    | Float Float
    | Char Char
    | String Text
    | Bool Bool
    | Var 
      { module_ :: ModuleName
      , name :: VarName 
      }
    | Argument VarName
    | Lambda 
      { lambdaArg :: VarName
      , lambdaBody :: ExprAnd a 
      }
    | Call 
      { fn :: ExprAnd a
      , argument :: ExprAnd a 
      }
    | If 
      { test :: ExprAnd a
      , then_ :: ExprAnd a
      , else_ :: ExprAnd a 
      }
    | Let 
      { bindings :: Map VarName (Binding (ExprAnd a))
      , body :: ExprAnd a 
      }
    | Unit
    | Record (Map VarName (Binding (ExprAnd a)))
    | Case 
        (ExprAnd a)
            [ (Pattern, ExprAnd a)
            ]
    deriving(Eq, Ord, Show, Foldable, Functor)
    
