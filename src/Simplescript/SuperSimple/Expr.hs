{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE DeriveFoldable  #-}

module Simplescript.SuperSimple.Expr where 

import Data.Map
import Data.Text
import Simplescript.Infer.Pattern 
import Simplescript.Infer.Type 
import Simplescript.Infer.UsedTypes

type Expr a = (Expr_ a, a)

data Expr_ a
    = Int Int
    | Bool Bool
    | Var VarName
    | Argument VarName
    | Lambda 
      { lambdaArg :: VarName
      , lambdaBody :: Expr a 
      }
    | Call 
      { fn :: Expr a
      , argument :: Expr a 
      }
    deriving(Eq, Ord, Show, Foldable, Functor)
    
