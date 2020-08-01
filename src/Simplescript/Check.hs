{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveFoldable       #-}

module Simplescript.Check where 

import Data.Bifunctor (first)
import Data.Functor (void)
import Data.Text (Text)
import qualified Data.Text as T


data Expr a
    = Number Double a
    | Int Int a
    | String Text a
    | Function Text a (Expr a) a
    | Apply (Expr a) (Expr a) a
    | Parens (Expr a) a
    deriving(Show, Eq, Functor, Foldable)

type ExprUnchecked = Expr (Maybe Type)

type ExprChecked = Expr Type

exprType :: Expr Type -> Type 
exprType e = case e of
    Number val t -> t
    Int val t -> t
    String val t -> t
    Function fn fnT arg t -> t
    Apply fn arg t -> t
    Parens val t -> t

exprWoType :: Expr Type -> Type -> Expr Type
exprWoType e = case e of
    Number val t -> Number val 
    Int val t -> Int val 
    String val t -> String val 
    Function fn fnT arg t -> Function fn fnT arg 
    Apply fn arg t -> Apply fn arg 
    Parens val t -> Parens val 

data Type_ a
    = NumberT
    | IntT
    | StringT 
    | FunctionT (Type_ a) (Type_ a)
    deriving(Show, Eq, Functor, Foldable)

type Type = Type_ ()

data TypeError_ a
    = Mismatch (Type_ a) (Type_ a)
    | IsNotAFunction (Expr a)
    deriving(Show, Eq)

type TypeError = TypeError_ ()

check :: ExprUnchecked -> Either [TypeError] ExprChecked
check e = case e of 
    Number val t -> typesMatches (Number val) NumberT t
    Int val t -> typesMatches (Int val) IntT t
    String val t -> typesMatches (String val) StringT t
    Function fn fnT arg t -> undefined
    Apply fn arg t -> case fn of 
        Function fn fnT arg t -> undefined 
        _ -> Left [IsNotAFunction $ void fn]
    Parens val t -> do 
        checked <- check val
        typesMatches (exprWoType checked) (exprType checked) t


typesMatches :: (Type -> ExprChecked) -> Type -> Maybe Type -> Either [TypeError] ExprChecked
typesMatches expr t = first pure . typesMatch expr t

typesMatch :: (Type -> ExprChecked) -> Type -> Maybe Type -> Either TypeError ExprChecked
typesMatch expr t = 
    maybe (Right $ expr t) 
        (\t2 -> 
            if t /= t2 then
                Left $ Mismatch t t2 
            else 
                Right $ expr t
        )
         

