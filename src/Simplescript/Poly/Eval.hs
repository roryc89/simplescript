
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Simplescript.Poly.Eval where

import Simplescript.Poly.Expr

import Control.Monad.Identity
import qualified Data.Map as Map

data Value
  = VInt Integer
  | VBool Bool
  | VClosure String Expr TermEnv

type TermEnv = Map.Map String Value

newtype Interpreter t = Interpreter (Either String t)
    deriving(Functor, Applicative, Monad)

instance MonadFail Interpreter where
  fail s = Interpreter (Left s)


emptyTmenv :: TermEnv
emptyTmenv = Map.empty

instance Show Value where
  show (VInt n) = show n
  show (VBool n) = show n
  show VClosure{} = "<<closure>>"

eval :: TermEnv -> Expr -> Interpreter Value
eval env expr = case expr of
  Lit (LInt k)  -> return $ VInt k
  Lit (LBool k) -> return $ VBool k

  Var x -> do
    let Just v = Map.lookup x env
    return v

  Op op a b -> do
    VInt a' <- eval env a
    VInt b' <- eval env b
    return $ binop op a' b'

  Lam x body ->
    return (VClosure x body env)

  App fun arg -> do
    VClosure x body clo <- eval env fun
    argv <- eval env arg
    let nenv = Map.insert x argv clo
    eval nenv body

  Let x e body -> do
    e' <- eval env e
    let nenv = Map.insert x e' env
    eval nenv body

  If cond tr fl -> do
    VBool br <- eval env cond
    if br
    then eval env tr
    else eval env fl

  Fix e -> do
    eval env (App e (Fix e))

binop :: Binop -> Integer -> Integer -> Value
binop Add a b = VInt $ a + b
binop Mul a b = VInt $ a * b
binop Sub a b = VInt $ a - b
binop Eql a b = VBool $ a == b

runEval :: TermEnv -> String -> Expr -> (Value, TermEnv)
runEval env nm ex =
  case eval env ex of 
      Interpreter (Left err) -> error err
      Interpreter (Right res) -> (res, Map.insert nm res env)