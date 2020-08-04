{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving       #-}


module Simplescript.Poly.Infer where

import Simplescript.Poly.Env as Env
import Simplescript.Poly.Type
import Simplescript.Poly.Expr

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity

import Data.List (nub)
import qualified Data.Map as Map
import qualified Data.Set as Set


-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

-- | Inference monad
type Infer a = 
  ( ReaderT
      Env             -- Typing environment
      ( StateT         -- Inference state
          InferState
          (Except TypeError)
      )
      a
  )              -- Result

-- | Inference state
newtype InferState = InferState { count :: Int }

-- | Initial inference state
initInfer :: InferState
initInfer = InferState { count = 0 }

type TypeEquation = (Type, Type)

type Unifier = (SubstMap, [TypeEquation])

-- | TypeEquation solver monad
type Solve a = ExceptT TypeError Identity a

newtype SubstMap = SubstMap (Map.Map TVar Type)
  deriving (Eq, Ord, Show, Semigroup, Monoid)

class Substitutable a where
  substitute :: SubstMap -> a -> a
  getFreeTypeVars   :: a -> Set.Set TVar

instance Substitutable Type where
  substitute _ (TCtr a)       = TCtr a
  substitute (SubstMap s) t@(TVar a) = Map.findWithDefault t a s
  substitute s (t1 `TArrow` t2) = substitute s t1 `TArrow` substitute s t2

  getFreeTypeVars TCtr{}         = Set.empty
  getFreeTypeVars (TVar a)       = Set.singleton a
  getFreeTypeVars (t1 `TArrow` t2) = getFreeTypeVars t1 `Set.union` getFreeTypeVars t2

instance Substitutable Forall where
  substitute (SubstMap s) (Forall as t)
      = Forall as $ substitute s' t
          where 
              s' = SubstMap $ foldr Map.delete s as

  getFreeTypeVars (Forall as t) = getFreeTypeVars t `Set.difference` Set.fromList as

instance Substitutable TypeEquation where
   substitute s (t1, t2) = (substitute s t1, substitute s t2)
   getFreeTypeVars (t1, t2) = getFreeTypeVars t1 `Set.union` getFreeTypeVars t2

instance Substitutable a => Substitutable [a] where
  substitute = map . substitute
  getFreeTypeVars   = foldr (Set.union . getFreeTypeVars) Set.empty

instance Substitutable Env where
  substitute s (TypeEnv env) = TypeEnv $ Map.map (substitute s) env
  getFreeTypeVars (TypeEnv env) = getFreeTypeVars $ Map.elems env

data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable String
  | Ambigious [TypeEquation]
  | UnificationMismatch [Type] [Type]

-------------------------------------------------------------------------------
-- Inference
-------------------------------------------------------------------------------

-- | Run the inference monad
runInfer :: Env -> Infer (Type, [TypeEquation]) -> Either TypeError (Type, [TypeEquation])
runInfer env m = runExcept $ evalStateT (runReaderT m env) initInfer

-- | Solve for the toplevel type of an expression in a given environment
inferExpr :: Env -> Expr -> Either TypeError Forall
inferExpr env ex = case runInfer env (infer ex) of
  Left err -> Left err
  Right (ty, cs) -> case runSolve cs of
    Left err -> Left err
    Right subst -> Right $ closeOver $ substitute subst ty

-- | Return the internal constraints used in solving for the type of an expression
constraintsExpr :: Env -> Expr -> Either TypeError ([TypeEquation], SubstMap, Type, Forall)
constraintsExpr env ex = case runInfer env (infer ex) of
  Left err -> Left err
  Right (ty, cs) -> case runSolve cs of
    Left err -> Left err
    Right subst -> Right (cs, subst, ty, sc)
      where
        sc = closeOver $ substitute subst ty

-- | Canonicalize and return the polymorphic toplevel type.
closeOver :: Type -> Forall
closeOver = normalize . generalize Env.empty

-- | Extend type environment
inEnv :: (Name, Forall) -> Infer a -> Infer a
inEnv (x, sc) m = do
  let scope e = remove e x `extend` (x, sc)
  local scope m

-- | Lookup type in the environment
lookupEnv :: Name -> Infer Type
lookupEnv x = do
  (TypeEnv env) <- ask
  case Map.lookup x env of
      Nothing   ->  throwError $ UnboundVariable x
      Just s    -> instantiate s

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer Type
fresh = do
    s <- get
    put s {count = count s + 1}
    return $ TVar $ TV (letters !! count s)

instantiate ::  Forall -> Infer Type
instantiate (Forall as t) = do
    as' <- mapM (const fresh) as
    let s = SubstMap $ Map.fromList $ zip as as'
    return $ substitute s t

generalize :: Env -> Type -> Forall
generalize env t  = Forall as t
    where as = Set.toList $ getFreeTypeVars t `Set.difference` getFreeTypeVars env

ops :: Binop -> Type
ops Add = typeInt `TArrow` (typeInt `TArrow` typeInt)
ops Mul = typeInt `TArrow` (typeInt `TArrow` typeInt)
ops Sub = typeInt `TArrow` (typeInt `TArrow` typeInt)
ops Eql = typeInt `TArrow` (typeInt `TArrow` typeBool)

infer :: Expr -> Infer (Type, [TypeEquation])
infer expr = case expr of
  Lit (LInt _)  -> return (typeInt, [])
  Lit (LBool _) -> return (typeBool, [])

  Var x -> do
      t <- lookupEnv x
      return (t, [])

  Lam x e -> do
    tv <- fresh
    (t, c) <- inEnv (x, Forall [] tv) (infer e)
    return (tv `TArrow` t, c)

  App e1 e2 -> do
    (t1, c1, t2, c2, tv) <- infer2Exprs e1 e2
    return (tv, c1 ++ c2 ++ [(t1, t2 `TArrow` tv)])

  Let x e1 e2 -> do
    env <- ask
    (t1, c1) <- infer e1
    case runSolve c1 of
        Left err -> throwError err
        Right sub -> do
            let sc = generalize (substitute sub env) (substitute sub t1)
            (t2, c2) <- inEnv (x, sc) $ local (substitute sub) (infer e2)
            return (t2, c1 ++ c2)

  Fix e1 -> do
    (t1, c1) <- infer e1
    tv <- fresh
    return (tv, c1 ++ [(tv `TArrow` tv, t1)])

  Op op e1 e2 -> do
    (t1, c1, t2, c2, tv) <- infer2Exprs e1 e2
    let u1 = t1 `TArrow` (t2 `TArrow` tv)
        u2 = ops op
    return (tv, c1 ++ c2 ++ [(u1, u2)])

  If cond tr fl -> do
    (t1, c1) <- infer cond
    (t2, c2) <- infer tr
    (t3, c3) <- infer fl
    return (t2, c1 ++ c2 ++ c3 ++ [(t1, typeBool), (t2, t3)])

  where 
    infer2Exprs e1 e2 = do 
      (t1, c1) <- infer e1
      (t2, c2) <- infer e2
      tv <- fresh
      return (t1, c1, t2, c2, tv)

inferTop :: Env -> [(String, Expr)] -> Either TypeError Env
inferTop env [] = Right env
inferTop env ((name, ex):xs) = case inferExpr env ex of
  Left err -> Left err
  Right ty -> inferTop (extend env (name, ty)) xs

normalize :: Forall -> Forall
normalize (Forall _ body) = Forall (map snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) (map TV letters)

    fv (TVar a)   = [a]
    fv (TArrow a b) = fv a ++ fv b
    fv (TCtr _)    = []

    normtype (TArrow a b) = TArrow (normtype a) (normtype b)
    normtype (TCtr a)   = TCtr a
    normtype (TVar a)   =
      case Prelude.lookup a ord of
        Just x -> TVar x
        Nothing -> error "type variable not in signature"


-------------------------------------------------------------------------------
-- TypeEquation Solver
-------------------------------------------------------------------------------

-- | The empty substitution
emptySubstMap :: SubstMap
emptySubstMap = mempty

-- | Compose substitutions
compose :: SubstMap -> SubstMap -> SubstMap
(SubstMap s1) `compose` (SubstMap s2) = SubstMap $ Map.map (substitute (SubstMap s1)) s2 `Map.union` s1

-- | Run the constraint solver
runSolve :: [TypeEquation] -> Either TypeError SubstMap
runSolve cs = runIdentity $ runExceptT $ solver st
  where st = (emptySubstMap, cs)

unifyMany :: [Type] -> [Type] -> Solve SubstMap
unifyMany [] [] = return emptySubstMap
unifyMany (t1 : ts1) (t2 : ts2) =
  do su1 <- unifies t1 t2
     su2 <- unifyMany (substitute su1 ts1) (substitute su1 ts2)
     return (su2 `compose` su1)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

unifies :: Type -> Type -> Solve SubstMap
unifies t1 t2 | t1 == t2 = return emptySubstMap
unifies (TVar v) t = v `bind` t
unifies t (TVar v) = v `bind` t
unifies (TArrow t1 t2) (TArrow t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2 = throwError $ UnificationFail t1 t2

-- Unification solver
solver :: Unifier -> Solve SubstMap
solver (su, cs) =
  case cs of
    [] -> return su
    ((t1, t2): cs0) -> do
      su1  <- unifies t1 t2
      solver (su1 `compose` su, substitute su1 cs0)

bind ::  TVar -> Type -> Solve SubstMap
bind a t | t == TVar a     = return emptySubstMap
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise       = return (SubstMap $ Map.singleton a t)

occursCheck ::  Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` getFreeTypeVars t
