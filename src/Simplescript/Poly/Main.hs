{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplescript.Poly.Main where 

import Simplescript.Poly.Expr
import Simplescript.Poly.Infer
import Simplescript.Poly.Parser
import Simplescript.Poly.Pretty
import Simplescript.Poly.Eval
import qualified Simplescript.Poly.Env as Env
import Debug.Trace

import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import Control.Monad.Identity
import Control.Monad.State.Strict

import Data.List (isPrefixOf, foldl')

import System.Exit
import System.Environment
import System.Console.Repline

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data IState = IState
  { tyctx :: Env.Env  -- Type environment
  , tmctx :: TermEnv  -- Value environment
  }

initState :: IState
initState = IState Env.empty emptyTmenv

type Repl a = HaskelineT (StateT IState IO) a
hoistErr :: Show e => Either e a -> Repl a
hoistErr (Right val) = return val
hoistErr (Left err) = do
  liftIO $ print err
  abort

-------------------------------------------------------------------------------
-- Execution
-------------------------------------------------------------------------------

evalDef :: TermEnv -> (String, Expr) -> TermEnv
evalDef env (nm, ex) = tmctx'
  where (val, tmctx') = runEval env nm ex

exec :: Bool -> Bool -> L.Text -> Repl ()
exec debug update source = do
  -- Get the current interpreter state
  st <- get

  -- Parser ( returns AST )
  mod <- hoistErr $ parseModule "<stdin>" source

  when debug $ do 
    liftIO $ putStrLn "AST"
    liftIO $ print mod
  
  -- Type Inference ( returns Typing Environment )
  tyctx' <- hoistErr $ inferTop (tyctx st) mod

  when debug $ do 
    liftIO $ putStrLn "type inference"
    liftIO $ print tyctx'
    
  -- Create the new environment
  let st' = st { tmctx = foldl' evalDef (tmctx st) mod
               , tyctx = tyctx' <> tyctx st
               }

  -- Update the interpreter state
  when update (put st')

  -- If a value is entered, print it.
  case lookup "it" mod of
    Nothing -> return ()
    Just ex -> do
      let (val, _) = runEval (tmctx st') "it"  ex
      showOutput (show val) st'

showOutput :: String -> IState -> Repl ()
showOutput arg st = do
  case Env.lookup "it" (tyctx st)  of
    Just val -> liftIO $ putStrLn $ ppsignature (arg, val)
    Nothing -> return ()

cmd :: String -> Repl ()
cmd source = exec False True (L.pack source)

-------------------------------------------------------------------------------
-- Commands
-------------------------------------------------------------------------------

-- :browse command
browse :: [String] -> Repl ()
browse _ = do
  st <- get
  liftIO $ mapM_ putStrLn $ ppenv (tyctx st)

-- :load command
load :: [String] -> Repl ()
load args = do
  contents <- liftIO $ L.readFile (unwords args)
  exec False True contents

-- :type command
typeof :: [String] -> Repl ()
typeof args = do
  st <- get
  let arg = unwords args
  case Env.lookup arg (tyctx st) of
    Just val -> liftIO $ putStrLn $ ppsignature (arg, val)
    Nothing -> exec False False (L.pack arg)

-- :quit command
quit :: a -> Repl ()
quit _ = liftIO exitSuccess

-------------------------------------------------------------------------------
-- Interactive Shell
-------------------------------------------------------------------------------

-- Prefix tab completer
defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [
    (":load"  , fileCompleter)
  --, (":type"  , values)
  ]

-- Default tab completer
comp :: (Monad m, MonadState IState m) => WordCompleter m
comp n = do
  let cmds = [":load", ":type", ":browse", ":quit"]
  Env.TypeEnv ctx <- gets tyctx
  let defs = Map.keys ctx
  return $ filter (isPrefixOf n) (cmds ++ defs)

cmdOptions :: [(String, [String] -> Repl ())]
cmdOptions = [
    ("load"   , load)
  , ("browse" , browse)
  , ("quit"   , quit)
  , ("type"   , typeof)
  ]

-------------------------------------------------------------------------------
-- Entry Point
-------------------------------------------------------------------------------

completer :: CompleterStyle (StateT IState IO)
completer = Prefix (wordCompleter comp) defaultMatcher

shell :: Repl a -> IO ()
shell pre = flip evalStateT initState
     $ evalRepl (pure "Poly> ") cmd cmdOptions Nothing completer pre

-------------------------------------------------------------------------------
-- Toplevel
-------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> shell (return ())
    [fname] -> shell (load [fname])
    ["test", fname] -> shell (load [fname] >> browse [] >> quit ())
    _ -> putStrLn "invalid arguments"