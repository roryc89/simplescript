{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE DeriveFoldable  #-}

module Simplescript.SuperSimple.Type where 

import Data.Bifunctor 
import Data.Map 
import Simplescript.Infer.UsedTypes
import Data.Text
import Data.Void

data Type
    = {- Example of a `TypeVar`:

             foo : var a. a -> Int

         will be parsed as

            TypeAnnotation
                { name = "foo"
                , type_ =
                    Function
                        { from = TypeVar "a"
                        , to = Int
                        }
                }

         These are the type variables user has given name to. (There are also
         `Id 0`-like values which are being given names by the compiler.)
      -}
      TypeVar Text
    | Function
        Type -- from
        Type -- to
    | Int
    | Bool
    | UserDefined Int Text
    | Id Int
    deriving(Eq, Ord, Show)

data Ctr = Ctr Text [Type]
        deriving(Eq, Ord, Show)
