{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE DeriveFoldable  #-}

module Simplescript.SuperSimple.Type where 

import Data.Bifunctor 
import Data.Map 
import Simplescript.Infer.UsedTypes
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
      TypeVar String
    | Function
        Type -- from
        Type -- to
    | Int
    | Bool
    deriving(Eq, Ord, Show)