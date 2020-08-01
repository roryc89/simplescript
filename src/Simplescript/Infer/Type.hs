{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE DeriveFoldable  #-}

module Simplescript.Infer.Type where 

import Data.Bifunctor 
import Data.Map 
import Simplescript.Infer.UsedTypes
import Data.Void
-- import 

type ConcreteType = TypeExt Void

type TypeOrId = TypeExt Int 

data TypeExt id
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
        (TypeExt id) -- from
        (TypeExt id) -- to
    | Int
    | Float
    | Char
    | String
    | Bool
    | Unit
    -- | Record (Map VarName (TypeExt id))
    | {- The actual definitions of type aliases custom types are elsewhere
         (in the Declaration module), this is just a "pointer", "var".

         Also, this is the *usage* of a type! So while definition of Maybe
         might be `Maybe a`, here you'll most likely see specific stuff
         like `Maybe Int`.

         This constructor encompasses both type aliases custom types:
      -}
      UserDefinedType
        --  qual -- qualifiedness
         String -- name
         [TypeExt id] -- args
    | Id id
    deriving (Eq, Ord, Functor, Foldable)

        -- where 
        --     mapIdTypeExts :: (a -> b) -> [TypeExt a c] -> [TypeExt b c]
        --     mapIdTypeExts f = fmap (mapIdTypeExt f)

        --     mapIdTypeExt :: (a -> b) -> TypeExt a c -> TypeExt b c
        --     mapIdTypeExt f = first (first f)


