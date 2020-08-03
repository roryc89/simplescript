{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE RecordWildCards #-}

module Simplescript.Parse.TypeOperator (typeOperatorTable) where

import Control.Monad.Combinators.Expr (Operator(..))
import Data.Text (Text)
import Data.Set as Set
import Data.Void (Void)
import Simplescript.Ast (Type(..), TypePos, Positions(..))
import Simplescript.Token (TokStream, SToken(..), WithPos(..), tokenVal)
import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec            as Parsec

type Parser = Parsec Void TokStream 

typeOperatorTable :: [[Operator Parser TypePos]]
typeOperatorTable =
  [ [ InfixL (pure TypeApply)
    ]
  , [ InfixL pOperator
    ]
  ]

pOperator :: Parser (TypePos -> TypePos -> TypePos)
pOperator = do 
    WithPos{..} <- tokNoErr (
        \case
            Operator op -> Just (`TypeOp` op)
            _ -> Nothing)

    pure $ tokenVal Positions{..}

tokNoErr ::  (SToken -> Maybe a) -> Parser (WithPos a)
tokNoErr f = tokWPosNoErr 
    (\WithPos{..} -> 
        case f tokenVal of 
            Just a -> 
                Just $ WithPos startPos endPos tokenLength a 
            _ -> Nothing 
        )

tokWPosNoErr :: (WithPos SToken -> Maybe a) -> Parser a
tokWPosNoErr f = Parsec.token f Set.empty