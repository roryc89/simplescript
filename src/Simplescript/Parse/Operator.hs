{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE RecordWildCards #-}

module Simplescript.Parse.Operator (operatorTable) where

import Control.Monad.Combinators.Expr (Operator(..))
import Data.Text (Text)
import Data.Set as Set
import Data.Void (Void)
import Simplescript.Ast (Expr(..), ExprPos, Positions(..))
import Simplescript.Token (TokStream, SToken(..), WithPos(..), tokenVal)
import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec            as Parsec

type Parser = Parsec Void TokStream 

operatorTable :: [[Operator Parser ExprPos]]
operatorTable =
  [ [ InfixL (pure Apply)
    ]
  , [ InfixL pOperator
    ]
  ]

pOperator :: Parser (ExprPos -> ExprPos -> ExprPos)
pOperator = do 
    WithPos{..} <- tokNoErr (
        \case
            Operator op -> Just (`Op` op)
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