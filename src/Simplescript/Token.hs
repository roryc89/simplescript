{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE DeriveFunctor  #-}


module Simplescript.Token where 
import Data.List.NonEmpty (NonEmpty (..))

import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import qualified Data.List          as DL
import qualified Data.List.NonEmpty as NE
import qualified Data.Set           as Set

type TokenAndPosLine = Line (WithPos SToken)

data Line a = Line Text [a] [Line a]
    deriving (Eq, Ord, Show, Functor)

removePositions :: TokenAndPosLine -> Line SToken
removePositions = fmap tokenVal

data SToken 
    = Identifier Text
    | Int Int
    | Number Double
    | Operator Text
    | SChar Char
    | SString Text
    | Assign
    | Backslash
    | Colon
    | Comma
    | LParen 
    | RParen
    | LBrace
    | RBrace
    | LSquareBracket
    | RSquareBracket
    deriving (Eq, Ord, Show)

showSToken :: SToken -> Text
showSToken = \case
    Identifier s -> s
    Operator s -> s
    SChar s -> T.singleton s
    SString s -> "\"" <>  s <> "\"" 
    Int n -> T.pack $ show n
    Number n -> T.pack $ show n
    Assign -> "="
    Backslash -> "\\"
    Colon -> ":"
    Comma -> ","
    LParen -> "("
    RParen -> ")"
    LBrace -> "{"
    RBrace -> "}"
    LSquareBracket -> "["
    RSquareBracket -> "]"

data WithPos a = WithPos
  { startPos :: SourcePos
  , endPos :: SourcePos
  , tokenLength :: Int
  , tokenVal :: a
  } deriving (Eq, Ord, Show)


data TokStream = TokStream
  { tokStreamInput :: Text -- for showing offending lines
  , unTokStream :: [WithPos SToken]
  }
  deriving (Eq, Ord, Show)

instance ShowErrorComponent TokStream where 
  showErrorComponent stream = show stream

instance Stream TokStream where
  type Token  TokStream = WithPos SToken
  type Tokens TokStream = [WithPos SToken]
  tokenToChunk Proxy x = [x]
  tokensToChunk Proxy xs = xs
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  take1_ (TokStream _ []) = Nothing
  take1_ (TokStream str (t:ts)) = Just
    ( t
    , TokStream (T.drop (tokensLength pxy (t:|[])) str) ts
    )
  takeN_ n (TokStream str s)
    | n <= 0    = Just ([], TokStream str s)
    | null s    = Nothing
    | otherwise =
        let (x, s') = splitAt n s
        in case NE.nonEmpty x of
          Nothing -> Just (x, TokStream str s')
          Just nex -> Just (x, TokStream (T.drop (tokensLength pxy nex) str) s')
  takeWhile_ f (TokStream str s) =
    let (x, s') = DL.span f s
    in case NE.nonEmpty x of
      Nothing -> (x, TokStream str s')
      Just nex -> (x, TokStream (T.drop (tokensLength pxy nex) str) s')
  showTokens Proxy = T.unpack
    . T.unwords
    . NE.toList
    . fmap (showSToken . tokenVal)

  tokensLength Proxy xs = sum (tokenLength <$> xs)
  reachOffset o PosState {..} =
    ( prefix ++ T.unpack restOfLine
    , PosState
        { pstateInput = TokStream
            { tokStreamInput = postStr
            , unTokStream = post
            }
        , pstateOffset = max pstateOffset o
        , pstateSourcePos = newSourcePos
        , pstateTabWidth = pstateTabWidth
        , pstateLinePrefix = prefix
        }
    )
    where
      
      prefix =
        if sameLine
          then pstateLinePrefix ++ T.unpack preStr
          else T.unpack preStr
      sameLine = sourceLine newSourcePos == sourceLine pstateSourcePos
      newSourcePos =
        case post of
          [] -> pstateSourcePos
          (x:_) -> startPos x
      (pre, post) = splitAt (o - pstateOffset) (unTokStream pstateInput)
      (preStr, postStr) = T.splitAt tokensConsumed (tokStreamInput pstateInput)
      tokensConsumed =
        case NE.nonEmpty pre of
          Nothing -> 0
          Just nePre -> tokensLength pxy nePre
      restOfLine = T.takeWhile (/= '\n') postStr

pxy :: Proxy TokStream
pxy = Proxy


liftSToken :: SToken -> WithPos SToken
liftSToken = WithPos pos pos 0
  where
    pos = initialPos ""
