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
import qualified Debug.Trace as D
import Safe

type TokenAndPosLine = Line [WithPos SToken]

data Line a = Line
    { line :: a
    , indented :: [Line a]
    }
    deriving (Eq, Ord, Show, Functor)

flattenLines :: [Line [WithPos SToken]] -> [WithPos SToken]
flattenLines =  DL.intercalate [liftSToken Newline] . fmap flattenLine

flattenLine :: Line [WithPos SToken] -> [WithPos SToken]
flattenLine Line{..} = 
  line <> (liftSToken IndentedNewline : flattenLines indented)

linesToList :: Line [a] -> [[a]]
linesToList Line{..} = [line] <> (linesToList =<< indented)

showTokenAndPosLines :: [TokenAndPosLine] -> Text 
showTokenAndPosLines =  T.strip . T.intercalate "\n" . showTokenAndPosLine . Line []

showTokenAndPosLine :: TokenAndPosLine -> [Text]
showTokenAndPosLine = fmap showSTokensWithIdent . insertBlankLines . linesToList 

insertBlankLines :: [[WithPos SToken]] -> [[WithPos SToken]]
insertBlankLines (row1:row2:tail) = row1 : (replicate blanks [] <> insertBlankLines (row2:tail))
    where 
        blanks = case (row1IdxMay, nextRowIdxMay) of 
            (Just row1Idx, Just nextRowIdx) -> nextRowIdx - row1Idx - 1
            _ -> 0
          
        row1IdxMay = getRow <$>  headMay row1
        nextRowIdxMay = getRow <$> (headMay =<< DL.find (/= []) (row2:tail))

        getRow = unPos . sourceLine . startPos

insertBlankLines toks = toks 

removePositions :: TokenAndPosLine -> Line [SToken]
removePositions = fmap (fmap tokenVal)

data SToken 
    = Keyword Keyword
    | Identifier Text
    | Int Int
    | Number Double
    | Operator Text
    | SChar Char
    | SString Text
    | Assign
    | Backslash
    | Arrow
    | Colon
    | Comma
    | LParen 
    | RParen
    | LBrace
    | RBrace
    | LSquareBracket
    | RSquareBracket
    | Newline
    | IndentedNewline
    deriving (Eq, Ord, Show)

data Keyword 
    = Let 
    | In 
    | Case 
    | Of 
    | Is 
    | If 
    | Then 
    | Else 
    | Import
    | Export
    | Help
    deriving (Eq, Ord, Show)

showSTokensWithIdent :: [WithPos SToken] -> Text
showSTokensWithIdent = go ""
  where 
    go :: Text -> [WithPos SToken] -> Text
    go result ts@(h:t) = 
        go (T.justifyLeft col ' ' result <> showSToken (tokenVal h)) t
        where 
            col = unPos (sourceColumn $ startPos h) - 1
    go result ts = result

showSToken :: SToken -> Text
showSToken = \case
    Keyword k -> T.toLower $ T.pack $ show k
    Identifier s -> s
    Operator s -> s
    SChar s -> T.singleton s
    SString s -> "\"" <>  s <> "\"" 
    Int n -> T.pack $ show n
    Number n -> T.pack $ show n
    Assign -> "="
    Arrow -> "=>"
    Backslash -> "\\"
    Colon -> ":"
    Comma -> ","
    LParen -> "("
    RParen -> ")"
    LBrace -> "{"
    RBrace -> "}"
    LSquareBracket -> "["
    RSquareBracket -> "]"
    Newline -> "\n"
    IndentedNewline -> "\n"

data WithPos a = WithPos
  { startPos :: SourcePos
  , endPos :: SourcePos
  , tokenLength :: Int
  , tokenVal :: a
  } deriving (Eq, Ord, Show, Functor)


newtype TokStream = TokStream [WithPos SToken]
  deriving (Eq, Ord, Show)

tokStreamInput :: TokStream -> Text
tokStreamInput = T.concat . fmap (showSToken . tokenVal) . unTokStream

unTokStream :: TokStream -> [WithPos SToken]
unTokStream (TokStream toks) = toks

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
  take1_ (TokStream []) = Nothing
  take1_ (TokStream (t:ts)) = Just
    ( t
    , TokStream ts
    )
  takeN_ n (TokStream s)
    | n <= 0    = Just ([], TokStream s)
    | null s    = Nothing
    | otherwise =
        let (x, s') = splitAt n s
        in case NE.nonEmpty x of
          Nothing -> Just (x, TokStream s')
          Just nex -> Just (x, TokStream s')
  takeWhile_ f (TokStream s) =
    let (x, s') = DL.span f s
    in case NE.nonEmpty x of
      Nothing -> (x, TokStream s')
      Just nex -> (x, TokStream s')
  showTokens Proxy = T.unpack
    . T.unwords
    . NE.toList
    . fmap (showSToken . tokenVal)

  tokensLength Proxy xs = sum (tokenLength <$> xs)
  reachOffset o PosState {..} =
    ( prefix ++ T.unpack restOfLine
    , PosState
        { pstateInput = TokStream post
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
