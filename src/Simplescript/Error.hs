module Simplescript.Error where 
import qualified Data.Text                  as T
import qualified Text.Megaparsec            as Parsec
import           Data.Void                  (Void)
import qualified Simplescript.Token as Tok

data ParseOrLexError 
    = LexError (Parsec.ParseErrorBundle T.Text Void)
    | ParseError (Parsec.ParseErrorBundle Tok.TokStream Void)
    deriving (Show, Eq)