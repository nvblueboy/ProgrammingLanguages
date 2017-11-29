module JSONLexer where

import Text.Megaparsec
import Text.Megaparsec.Char -- various basic parsers
import qualified Text.Megaparsec.Char.Lexer as L -- This avoids name clashes with Prelude.


-- The following makes things simpler by setting up no additional state '()
--   and restricting to a string parser.

type Parser = Parsec () String



data JSON = JSNull
          | JSBool Bool
          | JSNumber Integer
          | JSString String
          | JSArray [JSON]
          | JSObject [(String, JSON)]
          deriving (Show, Eq)

spaceConsumer :: Parser()
spaceConsumer = L.space space1 lineCmnt blockCmnt
	where
		lineCmnt = L.skipLineComment "//"
		blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

charWs :: Char -> Parser Char
charWs c = lexeme (char c)

betweenChars :: Char -> Char -> Parser a -> Parser a
betweenChars o c = between(charWs o) (charWs c)