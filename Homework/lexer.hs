module MoBettaLexa where

import Text.Megaparsec
import Text.Megaparsec.Char -- various basic parsers
import qualified Text.Megaparsec.Char.Lexer as L -- This avoids name clashes with Prelude.


-- The following makes things simpler by setting up no additional state '()
--   and restricting to a string parser.

type Parser = Parsec () String

-- The module Megaparsec defines a function
--   `parse :: Parsec e s a -> String -> s -> Either (ParseError (Token s) e) a`
-- So we get
--   `parse :: Parser a -> String -> String -> Either (ParseError (Token String) ()) a`


-- space1 is a parser from Text.Megaparsec.Char that will consume one or more whitespaces
-- L.space is a lexical analyzer that uses its first arguement to consume space characters and ignores comments per its 2nd and 3rd arguments
-- L.skipLineComment matches its 1st argument then ignores everything to end of line
-- L.skipblockComment matches begin- and end-comment strings and ignores everything between

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

-- Define a wrapper that consumes space after a parser
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

data MoBettaToken
  = Identifier String
  | Constant Integer
  | StringLiteral String
  | LParen
  deriving (Show, Eq)

keywords = ["while", "if", "then", "else", "print", "print","message", "read"]

-- Identifiers are defined as consisting of an alpha character followed
--  by any number of alphanumeric characters.
--  `lexeme` ignores trialing whitespace and comments. 'try' rewinds the parser
--  so that if it fails, it does not produce an error, leaving the stream
--  in its original state.
identifier :: Parser String
identifier = (lexeme . try) p
  where
    p = (:) <$> letterChar <*> many alphaNumChar

identifier' :: Parser MoBettaToken
identifier' = fmap Identifier identifier

intConst :: Parser Integer
intConst = (lexeme . try) ic
  where
    ic = do
      x <- L.decimal -- parse a literal
      notFollowedBy letterChar -- fail if followed by a letter
      return x -- return the  result if we haven't failed

intConst' :: Parser MoBettaToken
intConst' = fmap Constant intConst

stringLiteral :: Parser String
stringLiteral = char '"' *> manyTill L.charLiteral (char '"')

stringLiteral' :: Parser MoBettaToken
stringLiteral' = fmap StringLiteral stringLiteral

lparen :: Parser Char
lparen = (lexeme . try ) (char '(')

lparen' :: Parser MoBettaToken
lparen' = lparen *> return LParen
