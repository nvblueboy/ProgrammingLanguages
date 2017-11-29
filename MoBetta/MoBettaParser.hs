module MoBettaParser where

import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr

import MoBettaAST

type Parser = Parsec () String


spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

semicolon = lexeme (char ';')
lparen = lexeme (char '(')
rparen = lexeme (char ')')
lbrace = lexeme (char '{')
rbrace = lexeme (char '}')


