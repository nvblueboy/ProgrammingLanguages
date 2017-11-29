module MoBettaParser where

import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr

import MoBettaAST

type Parser = Parsec () String

programParser = sepEndBy1 statementParser semicolon <?> "program"

statementParser = choice [
    skipStmt,
    printStmt,
    readStmt,
    messageStmt,
    ifStmt,
   	whileStmt,
   	assignmentStmt,
   	blockStmt
] where
	skipStmt = lexeme (string "skip") >> return Skip
	printStmt = do
		lexeme (string "print")
		e <- aExpr
		return (Print e)
	readStmt = do
		lexeme (string "read")
		i <- identifier
		return (Read i)
	messageStmt = do
		lexeme (string "message")
		s <- stringLiteral
		return (Msg s)
	ifStmt = do
		lexeme (string "if")
		b <- bExpr
		lexeme (string "then")
		stmt1 <- statementParser
		lexeme (string "else")
		stmt2 <- statementParser
		return (If b stmt1 stmt2)
 

aExpr = makeExprParser aFactor AOpTable <?> "arithmetic expression"

aFactor = choice [intConst,
				  identifierExpr,
				  between lparen rparen aExpr,
				  ] <?> "arithmetic factor"

aOpTable = [ [ prefix "-" (AUn Neg)
		     , prefix "+" (id)]
		   , [ binary "*" (ABin Mul)
		     , binary "/" (ABin Div)
		     , binary "%" (ABin Mod)]
		   , [ binary "+" (ABin Add)
		     , binary "-" (ABin Sub)] ]

bExpr = makeExprParser bFactor BOpTable <?> "Boolean expression"

bFactor = choice [ comparison,
				   between lparen rparen bExpr] <?> "boolean factor"

bOpTable = [ [ prefix "not" (BUn Not)]
           , [ binary "and" (BBin And)
             , binary "or" (BBin Or)] ]

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


