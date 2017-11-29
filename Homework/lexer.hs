module MoBettaLexa where

import Text.Megaparsec
import Text.Megaparsec.Char -- various basic parsers
import qualified Text.Megaparsec.Char.Lexer as L -- This avoids name clashes with Prelude.
import System.Environment
import System.IO -- needed for file handling
import Data.List  -- needed for mapM

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
  | Keyword String
  | StringLiteral String
  | Assignment
  | LParen
  | RParen
  | LBrace
  | RBrace
  | BinOp BO
  | CmpOp CO
  | UnOp
  | Separator
  deriving (Show, Eq)

data BO
  = Plus
  | Minus
  | Times
  | Divide
  | Remainder
  | Or
  | And
  deriving (Show, Eq)

data CO
  = NotEq
  | Eq
  | LessThan
  | GreaterThan
  deriving (Show, Eq)

keywords = ["while", "if", "then", "else", "print","message", "read"]

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
identifier' = do
    x <- identifier
    if x `elem` keywords
        then
            return (Keyword x)
        else 
            return (Identifier x)

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

rparen :: Parser Char
rparen = (lexeme . try ) (char ')')

rparen' :: Parser MoBettaToken
rparen' = rparen *> return RParen

lbrace :: Parser Char
lbrace = (lexeme . try ) (char '{')

lbrace' :: Parser MoBettaToken
lbrace' = lbrace *> return LBrace

rbrace :: Parser Char
rbrace = (lexeme . try ) (char '}')

rbrace' :: Parser MoBettaToken
rbrace' = rbrace *> return RBrace

unop :: Parser Char
unop = (lexeme . try ) (char '-')

unop' :: Parser MoBettaToken
unop' = unop *> return UnOp

separator :: Parser Char
separator = (lexeme . try ) (char ';')

separator' :: Parser MoBettaToken
separator' = separator *> return Separator

assignment :: Parser Char
assignment = (lexeme . try ) (char '=')

assignment' :: Parser MoBettaToken
assignment' = assignment *> return Assignment

-- Binary Operations are separated into a separate set, listen in MoBettaToken as type BO.
--     So to parse these, you have to parse characters/strings into a type BO and then the
--     corresponding BO into a MoBettaToken as a subset of BinOp.
-- So when you parse these types (they parse just like other parsers), they will create a type
--     "BinOp <operation>"
-- Example usage:
--     input: parse (many (plus' <|> minus' <|> identifier')) "(no source)" "+ - test"
--     output: Right [BinOp Plus,BinOp Minus,Identifier "test"]

-- Step 1: Parse the char to match the plus sign.
plus :: Parser Char
plus = (lexeme . try ) (char '+')

-- Step 2: Use the char parser to create a BO type.
plusBO :: Parser BO
plusBO = plus *> return Plus

-- Step 3: Return the BO type as a BinOp in MoBettaToken.
plus' :: Parser MoBettaToken
plus' = do
  x <- plusBO
  return (BinOp x)
  
minus :: Parser Char
minus = (lexeme . try ) (char '-')

minusBO :: Parser BO
minusBO = minus *> return Minus

minus' :: Parser MoBettaToken
minus' = do
  x <- minusBO
  return (BinOp x)

times :: Parser Char
times = (lexeme . try ) (char '*')

timesBO :: Parser BO
timesBO = times *> return Times

times' :: Parser MoBettaToken
times' = do
  x <- timesBO
  return (BinOp x)

divide :: Parser Char
divide = (lexeme . try ) (char '/')

divideBO :: Parser BO
divideBO = divide *> return Divide

divide' :: Parser MoBettaToken
divide' = do
  x <- divideBO
  return (BinOp x)

remainder :: Parser Char
remainder = (lexeme . try ) (char '%')

remainderBO :: Parser BO
remainderBO = remainder *> return Remainder

remainder' :: Parser MoBettaToken
remainder' = do
  x <- remainderBO
  return (BinOp x)

orCondition :: Parser String
orCondition = (lexeme . try ) (string "||")

orConditionBO :: Parser BO
orConditionBO = orCondition *> return Or

orCondition' :: Parser MoBettaToken
orCondition' = do
  x <- orConditionBO
  return (BinOp x)

andCondition :: Parser String
andCondition = (lexeme . try ) (string "&&")

andConditionBO :: Parser BO
andConditionBO = andCondition *> return And

andCondition' :: Parser MoBettaToken
andCondition' = do
  x <- andConditionBO
  return (BinOp x)

notEquals :: Parser String
notEquals = (lexeme . try ) (string "!=")

notEqualsCO :: Parser CO
notEqualsCO = notEquals *> return NotEq

notEquals' :: Parser MoBettaToken
notEquals' = do
  x <- notEqualsCO
  return (CmpOp x)

equals :: Parser String
equals = (lexeme . try ) (string "==")

equalsCO :: Parser CO
equalsCO = equals *> return Eq

equals' :: Parser MoBettaToken
equals' = do
  x <- equalsCO
  return (CmpOp x)

lessThan :: Parser Char
lessThan = (lexeme . try ) (char '<')

lessThanCO :: Parser CO
lessThanCO = lessThan *> return LessThan

lessThan' :: Parser MoBettaToken
lessThan' = do
  x <- lessThanCO
  return (CmpOp x)

greaterThan :: Parser Char
greaterThan = (lexeme . try ) (char '>')

greaterThanCO :: Parser CO
greaterThanCO = greaterThan *> return GreaterThan

greaterThan' :: Parser MoBettaToken
greaterThan' = do
  x <- greaterThanCO
  return (CmpOp x)

token' :: Parser MoBettaToken
token' = choice [identifier', intConst', stringLiteral', lparen', rparen', lbrace', rbrace', unop', separator', assignment', plus', minus', times', divide', remainder', orCondition', andCondition', notEquals', equals', lessThan', greaterThan']

data Expression
    = Term 
    | Expression Plus Term
    | Expression Minus Term

data Term
    = IntConst

factor :: Term
factor = choice[intConst', identifier', between Lparen Rparen intConst']