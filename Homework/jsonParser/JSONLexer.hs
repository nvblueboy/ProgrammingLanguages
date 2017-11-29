module JSONLexer where

import Text.Megaparsec
import Text.Megaparsec.Char -- various basic parsers
import qualified Text.Megaparsec.Char.Lexer as L -- This avoids name clashes with Prelude.

import System.Environment
import System.IO -- needed for file handling
import Data.List  -- needed for mapM

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

json :: Parser JSON
json = ws *> jsValue -- consume any leading whitespace then parser a JSON value

jsValue :: Parser JSON
jsValue = choice [jsNull,jsBool,jsObject,jsArray,jsString]

jsAtom :: String -> JSON -> Parser JSON
jsAtom str val = val <$ lexeme (string str)

-- >>> parse jsNull "(---)" "null"
--- Right JSNull
jsNull :: Parser JSON
jsNull = jsAtom "null" JSNull


-- >>> parse $ jsBool "  false "
-- Right (JSBool False)
-- >>> parse $ jsBool "true"
-- Right (JSBool True)
jsBool :: Parser JSON
jsBool = jsAtom "false" (JSBool False)
     <|> jsAtom "true"  (JSBool True)

-- >>> parse jsString " \"foo bar baz\"  "
-- Right (JSString "foo bar baz")
jsString :: Parser JSON
jsString = JSString <$> lexeme (between (char '"') (char '"') (many jsChar))
  where
    jsChar = unescaped <|> escaped
    unescaped = noneOf "\"\\"
    escaped   = char '\\' *> escapedChar
    escapedChar = choice $ map ch alist
    ch (x,y) = y <$ char x
    alist = [
        ('b', '\b')
      , ('f', '\f')
      , ('n', '\n')
      , ('r', '\r')
      , ('t', '\t')
      , ('\\','\\')
      , ('\"','\"')
      ]

-- >>> parse jsArray "(--)" "  [ 1 , \"foo\" ,  true ] "
-- Right (JSArray [JSNumber 1,JSString "foo",JSBool True])
jsArray :: Parser JSON
jsArray = JSArray <$> betweenChars '[' ']' vals
  where
    vals = jsValue `sepBy` charWs ','
    -- sepBy separates items on a list by some punctuation.

-- >>> parse jsObject "(--)" "{"Boop": 1, "de": 2, "boo": true} "
-- Right JSObject [("Boop": JSNumber 1), ("de": JSNumber 2), ("boo":JSBool True)]
jsObject :: Parser JSON
jsObject = JSObject <$> betweenChars '{' '}' pairs
  where
    pairs = keyval `sepBy` charWs ','
    keyval = do
        JSString key <- jsString
        charWs ':'
        val <- jsValue
        return (key, val)


main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
        -- could be checking for empty list and for existence of the file
    handle <- openFile fileName ReadMode -- open file named by initial argument
    contents <- hGetContents handle  -- get entire contents of file
    print $ parse jsValue fileName contents
    hClose handle  -- you mom taught you to clean up after yourself