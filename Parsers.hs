{- This module contains Scheme types definitions -}

module Parsers 
    (
      symbol,
      spaces,
      parseChar,
      parseString,
      parseAtom,
      parseNumber,
      parseFloat,
      parseList,
      parseDottedList,
      parseQuoted,
      parseQuasiQuoted,
      parseUnQuote,
      parseVector,
      parseExpr
    ) 
    where
import Types
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric
import Data.Array

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseChar :: Parser LispVal
parseChar = do
    try $ string "#\\"
    value <- try (string "newline" <|> string "space") 
        <|> do { x <- anyChar; notFollowedBy alphaNum ; return [x] }
    return $ Character $ case value of
        "space" -> ' '
        "newline" -> '\n'
        otherwise -> (value !! 0)

-- parse string like "string"
parseString :: Parser LispVal
parseString = do 
    char '"'
    x <- many $ chars
    char '"'
    return $ String x
    where 
        -- to support escape characters
        chars = escaped <|> noneOf "\""
        escaped = char '\\' >> choice (zipWith escapedChar codes replacements)
        escapedChar code replacement = char code >> return replacement
        codes        = ['b',  'n',  'f',  'r',  't',  '\\', '\"', '/']
        replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '/']


-- parse lisp atom or literal true/false
parseAtom :: Parser LispVal
parseAtom = do 
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of 
               "#t" -> Bool True
               "#f" -> Bool False
               _    -> Atom atom  

-- parse number
parseNumber :: Parser LispVal
parseNumber = do
    liftM (Number . read) $ many1 digit

-- parse floating point number
parseFloat :: Parser LispVal
parseFloat = do 
    x <- many1 digit
    char '.'
    y <- many1 digit
    return $ Float (fst.head$readFloat (x++"."++y))  

-- parse parenthesized lisp list
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

-- parse dotted lisp list
parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

-- parse quoted expression
parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

-- parse quasi quoted expressions
parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
    char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote", x]

-- parse unquoted expressions
parseUnQuote :: Parser LispVal
parseUnQuote = do
    char ','
    x <- parseExpr
    return $ List [Atom "unquote", x]

-- parse vectors #(0 (2 2 2 2) "Anna")
parseVector :: Parser LispVal
parseVector = do 
    arrayValues <- sepBy parseExpr spaces
    return $ Vector (listArray (0, (length arrayValues - 1)) arrayValues)

-- parse either string or atom or number
parseExpr :: Parser LispVal
parseExpr = parseAtom
    <|> parseString
    <|> parseNumber
    <|> try parseFloat
    <|> parseChar
    <|> parseQuoted
    <|> try (do 
        char '('
        x <- try parseList <|> parseDottedList
        char ')'
        return x)
    <|> try (do 
        string "#("
        x <- parseVector
        char ')'
        return x)


