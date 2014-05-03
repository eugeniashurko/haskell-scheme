module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Data.Array


-- Parser part

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

-- Datatype definition part
-- data type that represents Lisp values
data LispVal = Atom String
          | Number Integer
          | Float Double
          | String String
          | Bool Bool
          | Character Char
          | List [LispVal]
          | DottedList [LispVal] LispVal
          | Vector (Array Int LispVal)
        
-- parse char like #\d #\D #\3 #\newline #\space
--parseChar :: Parser LispVal
--parseChar = do
--    _ <- try (string "#\\")
--    x <- anyChar
--    y <- many $ letter
--    let xy = x:y
--    return $ case xy of
--        "space" -> Character ' '
--        "newline" -> Character '\n'
--        _ -> Character x

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


--  Evaluation part
instance Show LispVal where show = showVal

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Character contents) = "#\\" ++ [contents]
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _          = 0

primitives :: [(String , [LispVal] -> LispVal)]
primitives = [("+" , numericBinop (+)) ,
              ("-" , numericBinop (-)) ,
              ("*" , numericBinop (*)) ,
              ("/" , numericBinop div) ,
              ("mod" , numericBinop mod) ,
              ("quotient" , numericBinop quot) ,
              ("remainder" , numericBinop rem) ,
              ("symbol?" , unaryOp symbolp) ,
              ("string?" , unaryOp stringp) ,
              ("number?" , unaryOp numberp) ,
              ("bool?", unaryOp boolp) ,
              ("list?" , unaryOp listp)]

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f [v] = f v

symbolp, numberp, stringp, boolp, listp :: LispVal -> LispVal
symbolp (Atom _)   = Bool True
symbolp _          = Bool False
numberp (Number _) = Bool True
numberp _          = Bool False
stringp (String _) = Bool True
stringp _          = Bool False
boolp   (Bool _)   = Bool True
boolp   _          = Bool False
listp   (List _)   = Bool True
listp   (DottedList _ _) = Bool True
listp   _          = Bool False

symbol2string, string2symbol :: LispVal -> LispVal
symbol2string (Atom s)   = String s
symbol2string _          = String ""
string2symbol (String s) = Atom s
string2symbol _          = Atom ""

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

main :: IO ()
main = getArgs >>= print . eval . readExpr . head