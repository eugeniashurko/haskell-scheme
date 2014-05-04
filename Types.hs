{- This module contains Scheme types definitions -}

module Types where

import Data.Array
import Control.Monad.Error
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.IORef
import System.IO

type Env = IORef [(String, IORef LispVal)]

type IOThrowsError = ErrorT LispError IO

data LispVal = Atom String
          | Number Integer
          | Float Double
          | String String
          | Bool Bool
          | Character Char
          | List [LispVal]
          | DottedList [LispVal] LispVal
          | Vector (Array Int LispVal)
          | Func {params :: [String], vararg :: (Maybe String), body :: [LispVal], closure :: Env}
          | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
          | IOFunc ([LispVal] -> IOThrowsError LispVal)
          | Port Handle 
          -- deriving (Eq)

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String


{- Utils for LispVal and LispError -}

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
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) = 
  "(lambda (" ++ unwords (map show args) ++ 
     (case varargs of 
        Nothing -> ""
        Just arg -> " . " ++ arg) ++ ") ...)" 
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected 
                                  ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError
instance Show LispVal where show = showVal

instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default

type ThrowsError = Either LispError

