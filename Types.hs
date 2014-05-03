{- This module contains Scheme types definitions -}

module Types 
    (
        LispVal 
        (
          Atom,
          Number,
          Float,
          String,
          Bool,
          Character,
          List,
          DottedList,
          Vector
        ),
        LispError 
        (
          NumArgs,
          TypeMismatch,
          Parser,
          BadSpecialForm,
          NotFunction,
          UnboundVar,
          Default
        )
    ) 
    where
import Data.Array
import Control.Monad.Error
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
          | Number Integer
          | Float Double
          | String String
          | Bool Bool
          | Character Char
          | List [LispVal]
          | DottedList [LispVal] LispVal
          | Vector (Array Int LispVal)

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String