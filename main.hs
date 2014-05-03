module Main where
import Types
import Parsers
import Primitives
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Array
import Control.Monad.Error

--  Evaluation part

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- Input evaluation
eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "if", pred, conseq, alt]) = 
    do result <- eval pred
       case result of
         Bool False -> eval alt
         otherwise -> eval conseq
eval form@(List (Atom "cond" : clauses)) =
    if null clauses
    then throwError $ BadSpecialForm "no true clause in cond expression: " form
    else case head clauses of
        List [Atom "else", expr] -> eval expr
        List [test, expr]        -> eval $ List [Atom "if",
                                                 test,
                                                 expr,
                                                 List (Atom "cond" : tail clauses)]
        _ -> throwError $ BadSpecialForm "ill-formed cond expression: " form
eval form@(List (Atom "case" : key : clauses)) =
    if null clauses
    then throwError $ BadSpecialForm "no true clause in case expression: " form
    else case head clauses of
        List (Atom "else" : exprs) -> mapM eval exprs >>= return . last
        List ((List datums) : exprs) -> do
          result <- eval key
          equality <- mapM (\x -> eqv [result, x]) datums
          if Bool True `elem` equality
            then mapM eval exprs >>= return . last
            else eval $ List (Atom "case" : key : tail clauses)
        _                     -> throwError $ BadSpecialForm "ill-formed case expression: " form
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

main :: IO ()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled