module Evaluation where
import Types
import Parsers
import Variables
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Array
import Control.Monad.Error
--  Evaluation part


-- Input evaluation

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = 
    do result <- eval env pred
       case result of
         Bool False -> eval env alt
         otherwise -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
    makeVarargs varargs env params body >>= defineVar env var
eval env (List [Atom "load", String filename]) = 
    load filename >>= liftM last . mapM (eval env)    
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarargs varargs env [] body
eval env (List (function : args)) = do 
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

--eval form@(List (Atom "cond" : clauses)) =
--    if null clauses
--    then throwError $ BadSpecialForm "no true clause in cond expression: " form
--    else case head clauses of
--        List [Atom "else", expr] -> eval expr
--        List [test, expr]        -> eval $ List [Atom "if",
--                                                 test,
--                                                 expr,
--                                                 List (Atom "cond" : tail clauses)]
--        _ -> throwError $ BadSpecialForm "ill-formed cond expression: " form
--eval form@(List (Atom "case" : key : clauses)) =
--    if null clauses
--    then throwError $ BadSpecialForm "no true clause in case expression: " form
--    else case head clauses of
--        List (Atom "else" : exprs) -> mapM eval exprs >>= return . last
--        List ((List datums) : exprs) -> do
--          result <- eval key
--          equality <- mapM (\x -> eqv [result, x]) datums
--          if Bool True `elem` equality
--            then mapM eval exprs >>= return . last
--            else eval $ List (Atom "case" : key : tail clauses)
--        _                     -> throwError $ BadSpecialForm "ill-formed case expression: " form

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . showVal

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args = 
    if num params /= num args && varargs == Nothing
       then throwError $ NumArgs (num params) args
       else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
    where remainingArgs = drop (length params) args
          num = toInteger . length
          evalBody env = liftM last $ mapM (eval env) body 
          bindVarArgs arg env = case arg of
              Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
              Nothing -> return env 
apply (IOFunc func) args = func args


load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

