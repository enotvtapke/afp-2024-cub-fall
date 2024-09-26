module Imp.Eval (Error (..), runProgram) where

import Control.Monad (when)
import Control.Monad.State
  ( MonadTrans (lift),
    StateT,
    evalStateT,
    gets,
    modify,
  )
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import qualified Data.Map.Strict as M
import Imp.Lang
import Text.Printf (printf)
import Text.Read (readMaybe)
import Control.Monad.RWS (RWST, evalRWST, asks, tell)

-- Variable mapping; shadowing is allowed
type VarMap = M.Map String Int

-- Possible errors reported during evaluation
data Error = UndefinedVar String | ParsingErr String | DivByZero deriving (Show)

newtype Config = Config { exitOnParsingErr :: Bool }

-- Evaluation monad
-- type EvalM = StateT VarMap (ExceptT Error IO)
type EvalM = RWST Config [Error] VarMap (ExceptT Error IO)

evalExpr :: Expr -> EvalM Int
evalExpr (Const x) = return x
evalExpr (Var name) = do
  val <- gets $ M.lookup name
  case val of
    Just x -> return x
    Nothing -> lift $ throwE (UndefinedVar name)
evalExpr (BinOp op lExpr rExpr) = do
  rVal <- evalExpr rExpr
  when (op == Div && rVal == 0) $ lift $ throwE DivByZero
  evalOp op <$> evalExpr lExpr <*> return rVal
  where
    evalOp :: Op -> Int -> Int -> Int
    evalOp Plus = (+)
    evalOp Minus = (-)
    evalOp Mult = (*)
    evalOp Div = div

evalCom :: Com -> EvalM ()
evalCom (Assign varName rhs) = do
  rhsVal <- evalExpr rhs
  modify (M.insert varName rhsVal)
evalCom (Read s) = do
  line <- lift $ lift getLine
  exit <- asks exitOnParsingErr
  case readMaybe line of
    Nothing ->
      if exit
        then
          lift $ throwE (ParsingErr line)
        else
          tell [ParsingErr line] >> evalCom (Read s)
    Just val -> modify (M.insert s val)
evalCom (Write expr) = evalExpr expr >>= lift . lift . print
evalCom (Seq lhs rhs) = evalCom lhs >> evalCom rhs
-- Now I understand, why it doesn't work.
-- evalCom (If cond lBranch rBranch) = (\c x y -> if c >= 0 then x else y) <$> evalExpr cond <*> evalCom lBranch <*> evalCom rBranch
evalCom (If cond lBranch rBranch) =
  do
    c <- evalExpr cond
    if c == 0 then evalCom lBranch else evalCom rBranch
evalCom Skip = return ()

evalProg :: Prog -> EvalM Int
evalProg (Prog com expr) = evalCom com >> evalExpr expr

eval :: Prog -> VarMap -> IO (Either Error (Int, [Error]))
eval program state = runExceptT (evalRWST (evalProg program) (Config True) state)

runProgram :: Prog -> IO ()
runProgram program = do
  result <- eval program M.empty
  case result of
    Left err -> print err
    Right x -> putStrLn $ printf "Result: %s" (show x)
  putStrLn ""
