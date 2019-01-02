module Main where

import AbsLatte
import LexLatte
import ParLatte
import ErrM

import Control.Monad.Writer
import Control.Monad.State

import Data.List
import Data.Map (Map, (!))
import Data.Maybe
import qualified Data.Map as M

import System.Environment
import System.FilePath
import System.IO
import System.Process

type Loc = Int
type Label = String
type Store = M.Map Label Loc
type SumMonad a = WriterT [String] (StateT (Store, Loc) IO) a


nextLabel :: SumMonad Label

nextLabel = do
  loc <- nextLocation
  return $ "%" ++ show loc


nextLocation :: SumMonad Int

nextLocation = do
  (store, loc) <- get
  let new = loc + 1
  modify $ (\(m, l) -> (m, new))
  return new


transIdent :: Ident -> SumMonad Loc

transIdent (Ident name) = do
  (store, _) <- get
  return $ fromMaybe 0 (M.lookup name store)


transProgram :: Program -> SumMonad ()

transProgram (Program []) = return ()

transProgram (Program (td:tds)) = do
  transTopDef td
  transProgram $ Program tds


transTopDef :: TopDef -> SumMonad ()

transTopDef (FnDef type_ (Ident ident) args block) = do
  tname <- transType type_
  alist <- transArg args
  let anames = intercalate ", " $ alist
  tell ["define " ++ tname ++ " @" ++ ident ++ "(" ++ anames ++ ") {"]
  transBlock block
  tell ["}"]


transArg :: [Arg] -> SumMonad [Label]

transArg args = return []


transBlock :: Block -> SumMonad ()

transBlock (Block []) = return ()

transBlock (Block (stmt:stmts)) = do
  transStmt stmt
  transBlock $ Block stmts


transStmt :: Stmt -> SumMonad ()

transStmt (Empty) = return ()

transStmt (BStmt block) = transBlock block

transStmt (Decl type_ []) = return ()

transStmt (Decl type_ (item:items)) = do
    _ <- transItem item
    transStmt (Decl type_ items)

transStmt (Ass ident@(Ident name) expr) = do
  label <- transExpr expr
  loc   <- transIdent ident
  tell $ ["store i32 " ++ label ++ ", i32* %" ++ show loc]

transStmt (Incr ident) = return ()

transStmt (Decr ident) = return ()

transStmt (Ret expr) = do
  val <- transExpr expr
  tell $ ["ret i32 " ++ val]

transStmt (VRet) = do
  tell $ ["ret void"]

transStmt (Cond expr stmt) = return ()

transStmt (CondElse expr stmt1 stmt2) = return ()

transStmt (While expr stmt) = return ()

transStmt (SExp expr) = do
  label <- transExpr expr
  next  <- nextLabel
  tell $ [next ++ " = call i32 (i8*, ...) @printf(i8* getelementptr inbounds \
          \([4 x i8], [4 x i8]* @.str, i32 0, i32 0), i32 " ++ label ++ ")"]


transItem :: Item -> SumMonad Loc

transItem (NoInit ident) = do
  new <- nextLocation
  tell ["%" ++ show new ++ " = alloca i32"]
  return new

transItem (Init ident@(Ident name) expr) = do
  loc <- nextLocation
  let label = "%" ++ show loc
  tell [label ++ " = alloca i32"]
  val <- transExpr expr
  modify (\(m, l) -> ((M.insert name loc m), l))
  tell $ ["store i32 " ++ val ++ ", i32* " ++ label]
  return loc


transType :: Type -> SumMonad String

transType type_ = case type_ of
  Int  -> return "i32"
  Str  -> return "i8*"
  Bool -> return "i1"
  Void -> return "void"
  Fun ftype atypes -> return $ show ftype


transExpr :: Expr -> SumMonad Label

transExpr (EVar ident) = do
  loc  <- transIdent ident
  next <- nextLabel
  tell [next ++ " = load i32, i32* %" ++ show loc]
  return next

transExpr (ELitInt integer) = return $ show integer

transExpr (ELitTrue) = do
  return ""

transExpr (ELitFalse) = do
  return ""

transExpr (EApp ident exprs) = do
  return ""

transExpr (EString string) = do
  return ""

transExpr (Neg expr) = do
  return ""

transExpr (Not expr) = transExpr (ERel expr EQU ELitFalse)

transExpr (EMul expr1 mulop expr2) = do
  op <- transMulOp mulop
  transIntOp op expr1 expr2

transExpr (EAdd expr1 addop expr2) = do
  op <- transAddOp addop
  transIntOp op expr1 expr2

transExpr (ERel expr1 relop expr2) = do
  op <- transRelOp relop
  transIntOp op expr1 expr2

transExpr (EAnd expr1 expr2) = transBoolOp "and" expr1 expr2

transExpr (EOr expr1 expr2) = transBoolOp "or" expr1 expr2


transIntOp :: String -> Expr -> Expr -> SumMonad Label

transIntOp op expr1 expr2 = do
  label1 <- transExpr expr1
  label2 <- transExpr expr2
  next   <- nextLabel
  tell [next ++ " = " ++ op ++ " i32 " ++ label1 ++ ", " ++ label2]
  return next


transBoolOp :: String -> Expr -> Expr -> SumMonad Label

transBoolOp op expr1 expr2 = do
  label1 <- transExpr expr1
  label2 <- transExpr expr2
  next   <- nextLabel
  tell [next ++ " = " ++ op ++ " i1 " ++ label1 ++ ", " ++ label2]
  return next


transAddOp :: AddOp -> SumMonad String

transAddOp addop = case addop of
  Plus  -> return "add nsw"
  Minus -> return "sub nsw"


transMulOp :: MulOp -> SumMonad String

transMulOp mulop = case mulop of
  Times -> return "mul nsw"
  Div   -> return "sdiv"
  Mod   -> return "srem"


transRelOp :: RelOp -> SumMonad String

transRelOp relop = case relop of
  LTH -> return "icmp slt"
  LE  -> return "icmp sle"
  GTH -> return "icmp sgt"
  GE  -> return "icmp sge"
  EQU -> return "icmp eq"
  NE  -> return "icmp ne"


pack :: [String] -> [String]

pack out = [prelude] ++ out ++ [postlude]
  where
    prelude =
      "@.str = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1"
    postlude = "declare i32 @printf(i8*, ...) #1"


run :: FilePath -> Program -> IO ()

run file prog = do
  ((_, out), _) <- runStateT (runWriterT (transProgram prog)) (M.empty, 0)
  let code = intercalate "\n" $ pack out
  writeFile filePath code
  handle <- runCommand command
  void $ waitForProcess handle
    where
      filePath = dropExtension file ++ ".ll"
      command  = "llvm-as " ++ filePath


main :: IO ()

main = do
args <- getArgs
case args of
  []  -> hPutStr stderr $ "Error: No file provided\n"
  (file:_) -> do
  code <- readFile file
  case pProgram (myLexer code) of
    (Bad msg) -> hPutStr stderr $ "Error: " ++ msg ++ "\n"
    (Ok tree) -> run file tree
