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
type Lbl = String
type Val = (Type, Lbl)
type Store = M.Map Lbl Val
type SumMonad a = WriterT [String] (StateT (Store, Loc) IO) a


getLbl :: SumMonad Lbl

getLbl = do
  (store, loc) <- get
  let next = loc + 1
  return $ "%" ++ show next


nextLbl :: SumMonad Lbl

nextLbl = do
  (store, loc) <- get
  let next = loc + 1
  modify (\(m, l) -> (m, next))
  return $ "%" ++ show next


transIdent :: Ident -> SumMonad Val

transIdent (Ident name) = do
  (store, _) <- get
  return $ fromMaybe (Void, "") (M.lookup name store)


transProgram :: Program -> SumMonad ()

transProgram (Program []) = return ()

transProgram (Program (td:tds)) = do
  transTopDef td
  transProgram $ Program tds


transTopDef :: TopDef -> SumMonad ()

transTopDef (FnDef tp (Ident name) args block) = do
  tname <- transType tp
  alist <- transArgs args
  let ftype = (Fun tp (argsTypes args))
  let lbl = "@" ++ name
  modify (\(m, l) -> ((M.insert name (ftype, lbl) m), l))
  let anames = intercalate ", " alist
  tell ["define " ++ tname ++ " " ++ lbl ++ "(" ++ anames ++ ") {"]
  transBlock block
  tell ["}"]


argsTypes :: [Arg] -> [Type]

argsTypes [] = []

argsTypes ((Arg tp ident):args) = tp : (argsTypes args)


transArgs :: [Arg] -> SumMonad [Lbl]

transArgs [] = return []

transArgs ((Arg tp (Ident name)):args) = do
  tname <- transType tp
  rest  <- transArgs args
  let name = tname ++ " " ++ name
  return (name:rest)


transArgsExprs :: [Expr] -> SumMonad [Lbl]

transArgsExprs [] = return []

transArgsExprs (expr:exprs) = do
  (tp, val) <- transExpr expr
  tname     <- transType tp
  rest      <- transArgsExprs exprs
  let name = tname ++ " " ++ val
  return (name:rest)


transBlock :: Block -> SumMonad ()

transBlock (Block []) = return ()

transBlock (Block (stmt:stmts)) = do
  transStmt stmt
  transBlock $ Block stmts


transStmt :: Stmt -> SumMonad ()

transStmt (Empty) =
  return ()

transStmt (BStmt block) = transBlock block

transStmt (Decl tp []) =
  return ()

transStmt (Decl tp (item:items)) = do
  transItem tp item
  transStmt (Decl tp items)

transStmt (Ass ident@(Ident name) expr) = do
  (tp, lbl) <- transIdent ident
  (_, val)  <- transExpr expr
  tname     <- transType tp
  tell ["store " ++ tname ++ " " ++ val ++ ", " ++ tname ++ "* " ++ lbl]

transStmt (Incr ident) =
  transStmt (Ass ident (EAdd (EVar ident) Plus (ELitInt 1)))

transStmt (Decr ident) =
  transStmt (Ass ident (EAdd (EVar ident) Minus (ELitInt 1)))

transStmt (Ret expr) = do
  (tp, val) <- transExpr expr
  tname     <- transType tp
  tell ["ret " ++ tname ++ " " ++ val]

transStmt (VRet) = do
  tell ["ret void"]

transStmt (Cond expr stmt) = do
  lbl <- getLbl
  let (_:next) = lbl
  (tp, val) <- transExpr expr
  tname     <- transType tp
  tell ["br " ++ tname ++ " " ++ val ++ ", label %then" ++ next
        ++ ", label %end" ++ next,
        "then" ++ next ++ ":"]
  transStmt stmt
  tell ["br label %end" ++ next,
        "end" ++ next ++ ":"]

transStmt (CondElse expr stmt1 stmt2) = do
  lbl <- getLbl
  let (_:next) = lbl
  (tp, val) <- transExpr expr
  tname     <- transType tp
  tell ["br " ++ tname ++ " " ++ val ++ ", label %then" ++ next
        ++ ", label %else" ++ next,
        "then" ++ next ++ ":"]
  transStmt stmt1
  tell ["br label %end" ++ next,
        "else" ++ next ++ ":"]
  transStmt stmt2
  tell ["br label %end" ++ next,
        "end" ++ next ++ ":"]

transStmt (While expr stmt) = do
  lbl <- getLbl
  let (_:next) = lbl
  tell ["br label %cond" ++ next ++ ""]

  (tp, val) <- transExpr expr
  tname     <- transType tp
  tell ["br " ++ tname ++ " " ++ val ++ ", label %loop" ++ next
        ++ ", label %end" ++ next,
        "loop" ++ next ++ ":"]
  transStmt stmt
  tell ["br label %cond" ++ next,
        "end" ++ next ++ ":"]

transStmt (SExp expr) =
  void $ transExpr expr


transItem :: Type -> Item -> SumMonad ()

transItem tp (NoInit ident@(Ident name)) = do
  tname <- transType tp
  lbl   <- nextLbl
  modify (\(m, l) -> ((M.insert name (tp, lbl) m), l))
  tell [lbl ++ " = alloca " ++ tname]

transItem tp (Init ident@(Ident name) expr) = do
  tname <- transType tp
  lbl   <- nextLbl
  modify (\(m, l) -> ((M.insert name (tp, lbl) m), l))
  tell [lbl ++ " = alloca " ++ tname]

  (_, val) <- transExpr expr
  tell ["store " ++ tname ++ " " ++ val ++ ", " ++ tname ++ "* " ++ lbl]


transType :: Type -> SumMonad String

transType tp = case tp of
  Int  -> return "i32"
  Str  -> return "i8*"
  Bool -> return "i1"
  Void -> return "void"
  Fun ftype atypes -> return $ show ftype


transExpr :: Expr -> SumMonad Val

transExpr (EVar ident) = do
  (tp, val) <- transIdent ident
  tname     <- transType tp
  lbl       <- nextLbl
  tell [lbl ++ " = load " ++ tname ++ ", " ++ tname ++ "* " ++ val]
  return (tp, lbl)

transExpr (ELitInt integer) =
  return (Int, show integer)

transExpr (ELitTrue) =
  return (Bool, "1")

transExpr (ELitFalse) =
  return (Bool, "0")

transExpr (EApp ident@(Ident name) exprs) = do
  ((Fun tp _), _)   <- transIdent ident
  tname             <- transType tp
  alist             <- transArgsExprs exprs
  let anames   = intercalate ", " alist
  let ellipsis = if (null alist) then " (...)" else ""
  let callFunc = "call " ++ tname ++ ellipsis ++ " @" ++ name ++
                 "(" ++ anames ++ ")"
  if (tp /= Void)
    then do
      lbl <- nextLbl
      tell [lbl ++ " = " ++ callFunc]
      return (tp, lbl)
    else do
      tell [callFunc]
      return (tp, "")

transExpr (EString string) =
  return (Str, string)

transExpr (Neg expr) =
  transExpr (EAdd (ELitInt 0) Minus expr)

transExpr (Not expr) =
  transExpr (ERel expr EQU ELitFalse)

transExpr (EMul expr1 mulop expr2) = do
  op  <- transMulOp mulop
  val <- transOp Int op expr1 expr2
  return (Int, val)

transExpr (EAdd expr1 addop expr2) = do
  op  <- transAddOp addop
  val <- transOp Int op expr1 expr2
  return (Int, val)

transExpr (ERel expr1 relop expr2) = do
  op  <- transRelOp relop
  val <- transOp Int op expr1 expr2
  return (Bool, val)

transExpr (EAnd expr1 expr2) = do
  val <- transOp Bool "and" expr1 expr2
  return (Bool, val)

transExpr (EOr expr1 expr2) = do
  val <- transOp Bool "or" expr1 expr2
  return (Bool, val)


transOp :: Type -> String -> Expr -> Expr -> SumMonad Lbl

transOp tp op expr1 expr2 = do
  (_, val1) <- transExpr expr1
  (_, val2) <- transExpr expr2
  tname     <- transType tp
  lbl       <- nextLbl
  tell [lbl ++ " = " ++ op ++ " " ++ tname ++ " " ++ val1 ++ ", " ++ val2]
  return lbl


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

pack out = prelude ++ out
  where
    prelude = [
      "declare void @printInt(i32)",
      "declare void @printString(i8*)",
      "declare i32 @readInt(...)",
      "declare i8* @readString(...)",
      "declare void @error(...)"]


run :: FilePath -> Program -> IO ()

run file prog = do
  let builtins = [("printInt", (Fun Void [Int], "@printInt")),
                  ("printString", (Fun Void [Str], "@printString")),
                  ("error", (Fun Void [], "@error")),
                  ("readInt", (Fun Int [], "@readInt")),
                  ("readString", (Fun Str [], "@readString"))]
  let state = (M.fromList builtins, 0)
  ((_, out), _) <- runStateT (runWriterT (transProgram prog)) state
  let code = intercalate "\n" $ pack out
  writeFile llPath code
  handle <- runCommand command
  void $ waitForProcess handle
    where
      dirPath = takeDirectory file
      llPath  = dropExtension file ++ ".ll"
      bcPath  = dropExtension file ++ ".bc"
      libPath = dirPath ++ "/*.bc"
      command = unwords ["llvm-as", llPath, "&& llvm-link -o", bcPath, libPath]


main :: IO ()

main = do
args <- getArgs
case args of
  []  -> hPutStr stderr "Error: No file provided\n"
  (file:_) -> do
  code <- readFile file
  case pProgram (myLexer code) of
    (Bad msg) -> hPutStr stderr $ "Error: " ++ msg ++ "\n"
    (Ok tree) -> run file tree
