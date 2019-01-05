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
type SumMonad a = WriterT [String] (StateT (Store, Loc, Loc) IO) a


setLoc :: Loc -> SumMonad ()

setLoc new = do
  modify (\(m, l, s) -> (m, new, s))


getLoc :: SumMonad Loc

getLoc = do
  (store, loc, _) <- get
  let next = loc + 1
  modify (\(m, l, s) -> (m, next, s))
  return next


nextLbl :: SumMonad Lbl

nextLbl = do
  (store, loc, _) <- get
  let next = loc + 1
  return $ "%" ++ show next


getLbl :: SumMonad Lbl

getLbl = do
  (store, loc, _) <- get
  let next = loc + 1
  modify (\(m, l, s) -> (m, next, s))
  return $ "%" ++ show next


getStrLbl :: SumMonad Lbl

getStrLbl = do
  (_, _, str) <- get
  let next = str + 1
  modify (\(m, l, s) -> (m, l, next))
  return $ "@.str." ++ show next


emptyString :: SumMonad Lbl

emptyString = do
  lbl <- getLbl
  tell [lbl ++ " = bitcast [1 x i8]* @.str.0 to i8*"]
  return lbl


initValue :: Type -> SumMonad String

initValue Str = do
  lbl <- emptyString
  return lbl

initValue tp = case tp of
  Int  -> return "0"
  Bool -> return "false"


retInitValue :: Type -> SumMonad String

retInitValue Str = do
  lbl <- emptyString
  return $ "ret i8* " ++ lbl

retInitValue tp = case tp of
  Int  -> return "ret i32 0"
  Bool -> return "ret i1 false"
  Void -> return "ret void"


transProgram :: Program -> SumMonad ()

transProgram (Program tds) = do
  declTopDefs tds
  transTopDefs tds


argsTypes :: [Arg] -> [Type]

argsTypes [] = []

argsTypes ((Arg tp ident):args) = tp : (argsTypes args)


argsTypeNames :: [Arg] -> SumMonad [Lbl]

argsTypeNames [] = return []

argsTypeNames ((Arg tp (Ident name)):args) = do
  tname <- transType tp
  rest  <- argsTypeNames args
  return (tname:rest)


declTopDefs :: [TopDef] -> SumMonad ()

declTopDefs [] =
  return ()

declTopDefs ((FnDef tp (Ident name) args block):tds) = do
  let ftype = (Fun tp (argsTypes args))
  modify (\(m, l, s) -> ((M.insert name (ftype, name) m), l, s))
  declTopDefs tds


transTopDefs :: [TopDef] -> SumMonad ()

transTopDefs [] =
  return ()

transTopDefs ((FnDef tp (Ident name) args block):tds) = do
  tname <- transType tp
  alist <- argsTypeNames args
  let anames = intercalate ", " alist
  tell ["define " ++ tname ++ " @" ++ name ++ "(" ++ anames ++ ") {"]
  declArgs args
  transBlock block
  guard <- retInitValue tp
  tell [guard]
  tell ["}"]
  transTopDefs tds


declArgs :: [Arg] -> SumMonad ()

declArgs (args) = do
  let len = length args
  setLoc len
  void $ mapM (declArg $ len + 1) args


declArg :: Int -> Arg -> SumMonad ()

declArg gap (Arg tp ident@(Ident name)) = do
  tname <- transType tp
  loc   <- getLoc
  let lbl = "%" ++ show loc
  let val = "%" ++ (show $ loc - gap)
  modify (\(m, l, s) -> ((M.insert name (tp, lbl) m), l, s))
  tell [lbl ++ " = alloca " ++ tname]
  tell ["store " ++ tname ++ " " ++ val ++ ", " ++ tname ++ "* " ++ lbl]


transArgs :: [Expr] -> SumMonad [Lbl]

transArgs [] = return []

transArgs (expr:exprs) = do
  (tp, val) <- transExpr expr
  tname     <- transType tp
  rest      <- transArgs exprs
  let name = tname ++ " " ++ val
  return (name:rest)


transIdent :: Ident -> SumMonad Val

transIdent (Ident name) = do
  (store, _, _) <- get
  return $ fromMaybe (Void, "") (M.lookup name store)


transBlock :: Block -> SumMonad ()

transBlock (Block stmts) = do
  (store, _, _) <- get
  void $ mapM transStmt stmts
  modify (\(_, l, s) -> (store, l, s))


transStmt :: Stmt -> SumMonad ()

transStmt (Empty) =
  return ()

transStmt (BStmt block) =
  transBlock block

transStmt (Decl tp items) =
  void $ mapM (transItem tp) items

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
  void $ getLbl
  tell ["ret " ++ tname ++ " " ++ val]

transStmt (VRet) = do
  void $ getLbl
  tell ["ret void"]

transStmt (Cond expr stmt) = do
  lbl <- nextLbl
  let (_:next) = lbl
  (_, val) <- transExpr expr
  tell ["br i1 " ++ val ++ ", label %then" ++ next ++ ", label %end" ++ next]

  tell ["then" ++ next ++ ":"]
  transStmt stmt
  tell ["br label %end" ++ next]

  tell ["end" ++ next ++ ":"]

transStmt (CondElse expr stmt1 stmt2) = do
  lbl <- nextLbl
  let (_:next) = lbl
  (_, val) <- transExpr expr
  tell ["br i1 " ++ val ++ ", label %then" ++ next ++ ", label %else" ++ next]

  tell ["then" ++ next ++ ":"]
  transStmt stmt1
  tell ["br label %end" ++ next]

  tell ["else" ++ next ++ ":"]
  transStmt stmt2
  tell ["br label %end" ++ next]

  tell ["end" ++ next ++ ":"]

transStmt (While expr stmt) = do
  lbl <- nextLbl
  let (_:next) = lbl
  tell ["br label %cond" ++ next]

  tell ["cond" ++ next ++ ":"]
  (_, val) <- transExpr expr
  tell ["br i1 " ++ val ++ ", label %loop" ++ next ++ ", label %end" ++ next]

  tell ["loop" ++ next ++ ":"]
  transStmt stmt
  tell ["br label %cond" ++ next]

  tell ["end" ++ next ++ ":"]

transStmt (SExp expr) =
  void $ transExpr expr


transItem :: Type -> Item -> SumMonad ()

transItem tp (NoInit ident@(Ident name)) = do
  val   <- initValue tp
  tname <- transType tp
  lbl   <- getLbl

  modify (\(m, l, s) -> ((M.insert name (tp, lbl) m), l, s))
  tell [lbl ++ " = alloca " ++ tname]
  tell ["store " ++ tname ++ " " ++ val ++ ", " ++ tname ++ "* " ++ lbl]

transItem tp (Init ident@(Ident name) expr) = do
  (_, val) <- transExpr expr
  tname    <- transType tp
  lbl      <- getLbl

  modify (\(m, l, s) -> ((M.insert name (tp, lbl) m), l, s))
  tell [lbl ++ " = alloca " ++ tname]
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
  lbl       <- getLbl
  tell [lbl ++ " = load " ++ tname ++ ", " ++ tname ++ "* " ++ val]
  return (tp, lbl)

transExpr (ELitInt integer) =
  return (Int, show integer)

transExpr (ELitTrue) =
  return (Bool, "true")

transExpr (ELitFalse) =
  return (Bool, "false")

transExpr (EApp ident@(Ident name) exprs) = do
  ((Fun tp _), _)   <- transIdent ident
  tname             <- transType tp
  alist             <- transArgs exprs
  let anames   = intercalate ", " alist
  let callFunc = "call " ++ tname ++ " @" ++ name ++ "(" ++ anames ++ ")"
  if (tp /= Void)
    then do
      lbl <- getLbl
      tell [lbl ++ " = " ++ callFunc]
      return (tp, lbl)
    else do
      tell [callFunc]
      return (tp, "")

transExpr (EString string) = do
  lbl <- getStrLbl
  let len = show $ (length string) + 1
  tell [lbl ++ " = constant [" ++ len ++ " x i8] c\"" ++ string ++ "\00\""]
  let loc = "%" ++ (drop 1 lbl)
  tell [loc ++ " = bitcast [" ++ len ++ " x i8]* " ++ lbl ++ " to i8*"]
  return (Str, loc)

transExpr (Neg expr) =
  transExpr (EAdd (ELitInt 0) Minus expr)

transExpr (Not expr) =
  transExpr (ERel expr EQU ELitFalse)

transExpr (EAdd expr1 Plus expr2) =
  transAdd expr1 expr2

transExpr (EAdd expr1 Minus expr2) = do
  val <- transOp "sub nsw" expr1 expr2
  return (Int, val)

transExpr (EMul expr1 mulop expr2) = do
  op  <- transMulOp mulop
  val <- transOp op expr1 expr2
  return (Int, val)

transExpr (ERel expr1 relop expr2) = do
  op  <- transRelOp relop
  val <- transOp op expr1 expr2
  return (Bool, val)

transExpr (EAnd expr1 expr2) = do
  lbl <- getLbl
  let (_:next) = lbl
  tell [lbl ++ " = alloca i1"]
  tell ["br label %first" ++ next]

  tell ["first" ++ next ++ ":"]
  (_, val1) <- transExpr expr1
  tell ["store i1 " ++ val1 ++ ", i1* " ++ lbl]
  tell ["br i1 " ++ val1 ++ ", label %second" ++ next ++ ", label %res" ++ next]

  tell ["second" ++ next ++ ":"]
  (_, val2) <- transExpr expr2
  tell ["store i1 " ++ val2 ++ ", i1* " ++ lbl]
  tell ["br label %res" ++ next]

  tell ["res" ++ next ++ ":"]
  val <- getLbl
  tell [val ++ " = load i1, i1* " ++ lbl]
  return (Bool, val)

transExpr (EOr expr1 expr2) = do
  lbl <- getLbl
  let (_:next) = lbl
  tell [lbl ++ " = alloca i1"]
  tell ["br label %first" ++ next]

  tell ["first" ++ next ++ ":"]
  (_, val1) <- transExpr expr1
  tell ["store i1 " ++ val1 ++ ", i1* " ++ lbl]
  tell ["br i1 " ++ val1 ++ ", label %res" ++ next ++ ", label %second" ++ next]

  tell ["second" ++ next ++ ":"]
  (_, val2) <- transExpr expr2
  tell ["store i1 " ++ val2 ++ ", i1* " ++ lbl]
  tell ["br label %res" ++ next]

  tell ["res" ++ next ++ ":"]
  val <- getLbl
  tell [val ++ " = load i1, i1* " ++ lbl]
  return (Bool, val)


transAdd :: Expr -> Expr -> SumMonad Val

transAdd expr1 expr2 = do
  (tp, val1) <- transExpr expr1
  (_, val2)  <- transExpr expr2
  case tp of
    Int -> do
      lbl   <- getLbl
      tell [lbl ++ " = add nsw i32 " ++ val1 ++ ", " ++ val2]
      return (tp, lbl)
    Str -> do
      l1 <- getLbl
      tell [l1 ++ " = call i32 @strlen(i8* " ++ val1 ++ ")"]
      l2 <- getLbl
      tell [l2 ++ " = call i32 @strlen(i8* " ++ val2 ++ ")"]
      l3 <- getLbl
      tell [l3 ++ " = add i32 " ++ l1 ++ ", 1"]
      l4 <- getLbl
      tell [l4 ++ " = add i32 " ++ l3 ++ ", " ++ l2]
      l5 <- getLbl
      tell [l5 ++ " = call i8* @malloc(i32 " ++ l4 ++ ")"]
      l6 <- getLbl
      tell [l6 ++ " = call i8* @strcpy(i8* " ++ l5 ++ ", i8* " ++ val1 ++ ")"]
      val <- getLbl
      tell [val ++ " = call i8* @strcat(i8* " ++ l6 ++ ", i8* " ++ val2 ++ ")"]
      return (tp, val)


transOp :: String -> Expr -> Expr -> SumMonad Lbl

transOp op expr1 expr2 = do
  (tp, val1) <- transExpr expr1
  (_, val2)  <- transExpr expr2
  tname      <- transType tp
  lbl        <- getLbl
  tell [lbl ++ " = " ++ op ++ " " ++ tname ++ " " ++ val1 ++ ", " ++ val2]
  return lbl


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

pack code = globals ++ c_functions ++ builtins ++ code
  where
    globals = ["@.str.0 = constant [1 x i8] zeroinitializer"]
    c_functions = [
      "declare i8* @malloc(i32)",
      "declare i8* @strcat(i8*, i8*)",
      "declare i8* @strcpy(i8*, i8*)",
      "declare i32 @strlen(i8*)"]
    builtins = [
      "declare void @printInt(i32)",
      "declare void @printString(i8*)",
      "declare i32 @readInt()",
      "declare i8* @readString()",
      "declare void @error()"]


moveGlobals :: [String] -> [String]

moveGlobals code = globals ++ rest
  where
    globals = filter (isPrefixOf "@") code
    rest = filter (\x -> not $ isPrefixOf "@" x) code


run :: FilePath -> Program -> IO ()

run file prog = do
  let builtins = [("printInt", (Fun Void [Int], "@printInt")),
                  ("printString", (Fun Void [Str], "@printString")),
                  ("error", (Fun Void [], "@error")),
                  ("readInt", (Fun Int [], "@readInt")),
                  ("readString", (Fun Str [], "@readString"))]
  let state = (M.fromList builtins, 0, 0)
  ((_, out), _) <- runStateT (runWriterT (transProgram prog)) state
  let code = intercalate "\n" $ moveGlobals $ pack out
  writeFile llPath code
  handle <- runCommand command
  void $ waitForProcess handle
    where
      dirPath = takeDirectory file
      llPath  = dropExtension file ++ ".ll"
      bcPath  = dropExtension file ++ ".bc"
      libPath = dirPath ++ "/lib/*.bc"
      command = unwords ["llvm-as", llPath, "&&",
                         "llvm-link -o", bcPath, bcPath, libPath]


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
