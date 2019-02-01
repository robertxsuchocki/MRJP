module Main where

import AbsLatte
import LexLatte
import ParLatte
import TypeLatte
import ErrM

import Control.Monad.Writer
import Control.Monad.State

import Data.List
import Data.Map (Map, (!))
import Data.Maybe
import qualified Data.Map as M

import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Process

type Loc = Int
type Lbl = String
type Val = (Type, Lbl)
type Store = M.Map Lbl Val
type WSIO a = WriterT [String] (StateT (Store, Loc, Loc) IO) a


setLoc :: Loc -> WSIO ()

setLoc new = do
  modify (\(m, l, s) -> (m, new, s))


getLoc :: WSIO Loc

getLoc = do
  (store, loc, _) <- get
  let next = loc + 1
  modify (\(m, l, s) -> (m, next, s))
  return next


nextLbl :: WSIO Lbl

nextLbl = do
  (store, loc, _) <- get
  let next = loc + 1
  return $ "%" ++ show next


getLbl :: WSIO Lbl

getLbl = do
  (store, loc, _) <- get
  let next = loc + 1
  modify (\(m, l, s) -> (m, next, s))
  return $ "%" ++ show next


getStrLbl :: WSIO Lbl

getStrLbl = do
  (_, _, str) <- get
  let next = str + 1
  modify (\(m, l, s) -> (m, l, next))
  return $ "@.str." ++ show next


emptyString :: WSIO Lbl

emptyString = do
  lbl <- getLbl
  tell [lbl ++ " = bitcast [1 x i8]* @.str.0 to i8*"]
  return lbl


initValue :: Type -> WSIO String

initValue Str = do
  lbl <- emptyString
  return lbl

initValue tp = case tp of
  Int   -> return "0"
  Bool  -> return "false"
  Cls _ -> return "null"


retInitValue :: Type -> WSIO ()

retInitValue Str = do
  lbl <- emptyString
  tell ["ret i8* " ++ lbl]

retInitValue tp = case tp of
  Int  -> tell ["ret i32 0"]
  Bool -> tell ["ret i1 false"]
  Void -> tell ["ret void"]
  Arr etp -> do
    tell ["ret %struct.array* null"]
  Cls ident -> do
    tname <- transStoredType (Cls ident)
    tell ["ret " ++ tname ++ " null"]


transProgram :: Program -> WSIO ()

transProgram (Program tds) = do
  tellClsDefs tds
  declClsDefs 0 tds
  declFnDefs tds
  transTopDefs tds


argsTypes :: [Arg] -> [Type]

argsTypes [] = []

argsTypes ((Arg tp ident):args) = tp : (argsTypes args)


argsTypeNames :: [Arg] -> WSIO [Lbl]

argsTypeNames [] = return []

argsTypeNames ((Arg tp _):args) = do
  tname <- transStoredType tp
  rest  <- argsTypeNames args
  return (tname:rest)


fieldsTypeNames :: [Field] -> WSIO [Lbl]

fieldsTypeNames [] = return []

fieldsTypeNames ((Field tp idents):fs) = do
  tname <- transValType tp
  let tnames = replicate (length idents) tname
  rest  <- fieldsTypeNames fs
  return (tnames ++ rest)


declField :: Int -> Ident -> Field -> WSIO Int

declField lp _ (Field _ []) = return lp

declField lp (Ident cls) (Field tp ((Ident field):idents)) = do
  let name = (cls ++ "." ++ field)
  modify (\(m, l, s) -> ((M.insert name (tp, (show lp)) m), l, s))
  declField (lp + 1) (Ident cls) (Field tp idents)


tellClsDefs :: [TopDef] -> WSIO ()

tellClsDefs [] =
  return ()

tellClsDefs ((ClsDef ident fields):tds) = do
  flist <- fieldsTypeNames fields
  let fnames = intercalate ", " flist
  tname <- transValType (Cls ident)
  tell [(init tname) ++ " = type {" ++ fnames ++ "}"]

tellClsDefs (_:tds) =
  return ()


declClsDefs :: Int -> [TopDef] -> WSIO Int

declClsDefs lp [] =
  return lp

declClsDefs lp ((ClsDef ident@(Ident name) []):tds) = do
  let lbl = "%struct." ++ name
  modify (\(m, l, s) -> ((M.insert name ((Cls ident), lbl) m), l, s))
  new_lp <- declClsDefs lp tds
  return new_lp

declClsDefs lp ((ClsDef ident (f:fs)):tds) = do
  lp1 <- declField lp ident f
  lp2 <- declClsDefs lp1 ((ClsDef ident fs):tds)
  return lp2

declClsDefs lp (_:tds) = do
  new_lp <- declClsDefs lp tds
  return new_lp


declFnDefs :: [TopDef] -> WSIO ()

declFnDefs [] =
  return ()

declFnDefs ((FnDef tp (Ident name) args block):tds) = do
  let ftype = (Fun tp (argsTypes args))
  modify (\(m, l, s) -> ((M.insert name (ftype, name) m), l, s))
  declFnDefs tds

declFnDefs (_:tds) =
  declFnDefs tds


transTopDefs :: [TopDef] -> WSIO ()

transTopDefs [] =
  return ()

transTopDefs ((FnDef tp (Ident name) args block):tds) = do
  tname <- transStoredType tp
  alist <- argsTypeNames args
  let anames = intercalate ", " alist
  tell ["define " ++ tname ++ " @" ++ name ++ "(" ++ anames ++ ") {"]
  (store, _, _) <- get
  declArgs args
  transBlock block
  retInitValue tp
  modify (\(_, l, s) -> (store, l, s))
  tell ["}"]
  transTopDefs tds

transTopDefs (_:tds) =
  transTopDefs tds


declArgs :: [Arg] -> WSIO ()

declArgs (args) = do
  let len = length args
  setLoc len
  void $ mapM (declArg $ len + 1) args


declArg :: Int -> Arg -> WSIO ()

declArg gap (Arg tp ident@(Ident name)) = do
  tname <- transStoredType tp
  loc   <- getLoc
  let lbl = "%" ++ show loc
  let val = "%" ++ (show $ loc - gap)
  modify (\(m, l, s) -> ((M.insert name (tp, lbl) m), l, s))
  tell [lbl ++ " = alloca " ++ tname]
  tell ["store " ++ tname ++ " " ++ val ++ ", " ++ tname ++ "* " ++ lbl]


transArgs :: [Expr] -> WSIO [Lbl]

transArgs [] = return []

transArgs (expr:exprs) = do
  (tp, val) <- transExpr expr
  tname     <- transStoredType tp
  rest      <- transArgs exprs
  let name = tname ++ " " ++ val
  return (name:rest)


transIdent :: Ident -> WSIO Val

transIdent (Ident name) = do
  (store, _, _) <- get
  return $ fromMaybe (Void, "") (M.lookup name store)


transBlock :: Block -> WSIO ()

transBlock (Block stmts) = do
  (store, _, _) <- get
  void $ mapM transStmt stmts
  modify (\(_, l, s) -> (store, l, s))


transStmt :: Stmt -> WSIO ()

transStmt (Empty) =
  return ()

transStmt (BStmt block) =
  transBlock block

transStmt (Decl tp items) =
  void $ mapM (transItem tp) items

transStmt (Ass ident@(Ident name) expr) = do
  (tp, lbl) <- transIdent ident
  (_, val)  <- transExpr expr
  tname     <- transStoredType tp
  tell ["store " ++ tname ++ " " ++ val ++ ", " ++ tname ++ "* " ++ lbl]

transStmt (ArrAss ident expr1 expr2) = do
  (atp, _)   <- transIdent ident
  (_, loc)   <- transExpr (EField ident (Ident "arr"))
  (_, index) <- transExpr expr1
  (etp, val) <- transExpr expr2
  atname     <- transValType atp
  etname     <- transValType etp
  lbl        <- getLbl
  ptr        <- getLbl
  tell [lbl ++ " = bitcast i8* " ++ loc ++ " to " ++ atname]
  tell [ptr ++ " = getelementptr " ++ etname ++ ", " ++ atname ++ " " ++ lbl ++
        ", " ++ etname ++ " " ++ index]
  tell ["store " ++ etname ++ " " ++ val ++ ", " ++ etname ++ "* " ++ ptr]

transStmt (FieldAss (Ident name) (Ident field) expr) = do
  (ctp, clbl) <- transIdent (Ident name)
  let (Cls (Ident cls)) = ctp
  (ftp, flbl) <- transIdent (Ident (cls ++ "." ++ field))
  ctname      <- transStoredType ctp
  ftname      <- transValType ftp
  (_, val)    <- transExpr expr
  cls         <- getLbl
  ptr         <- getLbl
  tell [cls ++ " = load " ++ ctname ++ ", " ++ ctname ++ "* " ++ clbl]
  tell [ptr ++ " = getelementptr " ++ (init ctname) ++ ", " ++ ctname ++ " "
        ++ cls ++ ", i32 0, i32 " ++ flbl]
  tell ["store " ++ ftname ++ " " ++ val ++ ", " ++ ftname ++ "* " ++ ptr]

transStmt (Incr ident) =
  transStmt (Ass ident (EAdd (EVar ident) Plus (ELitInt 1)))

transStmt (Decr ident) =
  transStmt (Ass ident (EAdd (EVar ident) Minus (ELitInt 1)))

transStmt (Ret expr) = do
  (tp, val) <- transExpr expr
  tname     <- transStoredType tp
  void $ getLbl
  tell ["ret " ++ tname ++ " " ++ val]

transStmt (VRet) = do
  void $ getLbl
  tell ["ret void"]

transStmt (Cond expr stmt) = do
  (_, val) <- transExpr expr
  lbl <- nextLbl
  let (_:next) = lbl
  tell ["br i1 " ++ val ++ ", label %then" ++ next ++ ", label %end" ++ next]

  tell ["then" ++ next ++ ":"]
  transStmt stmt
  tell ["br label %end" ++ next]

  tell ["end" ++ next ++ ":"]

transStmt (CondElse expr stmt1 stmt2) = do
  (_, val) <- transExpr expr
  lbl <- nextLbl
  let (_:next) = lbl
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

transStmt (For tp (Ident var) (Ident arr) stmt) = do
  let iter = (Ident $ var ++ ".iterator")
  let len  = (EField (Ident arr) (Ident "length"))
  let cond = (ERel (EVar iter) LTH len)
  let set_x = (Ass (Ident var) (EValArr (Ident arr) (EVar iter)))
  let inc_i = (Incr iter)
  let block = (BStmt (Block [set_x, stmt, inc_i]))

  (store, _, _) <- get
  transItem tp (NoInit (Ident var))
  transItem tp (Init iter (ELitInt 0))
  transStmt (While cond block)
  modify (\(_, l, s) -> (store, l, s))

transStmt (SExp expr) =
  void $ transExpr expr


transItem :: Type -> Item -> WSIO ()

transItem tp (NoInit ident@(Ident name)) = do
  val   <- initValue tp
  tname <- transStoredType tp
  lbl   <- getLbl

  modify (\(m, l, s) -> ((M.insert name (tp, lbl) m), l, s))
  tell [lbl ++ " = alloca " ++ tname]
  tell ["store " ++ tname ++ " " ++ val ++ ", " ++ tname ++ "* " ++ lbl]

transItem tp (Init ident@(Ident name) expr) = do
  (_, val) <- transExpr expr
  tname    <- transStoredType tp
  lbl      <- getLbl

  modify (\(m, l, s) -> ((M.insert name (tp, lbl) m), l, s))
  tell [lbl ++ " = alloca " ++ tname]
  tell ["store " ++ tname ++ " " ++ val ++ ", " ++ tname ++ "* " ++ lbl]


transValType :: Type -> WSIO String

transValType tp = case tp of
  Int  -> return "i32"
  Str  -> return "i8*"
  Bool -> return "i1"
  Void -> return "void"
  Fun ftype atypes -> return $ show ftype
  Arr tp -> do
    tname <- transValType tp
    return $ tname ++ "*"
  Cls (Ident name) -> return $ "%struct." ++ name ++ "*"


transStoredType :: Type -> WSIO String

transStoredType tp = case tp of
  Int  -> return "i32"
  Str  -> return "i8*"
  Bool -> return "i1"
  Void -> return "void"
  Fun ftype atypes -> return $ show ftype
  Arr tp -> return "%struct.array*"
  Cls (Ident name) -> return $ "%struct." ++ name ++ "*"


transClsName :: Type -> WSIO String

transClsName tp = case tp of
  Cls (Ident name) -> return name
  Arr tp           -> return "array"


transExpr :: Expr -> WSIO Val

transExpr (EVar ident) = do
  (tp, val) <- transIdent ident
  tname     <- transStoredType tp
  lbl       <- getLbl
  tell [lbl ++ " = load " ++ tname ++ ", " ++ tname ++ "* " ++ val]
  return (tp, lbl)

transExpr (ELitInt integer) =
  return (Int, show integer)

transExpr (ELitTrue) =
  return (Bool, "true")

transExpr (ELitFalse) =
  return (Bool, "false")

transExpr (ENewObj tp) = do
  tname <- transStoredType tp
  loc   <- getLbl
  lbl   <- getLbl
  tell [loc ++ " = call noalias i8* @malloc(i32 8)"]
  tell [lbl ++ " = bitcast i8* " ++ loc ++ " to " ++ tname]
  return (tp, lbl)

transExpr (ENull ident) = do
  return ((Cls ident), "null")

transExpr (EField (Ident name) (Ident field)) = do
  (ctp, clbl) <- transIdent (Ident name)
  clsname     <- transClsName ctp
  (ftp, flbl) <- transIdent (Ident (clsname ++ "." ++ field))
  ctname      <- transStoredType ctp
  ftname      <- transValType ftp
  cls         <- getLbl
  ptr         <- getLbl
  val         <- getLbl
  tell [cls ++ " = load " ++ ctname ++ ", " ++ ctname ++ "* " ++ clbl]
  tell [ptr ++ " = getelementptr " ++ (init ctname) ++ ", " ++ ctname ++ " "
        ++ cls ++ ", i32 0, i32 " ++ flbl]
  tell [val ++ " = load " ++ ftname ++ ", " ++ ftname ++ "* " ++ ptr]
  return (ftp, val)

transExpr (EApp ident@(Ident name) exprs) = do
  ((Fun tp _), _)   <- transIdent ident
  tname             <- transStoredType tp
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

transExpr (ENewArr tp expr) = do
  (_, val) <- transExpr (ENewObj (Cls (Ident "array")))

  (_, num) <- transExpr expr
  size     <- getLbl
  tell [size ++ " = mul i32 " ++ num ++ ", 8"]

  tname    <- transValType tp
  lbl      <- getLbl
  tell [lbl ++ " = call noalias i8* @malloc(i32 " ++ size ++ ")"]

  arr      <- getLbl
  tell [arr ++ " = getelementptr %struct.array, %struct.array* " ++ val
        ++ ", i32 0, i32 0"]
  tell ["store i8* " ++ lbl ++ ", i8** " ++ arr]

  len      <- getLbl
  tell [len ++ " = getelementptr %struct.array, %struct.array* " ++ val
        ++ ", i32 0, i32 1"]
  tell ["store i32 " ++ num ++ ", i32* " ++ len]

  return ((Arr tp), val)

transExpr (EValArr ident expr) = do
  (atp, _)     <- transIdent ident
  (_, loc)     <- transExpr (EField ident (Ident "arr"))
  (etp, index) <- transExpr expr
  atname <- transValType atp
  etname <- transValType etp
  lbl    <- getLbl
  ptr    <- getLbl
  val    <- getLbl
  tell [lbl ++ " = bitcast i8* " ++ loc ++ " to " ++ atname]
  tell [ptr ++ " = getelementptr " ++ etname ++ ", " ++ atname ++ " " ++ lbl ++
        ", " ++ etname ++ " " ++ index]
  tell [val ++ " = load " ++ (init atname) ++ ", " ++ atname ++ " " ++ ptr]
  let (Arr tp) = atp
  return (tp, val)

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


transAdd :: Expr -> Expr -> WSIO Val

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


transOp :: String -> Expr -> Expr -> WSIO Lbl

transOp op expr1 expr2 = do
  (tp, val1) <- transExpr expr1
  (_, val2)  <- transExpr expr2
  tname      <- transValType tp
  lbl        <- getLbl
  tell [lbl ++ " = " ++ op ++ " " ++ tname ++ " " ++ val1 ++ ", " ++ val2]
  return lbl


transMulOp :: MulOp -> WSIO String

transMulOp mulop = case mulop of
  Times -> return "mul nsw"
  Div   -> return "sdiv"
  Mod   -> return "srem"


transRelOp :: RelOp -> WSIO String

transRelOp relop = case relop of
  LTH -> return "icmp slt"
  LE  -> return "icmp sle"
  GTH -> return "icmp sgt"
  GE  -> return "icmp sge"
  EQU -> return "icmp eq"
  NE  -> return "icmp ne"


pack :: [String] -> [String]

pack code = globals ++ c_functions ++ builtins ++ structs ++ code
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
    structs = ["%struct.array = type { i8*, i32 }"]


moveGlobals :: [String] -> [String]

moveGlobals code = globals ++ rest
  where
    is_const = isPrefixOf "@"
    is_struct = isPrefixOf "%struct."
    func = \x -> is_const x || is_struct x
    globals = filter (func) code
    rest = filter (\x -> not (func x)) code


run :: FilePath -> Program -> IO ()

run file prog = do
  let builtins = [("printInt", (Fun Void [Int], "@printInt")),
                  ("printString", (Fun Void [Str], "@printString")),
                  ("error", (Fun Void [], "@error")),
                  ("readInt", (Fun Int [], "@readInt")),
                  ("readString", (Fun Str [], "@readString")),
                  ("array", (Cls (Ident "array"), "%struct.array")),
                  ("array.arr", (Str, "0")),
                  ("array.length", (Int, "1"))]
  let state = (M.fromList builtins, 0, 0)
  ((_, out), _) <- runStateT (runWriterT (transProgram prog)) state
  let code = intercalate "\n" $ moveGlobals $ pack out
  writeFile llPath code
  currPath <- getCurrentDirectory
  let libPath = currPath ++ "/lib/*.bc"
  handle <- runCommand $
    unwords ["llvm-as", llPath, "&&", "llvm-link -o", bcPath, bcPath, libPath]
  void $ waitForProcess handle
    where
      dirPath = takeDirectory file
      llPath  = dropExtension file ++ ".ll"
      bcPath  = dropExtension file ++ ".bc"


main :: IO ()

main = do
args <- getArgs
case args of
  []  -> hPutStr stderr "ERROR\nNo file provided\n"
  (file:_) -> do
  code <- readFile file
  case pProgram (myLexer code) of
    (Bad msg) -> hPutStr stderr $ "ERROR\n" ++ msg ++ "\n"
    (Ok tree) -> do
      ((valid, msg), _) <- runStateT (runWriterT (validProgram tree)) M.empty
      if (not valid)
        then do
          let errors = intercalate "\n" msg
          hPutStr stderr $ "ERROR\n" ++ errors ++ "\nTyping failed\n"
          return ()
        else do
          hPutStr stderr "OK\n"
          run file tree
