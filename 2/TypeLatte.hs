module TypeLatte where

import AbsLatte

import Control.Monad.State

import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Maybe

import System.IO

type TypeStore = M.Map String Type

type SIO a = (StateT TypeStore IO) a


argsDecl :: [Arg] -> SIO ()

argsDecl [] = return ()

argsDecl ((Arg tp (Ident name)):args) = do
  modify (M.insert name tp)
  argsDecl args


argTypes :: [Arg] -> [Type]

argTypes [] = []

argTypes ((Arg tp _):args) = tp : (argTypes args)


argsIdents :: [Arg] -> [Ident]

argsIdents [] = []

argsIdents ((Arg _ ident):args) = ident : (argsIdents args)


validArgs :: [Type] -> [Expr] -> SIO Bool

validArgs (tp:types) (expr:exprs) = do
  valid <- validExpr tp expr
  if (not valid)
    then return False
    else validArgs types exprs

validArgs [] [] = return True

validArgs _ _ = return False


validProgram :: Program -> SIO Bool

validProgram (Program tds) = do
  addBuiltins
  typeTopDefs tds
  valids <- mapM validTopDef tds
  return (all (== True) valids)


addBuiltins :: SIO ()
addBuiltins = do
  let builtins = [("printInt", Fun Void [Int]),
                  ("printString", Fun Void [Str]),
                  ("error", Fun Void []),
                  ("readInt", Fun Int []),
                  ("readString", Fun Str [])]
  modify (\_ -> M.fromList builtins)


typeTopDefs :: [TopDef] -> SIO ()

typeTopDefs [] =
  return ()

typeTopDefs ((FnDef tp (Ident name) args block):tds) = do
  let ftype = (Fun tp (argTypes args))
  modify (M.insert name ftype)
  typeTopDefs tds


validTopDef :: TopDef -> SIO Bool

validTopDef (FnDef tp (Ident name) args block) = do
  let idents = argsIdents args
  let Block stmts = block

  store <- get

  argsDecl args
  modify (M.insert "return" tp)
  valid_b <- validBlock block

  modify (\_ -> store)

  valid_r <- hasReturnClause stmts
  let valid_a = idents == (nub idents)
  let valid_v = all (/= Void) (argTypes args)

  return (valid_b && (valid_r || tp == Void) && valid_a && valid_v)


validBlock :: Block -> SIO Bool

validBlock (Block (s:ss)) = do
  valid <- validStmt s
  if (valid)
    then validBlock (Block ss)
    else do
      liftIO $ hPutStr stderr $ "Error: Typing failed in statement " ++
        (show s) ++ "\n"
      return False

validBlock (Block []) =
  return True


validIdent :: Ident -> Type -> SIO Bool

validIdent (Ident name) tp = do
  store <- get
  case M.lookup name store of
    Just t -> return (t == tp)
    _      -> return False


hasReturnClause :: [Stmt] -> SIO Bool

hasReturnClause [] =
  return False

hasReturnClause ((BStmt (Block bss)):ss) =
  hasReturnClause (bss ++ ss)

hasReturnClause ((While _ stmt):ss) =
  hasReturnClause (stmt:ss)

hasReturnClause ((CondElse expr stmt1 stmt2):ss) = do
  valid1 <- hasReturnClause [stmt1]
  valid2 <- hasReturnClause [stmt2]
  if (valid1 && valid2)
    then return True
    else hasReturnClause ss

hasReturnClause (s:ss) = case s of
  Ret _    -> return True
  VRet     -> return True
  _        -> hasReturnClause ss


validItem :: Type -> Item -> SIO Bool

validItem tp (NoInit (Ident name)) = do
  modify (M.insert name tp)
  return $ tp /= Void

validItem tp (Init (Ident name) expr) = do
  valid <- validExpr tp expr
  modify (M.insert name tp)
  return $ valid && tp /= Void


validStmt :: Stmt -> SIO Bool

validStmt Empty =
  return True

validStmt (BStmt block) =
  validBlock block

validStmt (Decl tp items) = do
  valids <- mapM (validItem tp) items
  return (all (== True) valids)

validStmt (Ass (Ident name) expr) = do
  store <- get
  case M.lookup name store of
    Just type' -> validExpr type' expr
    _          -> return False

validStmt (Incr ident) =
  validIdent ident Int

validStmt (Decr ident) =
  validIdent ident Int

validStmt (Ret expr) =
  validStmt (Ass (Ident "return") expr)

validStmt (VRet) =
  validIdent (Ident "return") Void

validStmt (Cond expr stmt) = do
  valid_e <- validExpr Bool expr
  valid_s <- validStmt stmt
  return (valid_e && valid_s)

validStmt (CondElse expr stmt1 stmt2) = do
  valid_e  <- validExpr Bool expr
  valid_s1 <- validStmt stmt1
  valid_s2 <- validStmt stmt2
  return (valid_e && valid_s1 && valid_s2)

validStmt (While expr stmt) = do
  valid_e <- validExpr Bool expr
  valid_s <- validStmt stmt
  return (valid_e && valid_s)

validStmt (SExp expr) =
  validExprsAny [Int, Bool, Str, Void] expr


validExprsAll :: [(Type, Expr)] -> SIO Bool

validExprsAll [] =
  return True

validExprsAll ((tp, expr):pairs) = do
  valid <- validExpr tp expr
  if not valid
    then return False
    else validExprsAll pairs


validExprsAny :: [Type] -> Expr -> SIO Bool

validExprsAny [] expr =
  return False

validExprsAny (tp:types) expr = do
  valid <- validExpr tp expr
  if valid
    then return True
    else validExprsAny types expr


validExpr :: Type -> Expr -> SIO Bool

validExpr tp (EVar ident) =
  validIdent ident tp

validExpr Int (ELitInt integer) =
  return True

validExpr Bool (ELitTrue) =
  return True

validExpr Bool (ELitFalse) =
  return True

validExpr Str (EString string) =
  return True

validExpr Int (Neg expr) =
  validExpr Int expr

validExpr Bool (Not expr) =
  validExpr Bool expr

validExpr Str (EAdd expr1 Plus expr2) =
  validExprsAll [(Str, expr1), (Str, expr2)]

validExpr Int (EAdd expr1 addop expr2) =
  validExprsAll [(Int, expr1), (Int, expr2)]

validExpr Int (EMul expr1 mulop expr2) =
  validExprsAll [(Int, expr1), (Int, expr2)]

validExpr Bool (ERel expr1 relop expr2) = do
  int <- validExprsAll [(Int, expr1), (Int, expr2)]
  bool <- validExprsAll [(Bool, expr1), (Bool, expr2)]
  return $ int || bool

validExpr Bool (EAnd expr1 expr2) =
  validExprsAll [(Bool, expr1), (Bool, expr2)]

validExpr Bool (EOr expr1 expr2) =
  validExprsAll [(Bool, expr1), (Bool, expr2)]

validExpr tp (EApp (Ident name) exprs) = do
  store <- get
  case M.lookup name store of
    Just (Fun t args) -> do
      valid_a <- validArgs args exprs
      return (t == tp && valid_a)
    _                 -> return False

validExpr _ _ = return False
