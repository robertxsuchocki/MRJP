module TypeLatte where

import AbsLatte

import Control.Monad.Writer
import Control.Monad.State

import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Maybe

import System.IO

type TypeStore = M.Map String Type

type TypeWSIO a = WriterT [String] (StateT TypeStore IO) a


argTypes :: [Arg] -> [Type]

argTypes [] = []
argTypes ((Arg tp _):args) = tp : (argTypes args)


argsIdents :: [Arg] -> [Ident]

argsIdents [] = []
argsIdents ((Arg _ ident):args) = ident : (argsIdents args)


topDefsIdents :: [TopDef] -> [Ident]

topDefsIdents [] = []
topDefsIdents ((FnDef _ ident _ _):tds) = ident : (topDefsIdents tds)
topDefsIdents ((ClsDef ident _):tds) = ident : (topDefsIdents tds)


itemsIdents :: [Item] -> [Ident]

itemsIdents [] = []
itemsIdents ((NoInit ident):its) = ident : (itemsIdents its)
itemsIdents ((Init ident expr):its) = ident : (itemsIdents its)


fieldsIdents :: [Field] -> [Ident]

fieldsIdents [] = []
fieldsIdents ((Field _ idents):fs) = idents ++ (fieldsIdents fs)


stmtsIdents :: [Stmt] -> [Ident]

stmtsIdents [] = []
stmtsIdents ((Decl _ items):its) = (itemsIdents items) ++ (stmtsIdents its)
stmtsIdents (_:its) = stmtsIdents its


argsDecl :: [Arg] -> TypeWSIO ()

argsDecl [] = return ()

argsDecl ((Arg tp (Ident name)):args) = do
  modify (M.insert name tp)
  argsDecl args


fieldDecl :: Ident -> Field -> TypeWSIO ()

fieldDecl _ (Field _ []) = return ()

fieldDecl (Ident cls) (Field tp ((Ident field):idents)) = do
  modify (M.insert (cls ++ "." ++ field) tp)
  fieldDecl (Ident cls) (Field tp idents)


clsDecl :: TopDef -> TypeWSIO ()

clsDecl (ClsDef ident@(Ident name) []) = do
  modify (M.insert name (Cls ident))

clsDecl (ClsDef ident (f:fs)) = do
  fieldDecl ident f
  clsDecl (ClsDef ident fs)



uniqueIdents :: [Ident] -> [Ident] -> TypeWSIO Bool

uniqueIdents _ [] =
  return True

uniqueIdents seen (ident@(Ident name):idents) = do
  if (elem ident seen)
    then do
      tell ["Redeclaration of a variable or function " ++ (show name)]
      return False
    else do
      rest <- uniqueIdents (ident:seen) idents
      return rest


validArgs :: [Type] -> [Expr] -> TypeWSIO Bool

validArgs (tp:tps) (expr:exprs) = do
  valid <- validExpr tp expr
  if (not valid)
    then return False
    else validArgs tps exprs

validArgs [] [] = return True

validArgs _ _ = return False


validProgram :: Program -> TypeWSIO Bool

validProgram (Program tds) = do
  addBuiltins
  typeTopDefs tds
  valid  <- validTopDefs tds
  unique <- uniqueIdents [] $ topDefsIdents tds
  main   <- validExpr Int (EApp (Ident "main") [])

  when (not main) $ tell ["No valid main function"]

  return $ valid && unique && main


addBuiltins :: TypeWSIO ()
addBuiltins = do
  let builtins = [("printInt", Fun Void [Int]),
                  ("printString", Fun Void [Str]),
                  ("error", Fun Void []),
                  ("readInt", Fun Int []),
                  ("readString", Fun Str [])]
  modify (\_ -> M.fromList builtins)


typeTopDefs :: [TopDef] -> TypeWSIO ()

typeTopDefs [] =
  return ()

typeTopDefs ((FnDef tp (Ident name) args block):tds) = do
  let ftype = (Fun tp (argTypes args))
  modify (M.insert name ftype)
  typeTopDefs tds

typeTopDefs ((ClsDef _ _):tds) =
  typeTopDefs tds


validTopDefs :: [TopDef] -> TypeWSIO Bool

validTopDefs [] =
  return True

validTopDefs (td:tds) = do
  valid <- validTopDef td
  if not valid
    then do
      tell ["Fail in declaration of " ++ (show td)]
      return False
    else validTopDefs tds


validTopDef :: TopDef -> TypeWSIO Bool

validTopDef cls@(ClsDef (Ident name) fields) = do
  unique <- uniqueIdents [] $ fieldsIdents fields
  clsDecl cls
  return unique

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

  when (not valid_r && tp /= Void) $
    tell ["No valid return in function " ++ name]

  when (not valid_a) $
    tell ["Repetitive arguments for function " ++ name]

  when (not valid_v) $
    tell ["Void arguments in function " ++ name]

  return (valid_b && (valid_r || tp == Void) && valid_a && valid_v)


validBlock :: Block -> TypeWSIO Bool

validBlock (Block stmts) = do
  valid_d <- uniqueIdents [] (stmtsIdents stmts)
  valid_s <- validStmts stmts
  when (not $ valid_d && valid_s) $
    tell ["Fail in block " ++ (show stmts)]
  return $ valid_d && valid_s


validStmts :: [Stmt] -> TypeWSIO Bool

validStmts (s:ss) = do
  valid <- validStmt s
  if (valid)
    then validStmts ss
    else do
      tell ["Fail in statement " ++ (show s)]
      return False

validStmts [] =
  return True


validIdent :: Ident -> Type -> TypeWSIO Bool

validIdent (Ident name) tp = do
  store <- get
  case M.lookup name store of
    Just t -> return (t == tp)
    _      -> return False


getRelOpVal :: RelOp -> Bool -> Bool -> Bool

getRelOpVal relop bool1 bool2 = case relop of
  LTH -> not bool1 && bool2
  LE  -> not bool1 || bool2
  GTH -> bool1 && not bool2
  GE  -> bool1 || not bool2
  EQU -> bool1 == bool2
  NE  -> bool1 /= bool2


isELitBool :: Expr -> Maybe Bool

isELitBool (ELitTrue) = Just True

isELitBool (ELitFalse) = Just False

isELitBool (Not expr) =
  case isELitBool expr of
    Just bool -> Just (not bool)
    Nothing   -> Nothing

isELitBool (ERel expr1 relop expr2) =
  case (isELitBool expr1, isELitBool expr2) of
    (Just bool1, Just bool2) -> Just (getRelOpVal relop bool1 bool2)
    _                        -> Nothing

isELitBool (EAnd expr1 expr2) =
  case (isELitBool expr1, isELitBool expr2) of
    (Just bool1, Just bool2) -> Just (bool1 && bool2)
    _                        -> Nothing

isELitBool (EOr expr1 expr2) =
  case (isELitBool expr1, isELitBool expr2) of
    (Just bool1, Just bool2) -> Just (bool1 || bool2)
    _                        -> Nothing

isELitBool _ = Nothing


hasReturnClause :: [Stmt] -> TypeWSIO Bool

hasReturnClause [] =
  return False

hasReturnClause ((BStmt (Block bss)):ss) =
  hasReturnClause (bss ++ ss)

hasReturnClause ((Cond expr stmt):ss) = do
  let truthy = (isELitBool expr == Just True)
  valid <- hasReturnClause [stmt]
  if (truthy && valid)
    then return True
    else hasReturnClause ss

hasReturnClause ((CondElse expr stmt1 stmt2):ss) = do
  let truthy = (isELitBool expr == Just True)
  let falsy  = (isELitBool expr == Just False)
  valid1 <- hasReturnClause [stmt1]
  valid2 <- hasReturnClause [stmt2]
  if (valid1 && valid2 || truthy && valid1 || falsy && valid2)
    then return True
    else hasReturnClause ss

hasReturnClause ((While expr stmt):ss) = do
  let truthy = (isELitBool expr == Just True)
  valid <- hasReturnClause [stmt]
  if (truthy && valid)
    then return True
    else hasReturnClause ss

hasReturnClause ((For _ _ _ stmt):ss) = do
  valid <- hasReturnClause [stmt]
  if (valid)
    then return True
    else hasReturnClause ss

hasReturnClause (s:ss) = case s of
  Ret _    -> return True
  VRet     -> return True
  _        -> hasReturnClause ss


validItems :: Type -> [Item] -> TypeWSIO Bool

validItems _ [] =
  return True

validItems tp (it:its) = do
  valid <- validItem tp it
  if not valid
    then do
      tell ["Fail in declaration of " ++ (show it)]
      return False
    else validItems tp its


validItem :: Type -> Item -> TypeWSIO Bool

validItem tp (NoInit (Ident name)) = do
  modify (M.insert name tp)
  return $ tp /= Void

validItem tp (Init (Ident name) expr) = do
  valid <- validExpr tp expr
  modify (M.insert name tp)
  return $ valid && tp /= Void


validStmt :: Stmt -> TypeWSIO Bool

validStmt Empty =
  return True

validStmt (BStmt block) =
  validBlock block

validStmt (Decl tp items) = do
  valid <- validItems tp items
  return valid

validStmt (Ass (Ident name) expr) = do
  store <- get
  case M.lookup name store of
    Just tp -> validExpr tp expr
    _       -> return False

validStmt (ArrAss (Ident name) expr1 expr2) = do
  store <- get
  case M.lookup name store of
    Just (Arr tp) ->
      validExprsAll [(Int, expr1), (tp, expr2)]
    _                -> return False

validStmt (FieldAss (Ident name) (Ident field) expr) = do
  store <- get
  case M.lookup name store of
    Just (Cls (Ident cls)) -> do
      case M.lookup (cls ++ "." ++ field) store of
        Just t -> do
          valid <- validExpr t expr
          return valid
        _      -> return False
    _ -> return False

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

validStmt (For tp (Ident name1) ident2 stmt) = do
  valid_i <- validIdent ident2 (Arr tp)
  store  <- get
  modify (M.insert name1 tp)
  valid_s <- validStmt stmt
  modify (\_ -> store)
  return (valid_i && valid_s)

validStmt (SExp expr) =
  validExprsAny [Int, Bool, Str, Void] expr


validExprsAll :: [(Type, Expr)] -> TypeWSIO Bool

validExprsAll [] =
  return True

validExprsAll ((tp, expr):pairs) = do
  valid <- validExpr tp expr
  if not valid
    then return False
    else validExprsAll pairs


validExprsAny :: [Type] -> Expr -> TypeWSIO Bool

validExprsAny [] expr =
  return False

validExprsAny (tp:tps) expr = do
  valid <- validExpr tp expr
  if valid
    then return True
    else validExprsAny tps expr


identTypeFromExpr :: Expr -> TypeWSIO (Maybe Type)

identTypeFromExpr (EVar (Ident name)) = do
  store <- get
  return $ M.lookup name store

identTypeFromExpr (ENull ident) =
  return $ Just (Cls ident)

identTypeFromExpr _ =
  return Nothing


validClsExprs :: Expr -> Expr -> TypeWSIO Bool

validClsExprs expr1 expr2 = do
  tp1 <- identTypeFromExpr expr1
  case tp1 of
    (Just t) -> do
      tp2 <- identTypeFromExpr expr2
      return $ tp1 == tp2
    _ -> return False


validExpr :: Type -> Expr -> TypeWSIO Bool

validExpr tp (EVar ident) =
  validIdent ident tp

validExpr tp (ENull (Ident name)) = do
  store <- get
  case M.lookup name store of
    Just t -> return (t == tp)
    _      -> return False

validExpr Int (ELitInt integer) =
  return True

validExpr Bool (ELitTrue) =
  return True

validExpr Bool (ELitFalse) =
  return True

validExpr tp (EApp (Ident name) exprs) = do
  store <- get
  case M.lookup name store of
    Just (Fun t args) -> do
      valid_a <- validArgs args exprs
      return (t == tp && valid_a)
    _                 -> return False

validExpr Str (EString string) =
  return True

validExpr Int (EField (Ident name1) (Ident "length")) = do
  store <- get
  case M.lookup name1 store of
    Just (Arr _) -> return True
    _            -> return False

validExpr tp (EField (Ident name) (Ident field)) = do
  store <- get
  case M.lookup name store of
    Just (Cls (Ident cls)) -> do
      case M.lookup (cls ++ "." ++ field) store of
        Just t -> return (t == tp)
        _      -> return False
    _ -> return False

validExpr tp1 (ENewObj tp2) =
  return $ tp1 == tp2

validExpr (Arr tp1) (ENewArr tp2 expr) = do
  len <- validExpr Int expr
  return $ (tp1 == tp2) && len

validExpr tp (EValArr (Ident name) expr) = do
  store <- get
  case M.lookup name store of
    Just (Arr t) -> do
      valid <- validExpr Int expr
      return (t == tp && valid)
    _            -> return False

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
  int  <- validExprsAll [(Int, expr1), (Int, expr2)]
  bool <- validExprsAll [(Bool, expr1), (Bool, expr2)]
  let op = (relop == EQU || relop == NE)
  cls  <- validClsExprs expr1 expr2
  return $ int || bool || (op && cls)

validExpr Bool (EAnd expr1 expr2) =
  validExprsAll [(Bool, expr1), (Bool, expr2)]

validExpr Bool (EOr expr1 expr2) =
  validExprsAll [(Bool, expr1), (Bool, expr2)]

validExpr _ _ = return False
