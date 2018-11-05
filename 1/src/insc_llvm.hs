module Main where

import AbsInstant
import LexInstant
import ParInstant
import ErrM

import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.State

import Data.List
import Data.Map (Map, (!))
import Data.Maybe
import qualified Data.Map as M

import System.Environment
import System.FilePath
import System.IO
import System.Process

type Label = String
type Loc = Int
type Store = M.Map Label Int
type WS a = WriterT [String] (StateT (Store, Int) IO) a


getStore :: WS Store
getStore = do
  (store, loc) <- get
  return store


nextLabel :: WS Label
nextLabel = do
  loc <- nextLocation
  return $ "%" ++ show loc


nextLocation :: WS Loc
nextLocation = do
  (store, loc) <- get
  let new = loc + 1
  modify $ (\(m, l) -> (m, new))
  return new


transIdent :: Ident -> WS Loc

transIdent (Ident name) = do
  store <- getStore
  let loc = fromMaybe 0 (M.lookup name store)
  if loc /= 0
    then return loc
    else do
      new <- nextLocation
      tell ["%" ++ show new ++ " = alloca i32, align 4"]
      return new


transProgram :: Program -> WS ()

transProgram (Prog []) = return ()

transProgram (Prog (s:ss)) = do
  transStmt s
  transProgram $ Prog ss


transStmt :: Stmt -> WS ()

transStmt (SAss ident@(Ident name) exp) = do
  label <- transExp exp
  loc   <- transIdent ident
  modify (\(m, l) -> ((M.insert name loc m), l))
  tell $ ["store i32 " ++ label ++ ", i32* %" ++ show loc ++ ", align 4"]

transStmt (SExp exp) = do
  label <- transExp exp
  next <- nextLabel
  tell $ [next ++ " = call i32 (i8*, ...) @printf(i8* getelementptr inbounds \
          \([4 x i8], [4 x i8]* @.str, i32 0, i32 0), i32 " ++ label ++ ")"]


transExp :: Exp -> WS Label

transExp (ExpLit integer) = return $ show integer

transExp (ExpVar ident) = do
  loc  <- transIdent ident
  next <- nextLabel
  tell [next ++ " = load i32, i32* %" ++ show loc ++ ", align 4"]
  return next

transExp (ExpAdd exp1 exp2) = do
  label1 <- transExp exp1
  label2 <- transExp exp2
  next   <- nextLabel
  tell [next ++ " = add nsw i32 " ++ label1 ++ ", " ++ label2]
  return next

transExp (ExpSub exp1 exp2) = do
  label1 <- transExp exp1
  label2 <- transExp exp2
  next   <- nextLabel
  tell [next ++ " = sub nsw i32 " ++ label1 ++ ", " ++ label2]
  return next

transExp (ExpMul exp1 exp2) = do
  label1 <- transExp exp1
  label2 <- transExp exp2
  next   <- nextLabel
  tell [next ++ " = mul nsw i32 " ++ label1 ++ ", " ++ label2]
  return next

transExp (ExpDiv exp1 exp2) = do
  label1 <- transExp exp1
  label2 <- transExp exp2
  next   <- nextLabel
  tell [next ++ " = sdiv i32 " ++ label1 ++ ", " ++ label2]
  return next


run :: Program -> IO [String]
run prog = do
  ((_, out), _) <- runStateT (runWriterT (transProgram prog)) (M.empty, 1)
  return out


printErr :: String -> IO ()
printErr msg = hPutStr stderr $ "Error: " ++ msg ++ "\n"


pack :: [String] -> String -> Int -> Int -> [String]
pack out name stack locals = prelude ++ out ++ postlude
  where
    prelude = [
      "@.str = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1",
      "define i32 @main() #0 {",
      "%1 = alloca i32, align 4",
      "store i32 0, i32* %1, align 4"]
    postlude = [
      "ret i32 0",
      "}",
      "declare i32 @printf(i8*, ...) #1"]


main :: IO ()
main = do
  args <- getArgs
  case args of
    []    -> printErr "No file provided"
    (f:_) -> do
      code <- readFile f
      case pProgram (myLexer code) of
        (Bad msg) -> printErr msg
        (Ok tree) -> do
          contents <- run tree
          writeFile clangName $ intercalate "\n" $ pack contents name 1000 1000
          callCommand $ "llvm-as " ++ clangName
            where
              name = dropExtension f
              clangName = name ++ ".ll"
