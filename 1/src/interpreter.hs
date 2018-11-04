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

type Loc = Integer
type Store = M.Map String Integer
type WS a = WriterT [String] (StateT Store IO) a


alloc :: WS Loc
alloc = do
  store <- get
  if (M.null store)
    then return 1
    else let (name, loc) = M.findMax store in return $ loc + 1


transIdent :: Ident -> WS Loc

transIdent (Ident name) = do
  store <- get
  return $ fromMaybe 1 (M.lookup name store)


transProgram :: Program -> WS ()

transProgram (Prog []) = return ()

transProgram (Prog (s:ss)) = do
  transStmt s
  transProgram $ Prog ss


transStmt :: Stmt -> WS ()

transStmt (SAss (Ident name) expr) = do
  loc <- alloc
  transExp expr
  modify (M.insert name loc)
  tell $ ["istore_" ++ show loc]

transStmt (SExp expr) = do
  tell $ ["getstatic java/lang/System/out Ljava/io/PrintStream;"]
  transExp expr
  tell $ ["invokevirtual java/io/PrintStream/println(I)V"]


transExp :: Exp -> WS ()

transExp (ExpAdd exp1 exp2) = do
  value1 <- transExp exp1
  value2 <- transExp exp2
  tell ["iadd"]

transExp (ExpSub expr1 expr2) = do
  value1 <- transExp expr1
  value2 <- transExp expr2
  tell ["isub"]

transExp (ExpMul expr1 expr2) = do
  value1 <- transExp expr1
  value2 <- transExp expr2
  tell ["imul"]

transExp (ExpDiv expr1 expr2) = do
  value1 <- transExp expr1
  value2 <- transExp expr2
  tell ["idiv"]

transExp (ExpLit integer) = do
  tell ["bipush " ++ show integer]

transExp (ExpVar ident) = do
  loc <- transIdent ident
  tell ["iload_" ++ show loc]


run :: Program -> IO [String]
run prog = do
  ((_, out), _) <- runStateT (runWriterT (transProgram prog)) M.empty
  return out


printErr :: String -> IO ()
printErr msg = hPutStr stderr $ "Error: " ++ msg ++ "\n"


pack :: [String] -> String -> Integer -> Integer -> [String]
pack out name stack locals = prelude ++ out ++ postlude
  where
    prelude = [".class public " ++ name,
               ".super java/lang/Object",
               ".method public static main([Ljava/lang/String;)V",
               ".limit stack " ++ (show stack),
               ".limit locals " ++ (show locals)]
    postlude = ["return",
                ".end method"]


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
          writeFile jasminName $ intercalate "\n" $ pack contents name 1000 1000
          callCommand $ "java -jar lib/jasmin.jar " ++ jasminName
            where
              name = dropExtension f
              jasminName = name ++ ".j"
