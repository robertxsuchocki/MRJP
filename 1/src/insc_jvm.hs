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

type Loc = Int
type Store = M.Map String Int
type WS a = WriterT [String] (StateT Store IO) a


nextLocation :: WS Loc
nextLocation = do
  store <- get
  return $ (M.size store) + 1


transIdent :: Ident -> WS Loc

transIdent (Ident name) = do
  store <- get
  let loc = fromMaybe 0 (M.lookup name store)
  if loc /= 0
    then return loc
    else do
      new <- nextLocation
      return new


transProgram :: Program -> WS ()

transProgram (Prog []) = return ()

transProgram (Prog (s:ss)) = do
  transStmt s
  transProgram $ Prog ss


transStmt :: Stmt -> WS ()

transStmt (SAss ident@(Ident name) exp) = do
  loc   <- transIdent ident
  transExp exp
  modify (M.insert name loc)
  tell $ ["istore " ++ show loc]

transStmt (SExp exp) = do
  tell $ ["getstatic java/lang/System/out Ljava/io/PrintStream;"]
  transExp exp
  tell $ ["invokevirtual java/io/PrintStream/println(I)V"]


transExp :: Exp -> WS ()

transExp (ExpLit integer) = do
  tell ["bipush " ++ show integer]

transExp (ExpVar ident) = do
  loc <- transIdent ident
  tell ["iload " ++ show loc]

transExp (ExpAdd exp1 exp2) = do
  transExp exp1
  transExp exp2
  tell ["iadd"]

transExp (ExpSub exp1 exp2) = do
  transExp exp1
  transExp exp2
  tell ["isub"]

transExp (ExpMul exp1 exp2) = do
  transExp exp1
  transExp exp2
  tell ["imul"]

transExp (ExpDiv exp1 exp2) = do
  transExp exp1
  transExp exp2
  tell ["idiv"]


run :: Program -> IO [String]
run prog = do
  ((_, out), _) <- runStateT (runWriterT (transProgram prog)) M.empty
  return out


printErr :: String -> IO ()
printErr msg = hPutStr stderr $ "Error: " ++ msg ++ "\n"


pack :: [String] -> String -> Int -> Int -> [String]
pack out name stack locals = prelude ++ out ++ postlude
  where
    prelude = [
      ".class public " ++ name,
      ".super java/lang/Object",
      ".method public static main([Ljava/lang/String;)V",
      ".limit stack " ++ (show stack),
      ".limit locals " ++ (show locals)]
    postlude = [
      "return",
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
