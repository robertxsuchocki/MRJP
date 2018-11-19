module Main where

import AbsInstant
import LexInstant
import ParInstant
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
type Code = [String]
type Store = M.Map Label Loc
type StackHeight = Int
type WS a = WriterT Code (StateT (Store, StackHeight) IO) a


nextLocation :: WS Loc

nextLocation = do
  (store, _) <- get
  return $ (M.size store) + 1


stmtLoc :: String -> Int -> String

stmtLoc str loc = concat [str, sep, show loc]
  where
    sep = if (0 <= loc && loc <= 3) then "_" else " "


stmtInt :: Integer -> String

stmtInt int
  | 0 <= int && int <= 5          = "iconst_" ++ intStr
  | -128 <= int && int <= 127     = "bipush " ++ intStr
  | -32769 <= int && int <= 32767 = "sipush " ++ intStr
  | otherwise                     = "ldc "    ++ intStr
  where
    intStr = show int


transIdent :: Ident -> WS Loc

transIdent (Ident name) = do
  (store, _) <- get
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
  loc <- transIdent ident
  (code, sh) <- transExp exp
  modify (\(s, h) -> ((M.insert name loc s), (max h sh)))
  tell code
  tell [stmtLoc "istore" loc]

transStmt (SExp exp) = do
  tell ["getstatic java/lang/System/out Ljava/io/PrintStream;"]
  (code, sh) <- transExp exp
  modify (\(s, h) -> (s, (max h (sh + 1))))
  tell code
  tell ["invokevirtual java/io/PrintStream/println(I)V"]


transExp :: Exp -> WS (Code, StackHeight)

transExp (ExpAdd exp1 exp2) = transOp "iadd" exp1 exp2 True

transExp (ExpSub exp1 exp2) = transOp "isub" exp1 exp2 False

transExp (ExpMul exp1 exp2) = transOp "imul" exp1 exp2 True

transExp (ExpDiv exp1 exp2) = transOp "idiv" exp1 exp2 False

transExp (ExpLit int) = return ([stmtInt int], 1)

transExp (ExpVar ident) = do
  loc <- transIdent ident
  return ([stmtLoc "iload" loc], 1)


transOp :: String -> Exp -> Exp -> Bool -> WS (Code, StackHeight)

transOp op exp1 exp2 swap = do
  (code1, sh1) <- transExp exp1
  (code2, sh2) <- transExp exp2
  if not swap
    then return (concat [code1, code2, [op]], (max sh1 (sh2 + 1)))
    else do
      let height = if sh1 == sh2 then (sh1 + 1) else (max sh1 sh2)
      let code   = if sh1 < sh2 then code2 ++ code1 else code1 ++ code2
      return (code ++ [op], height)


pack :: Code -> String -> (Store, StackHeight) -> Code

pack out name (store, height) = prelude ++ out ++ postlude
  where
    prelude = [
      ".class public " ++ name,
      ".super java/lang/Object",
      ".method public static main([Ljava/lang/String;)V",
      ".limit stack " ++ (show height),
      ".limit locals " ++ (show $ (M.size store) + 1)]
    postlude = [
      "return",
      ".end method"]


run :: FilePath -> Program -> IO ()

run file prog = do
  ((_, out), state) <- runStateT (runWriterT (transProgram prog)) (M.empty, 1)
  let code = intercalate "\n" $ pack out name state
  writeFile filePath code
  handle <- runCommand command
  void $ waitForProcess handle
    where
      filePath = dropExtension file ++ ".j"
      dirPath  = takeDirectory filePath
      name     = takeBaseName filePath
      command  = unwords ["java", "-jar", "lib/jasmin.jar", "-d", dirPath,
                          filePath, ">/dev/null"]


main :: IO ()

main = do
  args <- getArgs
  case args of
    []    -> hPutStr stderr $ "Error: No file provided\n"
    (file:_) -> do
      code <- readFile file
      case pProgram (myLexer code) of
        (Bad msg) -> hPutStr stderr $ "Error: " ++ msg ++ "\n"
        (Ok tree) -> run file tree
