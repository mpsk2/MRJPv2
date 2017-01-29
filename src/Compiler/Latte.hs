module Latte where

import System.Exit

import Latte.Lex
import Latte.Par
import Latte.Print

import Latte.ErrM

import Commons
import Errors

type ParseFun a = [Token] -> Err a

myLLexer :: String -> [Token]
myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = if v > 1 then putStrLn s else return ()

runFile :: (Print a, Show a) => Verbosity -> ParseFun a -> FilePath -> RunRT a
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: (Print a, Show a) => Verbosity -> ParseFun a -> String -> RunRT a
run v p s = let ts = myLLexer s in case p ts of
    Bad s2   -> do putStrLn "\nParse              Failed...\n"
                   putStrV v "Tokens:"
                   putStrV v $ show ts
                   putStrLn s2
                   exitFailure
    Ok  tree -> do putStrLn "\nParse Successful!"
                   return $ Right tree

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree = do
    putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
    putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree
