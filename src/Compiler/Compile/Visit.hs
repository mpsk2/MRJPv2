module Compile.Visit where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map

import Latte.Abs

import Commons
import Compile.Commons
import Compile.Constants
import Errors

runCR :: Program -> RT [String] CState
runCR p = runR (rProgram p) initialCState

rProgram :: Program -> CRunner [String]
rProgram (StandardProgram tds) = do
    let begging = programHeader ["main"] ["puts", "printf"]
    mapM collectTopDef tds
    _middle <- mapM rTopDef tds
    let middle = foldr (++) [] _middle
    let ending = programFooter
    return $ begging ++ middle ++ ending
    where
        collectTopDef :: TopDef -> CRunner ()
        collectTopDef (FnDef ty (Ident s) _ _) = do
            st <- get
            put $ st { topDefs = Map.insert s ty $ topDefs st }
            return ()
        collectTopDef _ = throwError $ NotImplementedError "compile other top defs than functions"

rTopDef :: TopDef -> CRunner [String]
rTopDef (FnDef ty (Ident s) args block) = do
    newFunctionStack
    let begging = functionHeader s
    middle <- collectArgs args 1
    let ending = functionFooter
    return $ begging ++ middle ++ ending
    where
        collectArgs :: [Arg] -> Int -> CRunner [String]
        collectArgs [] _ = return []
        collectArgs ((NoInitArg ty (Ident s)):t) n | n < 7 = do
            (newStack, size) <- giveNextStack ty
            -- TODO: Add variable
            let thatOne = ["   mov   " ++ (stackName newStack size) ++ ", " ++ (registerName (argsRegister n) size)]
            next <- (collectArgs t (n+1))
            return $ thatOne ++ next
                                                   | otherwise = throwError $ NotImplementedError "collect args at compile"
rTopDef _ = throwError $ NotImplementedError "compile other top defs than functions"