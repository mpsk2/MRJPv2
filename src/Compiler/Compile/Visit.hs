module Compile.Visit where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

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
    let endLabel = s ++ "_end"
    st_0 <- get
    put $ st_0 { endFunctionLabel = endLabel }
    st <- get
    let begging = functionHeader s

    st_1 <- get
    rBlock block
    st_2 <- get
    put $ st_1 { usedRegisters = usedRegisters st_2 }
    (csb, csa) <- makeCallerSafe
    middle <- collectArgs args 1
    blockContent <- rBlock block

    let ending = functionFooter
    st2 <- get
    liftIO $ putStrLn $ (show st2) ++ "for next function"
    put $ st
    return $ begging ++ csb ++ middle ++ blockContent ++ [endLabel ++ ":"] ++ csa ++ ending
    where
        collectArgs :: [Arg] -> Int -> CRunner [String]
        collectArgs [] _ = return []
        collectArgs ((NoInitArg ty (Ident s)):t) n | n < 7 = do
            (newStack, size) <- giveNextStack ty
            addVariable ty s (Stack newStack)
            let thatOne = ["   mov   " ++ (stackName newStack size) ++ ", " ++ (registerName (argsRegister n) size)]
            next <- (collectArgs t (n+1))
            return $ thatOne ++ next
                                                   | otherwise = do
            let stackNumber = getArgStack n
            addVariable ty s (Stack stackNumber)
            collectArgs t (n+1)
rTopDef _ = throwError $ NotImplementedError "compile other top defs than functions"

rBlock :: Block -> CRunner [String]
rBlock (ParenBlock stmts) = do
    st_1 <- get

    _middle <- mapM rStmt stmts
    let middle = concat _middle

    st_2 <- get
    put $ st_1 { usedRegisters = usedRegisters st_2 }

    return middle

rStmt :: Stmt -> CRunner [String]
rStmt Empty = return $ []
rStmt (BStmt block) = rBlock block
rStmt (SDecl decl) = rDecl decl
rStmt (Incr (Ident s)) = do
    st <- get
    let (place, t) = (Map.!) (variables st) s
    case place of
        Stack i -> return $ ["   inc   " ++ (stackName i $ typeSize t)]
        r@(Register _) -> throwError $ NotImplementedError "compile decr register"
rStmt (Decr (Ident s)) = do
    st <- get
    let (place, t) = (Map.!) (variables st) s
    case place of
        Stack i -> return $ ["   dec   " ++ (stackName i $ typeSize t)]
        r@(Register _) -> throwError $ NotImplementedError "compile decr register"
rStmt (Ret expr) = do
    exprOutput <- rExpr expr
    ret <- rStmt VRet
    return $ exprOutput ++ ret
rStmt VRet = do
    st <- get
    return ["   jmp " ++ (endFunctionLabel st) ]
rStmt _ = return $ ["   nop"]

rDecl :: Decl -> CRunner [String]
rDecl (Decl t items) = do
    _middle <- mapM (rItem t) items
    return $ concat _middle
rItem :: Type -> Item -> CRunner [String]
rItem t (NoInit (Ident s)) = do
    (st, size) <- giveNextStack t
    addVariable t s (Stack st)
    return $ ["   mov   " ++ (stackName st size) ++ ", 0"]
rItem _ _ = throwError $ NotImplementedError "compile item init"

rExpr :: Expr -> CRunner [String]
rExpr (ELitInt 0) = do
    return $ ["   xor rax, rax"]
rExpr (ELitInt n) = return $ ["   mov rax, " ++ (show n)]
rExpr (Neg (ELitInt n)) = rExpr (ELitInt (-n))
rExpr (Neg expr) = do
    e <- rExpr expr
    return $ e ++ ["   neg rax"]
rExpr e@(EAdd expr1 Plus expr2) = do
    place <- givePlace
    e1 <- rExpr expr1
    e2 <- rExpr expr2
    case place of
        Stack n -> throwError $ NotImplementedError "compile add stack"
        r@(Register _) -> do
            restorePlace r
            return $ e1 ++ ["   mov " ++ (show r) ++ ", rax"] ++ e2 ++ ["   add rax, " ++ (show r)]
rExpr (EMul expr1 Times expr2) = do
    place <- givePlace
    e1 <- rExpr expr1
    e2 <- rExpr expr2
    case place of
        Stack n -> throwError $ NotImplementedError "compile add stack"
        r@(Register _) -> do
            restorePlace r
            return $ e1 ++ ["   mov " ++ (show r) ++ ", rax"] ++ e2 ++ ["   mul " ++ (show r)]

rExpr _ = return $ ["   xor rax, rax ; fake end"]
