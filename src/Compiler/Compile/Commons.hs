module Compile.Commons where

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

import Latte.Abs

import Commons

data Place = Stack Int | Register String deriving (Eq, Ord)

type CRunner r = Runner r CState

instance Show Place where
    show (Stack n) = (show n) ++ "+rbp"
    show (Register s) = s

data CState = CState {
    variables :: Map.Map String (Place, Type),
    topDefs :: Map.Map String Type,
    lastStack :: Int,
    usedRegisters :: Set.Set Place,
    stmtUsedRegisters :: Set.Set Place,
    endFunctionLabel :: String
    } deriving (Show, Eq)

initialCState :: CState
initialCState = CState { variables = Map.empty
                       , topDefs = Map.empty
                       , lastStack = 0
                       , usedRegisters = Set.empty
                       , stmtUsedRegisters = Set.empty
                       , endFunctionLabel = "" }

typeSize :: Type -> Int
typeSize Int = 8
typeSize Str = 8
typeSize Bool = 1

giveNextStack :: Type -> CRunner (Int, Int)
giveNextStack t = do
    st <- get
    let size = typeSize t
    let ls = (quot (lastStack st) size) * size - size
    put $ st { lastStack = ls }
    return (ls, typeSize t)

getArgStack :: Int -> Int
getArgStack n = (n - 4) * 8

newFunctionStack :: CRunner ()
newFunctionStack = do
    st <- get
    put $ st { lastStack = 0 }
    return ()

addVariable :: Type -> String -> Place -> CRunner ()
addVariable t name place = do
    st <- get
    put $ st { variables = Map.insert name (place, t) (variables st) }
    return ()

callerSafe :: Place -> Bool
callerSafe (Register "rbp") = True
callerSafe (Register "rbx") = True
callerSafe (Register "r12") = True
callerSafe (Register "r13") = True
callerSafe (Register "r14") = True
callerSafe (Register "r15") = True
callerSafe _ = False

makeCallerSafe :: CRunner ([String], [String])
makeCallerSafe = do
    st <- get
    iterateRegisters (Set.toList $ usedRegisters st) [] []
    where
        iterateRegisters :: [Place] -> [String] -> [String] -> CRunner ([String], [String])
        iterateRegisters [] acc1 acc2 = return $ (acc1, reverse acc2)
        iterateRegisters (h:t) acc1 acc2 | callerSafe h = do
            giveNextStack Int
            let nextBegging = (("   push  " ++ (show h) ++ " ; that register is caller safe"):acc1)
            let nextEnding = (("   pop   " ++ (show h) ++ " ; that register is caller safe"):acc2)
            iterateRegisters t nextBegging nextEnding
                                         | otherwise = iterateRegisters t acc1 acc2

places :: Set.Set Place
places = Set.fromList [ Register "r15"
                      , Register "r14"
                      , Register "r13"
                      , Register "r12"
                      , Register "r11"
                      , Register "r10"
                      , Register "r9"
                      , Register "r8"
                      , Register "rcx"
                      , Register "rdx"
                      , Register "rsi"
                      , Register "rdi"
                      , Register "rdx"]

givePlace :: CRunner Place
givePlace = do
    st <- get
    let used = stmtUsedRegisters st
    let freeSpace = Set.toList $ Set.difference places used
    case freeSpace of
        [] -> do
            (s,_) <- giveNextStack Int
            return $ Stack s
        (h:_) -> do
            put $ st { stmtUsedRegisters = Set.insert h used, usedRegisters = Set.insert h (usedRegisters st)}
            return h

restorePlace :: Place -> CRunner ()
restorePlace (Stack _) = return ()
restorePlace r = do
    st <- get
    put $ st { stmtUsedRegisters = Set.delete r $ stmtUsedRegisters st }
    return ()