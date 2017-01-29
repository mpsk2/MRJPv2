module Compile.Commons where

import Control.Monad.State
import qualified Data.Map as Map

import Latte.Abs

import Commons

data Place = Stack Int | Register String deriving (Eq)

type CRunner r = Runner r CState

instance Show Place where
    show (Stack n) = (show n) ++ "+rbp"
    show (Register s) = s

data CState = CState {
    variables :: Map.Map String Place,
    topDefs :: Map.Map String Type,
    lastStack :: Int
    } deriving (Show, Eq)

initialCState :: CState
initialCState = CState { variables = Map.empty, topDefs = Map.empty, lastStack = 0 }

typeSize :: Type -> Int
typeSize Int = 8
typeSize Bool = 1

giveNextStack :: Type -> CRunner (Int, Int)
giveNextStack t = do
    st <- get
    let size = typeSize t
    let ls = (quot (lastStack st) size) * size - size
    put $ st { lastStack = ls }
    return (ls, typeSize t)

newFunctionStack :: CRunner ()
newFunctionStack = do
    st <- get
    put $ st { lastStack = 0 }
    return ()