module Validate.TypeCheck where

import Control.Monad.Except
import qualified Data.Set as Set

import Latte.Abs

import Commons
import Errors

data VTCState = VTCState deriving (Show, Eq)

initialVTCState :: VTCState
initialVTCState = VTCState 

type VTCRunner = Runner () VTCState
type VTCRT = RT () VTCState

vtcrProgram :: Program -> VTCRunner
vtcrProgram p = return () -- mock
