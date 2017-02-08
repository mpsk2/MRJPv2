module Validate.Ident where

import Control.Monad.Except
import qualified Data.Set as Set

import Latte.Abs

import Commons
import Errors

data VIState = VIState { idents :: Set.Set String } deriving (Show, Eq)

initialVIState :: VIState
initialVIState = VIState { idents = Set.empty }

type VIRunner = Runner () VIState
type VIRT = RT () VIState

runVIR :: Program -> VIRT
runVIR p = runR (virProgram p) initialVIState 
 
virProgram :: Program -> VIRunner
virProgram (StandardProgram tdfs) = do
    return ()
