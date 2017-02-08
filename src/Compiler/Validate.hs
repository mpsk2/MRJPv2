module Validate where

import Control.Applicative
import Control.Lens
import Control.Lens.Zoom
import Control.Monad.Except
import Control.Monad.State

import Latte.Abs

import Commons
import Errors

import Validate.Ident
import Validate.TypeCheck

goGo :: (Program -> Runner () a) -> (Program -> Runner () b) -> Program -> Runner () (a, b)
goGo a b = liftA2 (>>) (zoom _1 . a) (zoom _2 . b)

goValidate :: Program -> Runner () (VIState, VTCState)
goValidate = goGo virProgram vtcrProgram

initialVState :: (VIState, VTCState)
initialVState = (initialVIState, initialVTCState)

validate :: Program -> RunRT ()
validate program = runExceptT (evalStateT (goValidate program) initialVState)

