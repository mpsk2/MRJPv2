module Commons where

import Control.Monad.Except
import Control.Monad.State

import Errors

type Runner returnType stateType = StateT stateType (ExceptT LatteError IO) returnType
type RT r s = IO (Either LatteError (r, s))
type RunRT r = IO (Either LatteError r)

runR :: Runner r s -> s -> RT r s
runR runner initialState = runExceptT (runStateT runner initialState)