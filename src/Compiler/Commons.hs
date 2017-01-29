module Commons where

import Errors

type RunRT r = IO (Either LatteError r)