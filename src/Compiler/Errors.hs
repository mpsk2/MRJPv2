module Errors where

data LatteError
    = NotImplementedError String
    | ParseError
    | OtherError String
    deriving (Eq, Show)

