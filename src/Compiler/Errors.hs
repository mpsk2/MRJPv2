module Errors where

data LatteError
    = NotImplementedError String
    | ParseError
    | NoSuchIdent String
    | OtherError String
    deriving Eq

instance Show LatteError where
    show (NotImplementedError s) = s ++ " has not been implemented."
    show ParseError = "Syntax error detected by parser."
    show (NoSuchIdent s) = s ++ " ident does not exists."

