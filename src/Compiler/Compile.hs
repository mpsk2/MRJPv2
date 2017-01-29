module Compile where

import Compile.Constants

emptyProgram :: String
emptyProgram = unlines $ (programHeader ["main"] []) ++ (functionHeader "main") ++ functionFooter