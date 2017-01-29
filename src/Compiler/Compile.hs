module Compile where

import Compile.Commons
import Compile.Constants

emptyProgram :: String
emptyProgram = unlines $ (programHeader ["main"] ["puts", "printf"]) ++ (functionHeader "main") ++ functionFooter ++ programFooter

