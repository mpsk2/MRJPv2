module Compile.Constants where

import Data.List ( intercalate )

header :: [String]
header = [
    "   ; Michał Piotr Stankiewicz",
    "   ; Latte Compiler program",
    "   ; MRJP 2016/2017",
    ""
    ]

programHeader :: [String] -> [String] -> [String]
programHeader globals externs = header ++ [
        joinLike "global" globals,
        joinLike "extern" externs,
        "    section .text"
    ]
    where
        joinLike :: String -> [String] -> String
        joinLike name [] = "    ; no " ++ name ++ "s"
        joinLike name s = "    " ++ name ++ " " ++ (intercalate ", " s)

functionHeader :: String -> [String]
functionHeader functionName = [
    functionName ++ ":",
    "   push  rbp",
    "   mov   rbp, rsp",
    ""
    ]

functionFooter :: [String]
functionFooter = [
    "   pop rbp",
    "   ret"
    ]