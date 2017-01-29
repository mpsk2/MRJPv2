module Compile.Constants where

import Data.List ( intercalate )
import qualified Data.Map as Map

import Compile.Commons

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

printString :: [String]
printString = [
    "   ; assume that string pointer is in rdi",
    "   call puts"
    ]

printInt :: [String]
printInt = [
    "    ; assume that int is in rdi",
    "    mov   rsi, rdi",
    "    mov   rdi, format",
    "    xor   rax, rax ; because printf is varargs",
    "    call  printf"
    ]

programFooter :: [String]
programFooter = [
    "format:",
    "   db \"%20ld\", 10, 0"
    ]

registers :: Map.Map String (Map.Map Int String)
registers = Map.fromList [
    ("rax", Map.fromList [(1, "al"), (2, "ax"), (4, "eax"), (8, "rax")]),
    ("rcx", Map.fromList [(1, "cl"), (2, "cx"), (4, "ecx"), (8, "rcx")]),
    ("rdx", Map.fromList [(1, "dl"), (2, "dx"), (4, "edx"), (8, "rdx")]),
    ("rbx", Map.fromList [(1, "bl"), (2, "bx"), (4, "ebx"), (8, "rbx")]),
    ("rsp", Map.fromList [(1, "spl"), (2, "sp"), (4, "esp"), (8, "rsp")]),
    ("rbp", Map.fromList [(1, "bpl"), (2, "bp"), (4, "ebp"), (8, "rbp")]),
    ("rsi", Map.fromList [(1, "sil"), (2, "si"), (4, "esi"), (8, "rsi")]),
    ("rdi", Map.fromList [(1, "dil"), (2, "di"), (4, "edi"), (8, "rdi")]),
    ("r8", Map.fromList [(1, "r8b"), (2, "r8w"), (4, "r8d"), (8, "r8")]),
    ("r9", Map.fromList [(1, "r9b"), (2, "r9w"), (4, "r9d"), (8, "r9")]),
    ("r10", Map.fromList [(1, "r10b"), (2, "r10w"), (4, "r10d"), (8, "r10")]),
    ("r11", Map.fromList [(1, "r11b"), (2, "r11w"), (4, "r11d"), (8, "r11")]),
    ("r12", Map.fromList [(1, "r12b"), (2, "r12w"), (4, "r12d"), (8, "r12")]),
    ("r13", Map.fromList [(1, "r13b"), (2, "r13w"), (4, "r13d"), (8, "r13")]),
    ("r14", Map.fromList [(1, "r14b"), (2, "r14w"), (4, "r14d"), (8, "r14")]),
    ("r15", Map.fromList [(1, "r15b"), (2, "r15w"), (4, "r15d"), (8, "r15")])
    ]

registerName :: String -> Int -> String
registerName name bytes = (Map.!) ((Map.!) registers name) bytes

stackName :: Int -> Int -> String
stackName diff 1 = "BYTE [" ++ (show $ Stack diff) ++ "]"
stackName diff 2 = "WORD [" ++ (show $ Stack diff) ++ "]"
stackName diff 4 = "DWORD [" ++ (show $ Stack diff) ++ "]"
stackName diff 8 = "QWORD [" ++ (show $ Stack diff) ++ "]"

argsRegister :: Int -> String
argsRegister 1 = "rdi"
argsRegister 2 = "rsi"
argsRegister 3 = "rdx"
argsRegister 4 = "rcx"
argsRegister 5 = "r8"
argsRegister 6 = "r9"