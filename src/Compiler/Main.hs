module Main where

import Data.Either
import System.Environment
import System.Process

import Cli
import Compile
import Latte
import Latte.Par

main :: IO ()
main = do
    args <- getArgs
    let (actions, nonOptions, _) = getOptions args
    _ <- foldl (>>=) (return startOptions) actions

    validateArgs nonOptions

    let inputFileName = head nonOptions

    rightProgram <- runFile 0 pProgram inputFileName
    let program = rights [rightProgram]

    -- Compile below
    let asmFileName = asmName inputFileName
    let binFileName = binName inputFileName

    writeFile asmFileName emptyProgram

    putStrLn $ "Assembly file: " ++ asmFileName
    putStrLn $ "Binary   file: " ++ binFileName
    putStrLn $ "Running nasm"
    callProcess "nasm" ["-felf64", asmFileName, "-o", "__a.o"]
    putStrLn $ "Linking with c standard libraries"
    callProcess "gcc" ["__a.o", "-o", binFileName]
    callProcess "rm" ["__a.o"]
    return ()
