module Main where

import Data.Either
import System.Environment
import System.Exit
import System.Process

import Cli
import Compile
import Compile.Visit
import Latte
import Latte.Abs
import Latte.Par
import Validate

handleValidate :: Program -> IO ()
handleValidate p = do
    v <- validate p
    case v of
        Right _ -> return ()
        Left e -> do
            putStrLn $ show e
            exitFailure 

main :: IO ()
main = do
    args <- getArgs
    let (actions, nonOptions, _) = getOptions args
    _ <- foldl (>>=) (return startOptions) actions

    validateArgs nonOptions

    let inputFileName = head nonOptions

    rightProgram <- runFile 0 pProgram inputFileName
    let program = head $ rights [rightProgram]

    handleValidate program

    -- Compile below
    let asmFileName = asmName inputFileName
    let binFileName = binName inputFileName

    p <- runCR program
    case p of
        Left a -> do
            putStrLn $ show a
            exitFailure
        Right (r, s) -> do
            writeFile asmFileName $ unlines r

            putStrLn $ "Assembly file: " ++ asmFileName
            putStrLn $ "Binary   file: " ++ binFileName
            putStrLn $ "Running nasm"
            callProcess "nasm" ["-felf64", asmFileName, "-o", "__a.o"]
            putStrLn $ "Linking with c standard libraries"
            callProcess "gcc" ["__a.o", "-o", binFileName]
            callProcess "rm" ["__a.o"]
            return ()

