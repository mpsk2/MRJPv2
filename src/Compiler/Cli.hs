module Cli where

import Data.Strings ( strEndsWith )
import System.Environment
import System.Exit
import System.Console.GetOpt
import System.Directory
import System.IO

data Options = Options { } deriving (Eq, Show)

startOptions :: Options
startOptions = Options { }

usageInfoHeader :: String -> String
usageInfoHeader program_name = "Usage: " ++ program_name ++ " file_name [OPTIONS]"

options :: [ OptDescr (Options -> IO Options) ]
options =
  [ Option "h" ["help"]
    (NoArg
      (\_ -> do
	prg <- getProgName
	hPutStrLn stderr (usageInfo (usageInfoHeader prg) options)
	exitWith ExitSuccess))
      "Show help"
  ]


getOptions :: [String] -> ([Options -> IO Options], [String], [String])
getOptions args = getOpt RequireOrder options args

argsFail :: IO ()
argsFail = do
  prg <- getProgName
  hPutStrLn stderr (usageInfo (usageInfoHeader prg) options)
  exitWith ExitSuccess

validateArgs :: [String] -> IO ()
validateArgs [] = argsFail
validateArgs [path] = do
  exists' <- doesFileExist path
  if exists'
     then return ()
     else argsFail
validateArgs (_:_:_) = argsFail

asmName :: String -> String
asmName fileName | strEndsWith fileName ".lat" = (Prelude.take ((Prelude.length fileName) - 3) fileName) ++ "s"
                 | otherwise = fileName ++ ".s"

binName :: String -> String
binName fileName | strEndsWith fileName ".lat" = (Prelude.take ((Prelude.length fileName) - 4) fileName) ++ ".out"
                 | otherwise = "your_file_had_bad_extension.out"
