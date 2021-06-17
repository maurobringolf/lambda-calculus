module Main where

import Parser
import Ast
import Interpreter

import System.Environment(getArgs)
import System.Directory(doesFileExist)
import System.Exit(exitFailure, exitSuccess)

import Data.List(isPrefixOf)

printUsage :: IO ()
printUsage = putStrLn "Usage: lc [--version] [--eager] PROGRAM [ARGS]"

printVersion :: IO ()
printVersion = putStrLn "lc 1.0.0"

optionList :: [String]
optionList = ["version", "eager"]

main :: IO ()
main = do args <- getArgs
          if length args == 0 then do
            printUsage
            exitFailure
          else do
            let options = [ opt | ('-':'-':opt) <- args, opt `elem` optionList ]
            if "version" `elem` options then do
              printVersion
              exitSuccess
            else do
              let interpreter = if "eager" `elem` options then eagerEval else lazyEval
                  fileName:arguments = filter (not . isPrefixOf "--") args
              fileExists <- doesFileExist fileName
              if not fileExists then do
                putStrLn $ "Cannot open file: " ++ fileName
              else do
                source <- readFile fileName
                let program = parse source
                    res = interpreter (foldl App program (map parse arguments))
                putStrLn $ show res
