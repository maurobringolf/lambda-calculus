{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Parser(parse)
import Ast
import Interpreter

import SyntaxSugar.Compiler(compileMain, execTyped)
import qualified SyntaxSugar.Parser(parse)

import System.Directory(doesFileExist)
import System.Exit(exitFailure, exitSuccess)
import Options.Applicative

import Control.Monad(when)

data Options = Options { evaluationStrategy :: EvaluationStrategy
                       , compileOnly :: Bool
                       , syntaxSugar :: Bool
                       , file :: String
                       , arguments :: [String]
                       }


version :: Parser (a -> a)
version = infoOption "lc v1.0.0" (long "version" <> short 'v' <> help "Print version and exit.")

eagerParser :: Parser EvaluationStrategy
eagerParser = flag Lazy Eager (long "eager" <> short 'e' <> help "Use eager evaluation strategy instead of lazy (the default).")

compileOnlyParser :: Parser Bool
compileOnlyParser = switch (long "compileOnly" <> short 'c' <> help "Parse and compile only and print the result to stdout.")

syntaxSugarParser :: Parser Bool
syntaxSugarParser = switch (long "syntaxSugar" <> short 's' <> help "Compile away all syntax sugar.")

fileParser :: Parser String
fileParser = argument str (metavar "FILE" <> help "Program to execute")

argsParser :: Parser [String]
argsParser = many $ argument str (metavar "ARGS..." <> help "Optional space-separated list of lambda terms to apply the program to")

optionsParser :: Parser Options
optionsParser = Options <$> eagerParser <*> compileOnlyParser <*> syntaxSugarParser <*> fileParser <*> argsParser

main :: IO ()
main = do
          Options{evaluationStrategy, compileOnly, syntaxSugar, file, arguments} <- execParser $ (info (helper <*> version <*> optionsParser)) (fullDesc <> progDesc "" <> header "")

          fileExists <- doesFileExist file

          when (not fileExists) $ do
            putStrLn $ "Cannot open file " ++ file
            exitFailure

          source <- readFile file

          if syntaxSugar then do
            let program = SyntaxSugar.Parser.parse source
            when (compileOnly) $ do
              putStrLn $ show program
              exitSuccess
            putStrLn $ execTyped $ program
          else do
            let program = Parser.parse source
            let res = eval evaluationStrategy $ foldl App program $ map Parser.parse arguments
            putStrLn $ show res
