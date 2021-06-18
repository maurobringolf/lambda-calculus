{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Parser(parse)
import Ast
import Interpreter

import System.Environment(getArgs)
import System.Directory(doesFileExist)
import System.Exit(exitFailure, exitSuccess)
import Options.Applicative

import Data.List(isPrefixOf)

import Control.Monad(when)

data Options = Options { evaluationStrategy :: EvaluationStrategy
                       , file :: String
                       , arguments :: [String]
                       }


version :: Parser (a -> a)
version = infoOption "lc v1.0.0" (long "version" <> short 'v' <> help "Print version and exit.")

eagerParser :: Parser EvaluationStrategy
eagerParser = flag Lazy Eager (long "eager" <> short 'e' <> help "Use eager evaluation strategy instead of lazy (the default).")

fileParser :: Parser String
fileParser = argument str (metavar "FILE" <> help "Program to execute")

argsParser :: Parser [String]
argsParser = many $ argument str (metavar "ARGS..." <> help "Optional space-separated list of lambda terms to apply the program to")

optionsParser :: Parser Options
optionsParser = Options <$> eagerParser <*> fileParser <*> argsParser

main :: IO ()
main = do
          Options{evaluationStrategy, file, arguments} <- execParser $ (info (helper <*> version <*> optionsParser)) (fullDesc <> progDesc "" <> header "")

          fileExists <- doesFileExist file

          when (not fileExists) $ do
            putStrLn $ "Cannot open file " ++ file
            exitFailure

          source <- readFile file
          let program = parse source
              res = eval evaluationStrategy $ foldl App program $ map parse arguments
          putStrLn $ show res
