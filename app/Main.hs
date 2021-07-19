{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Parser(parse)
import Ast
import Interpreter

import ChurchEncoding.Compiler(compileMain, execTyped)
import qualified ChurchEncoding.Parser(parse)

import System.Directory(doesFileExist)
import System.Exit(exitFailure, exitSuccess)
import Options.Applicative

import Control.Monad(when)

data Options = Options { evaluationStrategy :: EvaluationStrategy
                       , compileOnly :: Bool
                       , haskell :: Bool
                       , file :: String
                       , arguments :: [String]
                       }


version :: Parser (a -> a)
version = infoOption "v1.0.0" (long "version" <> short 'v' <> help "Print version and exit.")

eagerParser :: Parser EvaluationStrategy
eagerParser = flag lazyStrategy Eager (long "eager" <> short 'e' <> help "Use eager evaluation strategy instead of lazy (does not apply for --haskell).")

compileOnlyParser :: Parser Bool
compileOnlyParser = switch (long "compileOnly" <> short 'c' <> help "Do not run the program, but print the lambda term to stdout.")

haskellParser :: Parser Bool
haskellParser = switch (long "haskell" <> help "Change input language to Haskell.")

fileParser :: Parser String
fileParser = argument str (metavar "FILE" <> help "Program to execute")

argsParser :: Parser [String]
argsParser = many $ argument str (metavar "ARGS..." <> help "Optional space-separated list of lambda terms to apply the program to")

optionsParser :: Parser Options
optionsParser = Options <$> eagerParser <*> compileOnlyParser <*> haskellParser <*> fileParser <*> argsParser

main :: IO ()
main = do
          Options{evaluationStrategy, compileOnly, haskell, file, arguments} <- execParser $ (info (helper <*> version <*> optionsParser)) (fullDesc <> progDesc "" <> header "")

          fileExists <- doesFileExist file

          when (not fileExists) $ do
            putStrLn $ "Cannot open file " ++ file
            exitFailure

          source <- readFile file

          if haskell then do
            let program = ChurchEncoding.Parser.parse source
            when (compileOnly) $ do
              putStrLn $ show $ compileMain program
              exitSuccess
            putStrLn $ execTyped $ program
          else do
            let program = Parser.parse source
            let res = eval evaluationStrategy $ foldl App program $ map Parser.parse arguments
            putStrLn $ show res
