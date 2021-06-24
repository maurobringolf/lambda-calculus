module SyntaxSugar.Parser where

import SyntaxSugar.Ast

import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr(buildExpressionParser, Assoc(..), Operator(..))
import Text.ParserCombinators.Parsec.Prim(Parser, (<|>), (<?>), parse, try)
import Text.ParserCombinators.Parsec.Combinator(many1, eof, sepBy)
import Text.ParserCombinators.Parsec.Char(alphaNum, oneOf, letter, newline)

import Data.Char(isSpace)

lexer = P.makeTokenParser emptyDef{ commentStart = "--"
                                  , commentEnd = "\n"
                                  , identStart = letter
                                  , identLetter = alphaNum
                                  , opStart = oneOf "*+-=<>"
                                  , opLetter = oneOf "*+-=<>"
                                  , reservedOpNames = ["+", "*", "-", "==", "<=", ">="]
                                  , reservedNames = ["true", "false", "nop",
                                                      "if", "then", "else", "fi",
                                                      "while", "do", "od", "True", "False"]
                                  }

P.TokenParser{ P.parens = m_parens
             , P.identifier = m_identifier
             , P.reservedOp = m_reservedOp
             , P.reserved = m_reserved
             , P.natural = m_natural
             , P.symbol = m_symbol
             , P.lexeme = m_lexeme
             , P.semiSep1 = m_semiSep1
             , P.whiteSpace = m_whiteSpace } = lexer

parseExp :: Parser Exp
parseExp = parseLambda <|> parseIfElse

parseIfElse :: Parser Exp
parseIfElse = do m_reservedOp "if"
                 b <- parseExp
                 m_reservedOp "then"
                 e1 <- parseExp
                 m_reservedOp "else"
                 e2 <- parseExp
                 return $ IfElse b e1 e2

parseLambda' :: Parser Exp
parseLambda' = do m_symbol "\\"
                  boundVar <- m_identifier
                  m_symbol "->"
                  body <- parseExp
                  return $ Abs boundVar body

parseLambda :: Parser Exp
parseLambda = parseLambda'
          <|> parseOp'

parseApp :: Parser Exp
parseApp = do xs <- many1 $ parseAtom
              return $ foldl1 App xs

parseAtom :: Parser Exp
parseAtom = parseBool <|> parseNum <|> parseVar <|> m_parens parseExp

parseBool :: Parser Exp
parseBool = (m_reserved "True" >> return (Boolean True)) <|> (m_reserved "False" >> return (Boolean False))

parseVar :: Parser Exp
parseVar = do x <- m_identifier
              return $ Var x

parseNum :: Parser Exp
parseNum = do n <- m_natural
              return $ Numeral n

parseOp' :: Parser Exp
parseOp' = buildExpressionParser table parseApp <?> "expression"

table = [
          [ Infix (m_reservedOp "*" >> return (\l -> \r ->(App (App Mult l) r)))  AssocRight]
        , [ Infix (m_reservedOp "+" >> return (\l -> \r ->(App (App Add l) r)))  AssocLeft, Infix (m_reservedOp "-" >> return (\l -> \r ->(App (App Sub l) r)))  AssocLeft]
        , [ Infix (m_reservedOp "==" >> return (\l -> \r ->(App (App Eq l) r)))  AssocNone, Infix (m_reservedOp "<=" >> return (\l -> \r ->(App (App Leq l) r)))  AssocNone, Infix (m_reservedOp ">=" >> return (\l -> \r ->(App (App Geq l) r)))  AssocNone]
        , [ Infix (m_reservedOp "&&" >> return (\l -> \r ->(App (App And l) r))) AssocRight]
        , [ Infix (m_reservedOp "||" >> return (\l -> \r ->(App (App Or l) r))) AssocRight]
        ]

parse :: String -> Program
parse c = let defs = splitDefs c
           in
  P $ map (\d -> (case Text.ParserCombinators.Parsec.Prim.parse (parseDefinition <* eof) "" d of Left err -> error (show err); Right e -> e)) defs

splitDefs :: String -> [String]
splitDefs s = let ls = filter (/= "") $ lines s
                  indented = map (isSpace . head) ls
                  xs = zip indented ls
               in
                  go xs
               where
                  go [] = []
                  go ((False, line):rs) = let (same, rest) = span fst rs
                                           in [line ++ concatMap snd same] ++ go rest

parseDefinition :: Parser Definition
parseDefinition = do id <- m_identifier
                     m_symbol "="
                     e <- parseExp
                     return $ Def id e
