module Parser where

import Ast

import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language(emptyDef, commentLine, identLetter, identStart, reservedNames)
import Text.ParserCombinators.Parsec.Prim(Parser, (<|>), many)
import qualified Text.Parsec(parse)
import Text.ParserCombinators.Parsec.Char(alphaNum)

lexer :: P.TokenParser ()
lexer = P.makeTokenParser (emptyDef {commentLine=";", identLetter=alphaNum, identStart=alphaNum, reservedNames=["λ"]})

parens :: Parser a -> Parser a
parens = P.parens lexer

identifier :: Parser String
identifier = P.identifier lexer

commaSep1 :: Parser a -> Parser [a]
commaSep1 = P.commaSep1 lexer

symbol :: String -> Parser String
symbol = P.symbol lexer

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

parseVar :: Parser Term
parseVar = do name <- identifier
              return $ Var name

parseLambda :: Parser Term
parseLambda = do _ <- symbol "λ"
                 param <- identifier
                 _ <- symbol "."
                 body <- parseTerm
                 return $ Abs param body

parseTerm :: Parser Term
parseTerm = do t <- atom
               ts <- many atom
               return $ foldl App t ts

parse :: String -> Term
parse s = case Text.Parsec.parse parseTerm "" s of
  Left err -> error $ show err
  Right t -> t

atom :: Parser Term
atom = parseLambda <|> parseParenthesized <|> parseVar

parseParenthesized :: Parser Term
parseParenthesized = parens parseTerm
