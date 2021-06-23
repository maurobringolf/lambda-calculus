module SyntaxSugar.Parser where

import SyntaxSugar.Ast

import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr(buildExpressionParser, Assoc(..), Operator(..))
import Text.ParserCombinators.Parsec.Prim(Parser, (<|>), (<?>), parse, try)
import Text.ParserCombinators.Parsec.Combinator(many1, eof)
import Text.ParserCombinators.Parsec.Char(alphaNum, oneOf, letter)

lexer = P.makeTokenParser emptyDef{ commentStart = "--"
                                  , commentEnd = "\n"
                                  , identStart = letter
                                  , identLetter = alphaNum
                                  , opStart = oneOf "*+-:"
                                  , opLetter = oneOf "*+-:"
                                  , reservedOpNames = ["+", "*", "-"]
                                  , reservedNames = ["true", "false", "nop",
                                                      "if", "then", "else", "fi",
                                                      "while", "do", "od"]
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
parseExp = parseLambda

parseLambda' :: Parser Exp
parseLambda' = do m_symbol "\\"
                  boundVar <- m_identifier
                  m_symbol "->"
                  body <- parseLambda
                  return $ Abs boundVar body

parseLambda :: Parser Exp
parseLambda = parseLambda'
          <|> parseOp'

parseApp :: Parser Exp
parseApp = do xs <- many1 $ parseAtom
              return $ foldl1 App xs

parseAtom :: Parser Exp
parseAtom = parseNum <|> parseVar <|> m_parens parseExp

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
        ]

parse :: String -> Program
parse c = case Text.ParserCombinators.Parsec.Prim.parse (parseProgram <* eof) "" c of
  Left err -> error (show err)
  Right e  -> e

parseDefinition :: Parser Definition
parseDefinition = do id <- m_identifier
                     m_symbol "="
                     e <- parseExp
                     return $ Def id e

parseProgram :: Parser Program
parseProgram = P <$> many1 parseDefinition
