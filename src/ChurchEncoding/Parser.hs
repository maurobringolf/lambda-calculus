module ChurchEncoding.Parser where

import ChurchEncoding.Ast

import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr(buildExpressionParser, Assoc(..), Operator(..))
import Text.ParserCombinators.Parsec.Prim(Parser, (<|>), (<?>), parse, try, many)
import Text.ParserCombinators.Parsec.Combinator(many1, eof, sepBy, sepBy1)
import Text.ParserCombinators.Parsec.Char(alphaNum, oneOf, letter, newline, satisfy)

import Data.Char(isSpace, isUpper, isLower)
import Data.Either(lefts, rights)
import Data.List(sortBy)

lexer = P.makeTokenParser emptyDef{ commentStart = "--"
                                  , commentEnd = "\n"
                                  , identStart = satisfy isLower
                                  , identLetter = alphaNum
                                  , opStart = oneOf "*+-=<>:"
                                  , opLetter = oneOf "*+-=<>:"
                                  , reservedOpNames = ["+", "*", "-", "==", "<=", ">=", ":"]
                                  , reservedNames = ["true", "false", "nop",
                                                      "if", "then", "else", "fi",
                                                      "while", "do", "od", "True", "False", "foldr", "tail", "data", "case", "of"]
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
parseExp = parseLambda <|> parseIfElse <|> parseCaseOf

parseCaseOf :: Parser Exp
parseCaseOf = do m_reservedOp "case"
                 t <- parseExp
                 m_reservedOp "of"
                 ps <- many1 (do p <- parsePattern
                                 m_symbol "->"
                                 e <- parseExp
                                 m_symbol ";"
                                 return (p,e)) 
                 return $ CaseOf t (sortByConsName ps)

sortByConsName :: [(Exp, Exp)] -> [(Exp, Exp)]
sortByConsName = sortBy (\a b -> compare (getConsName $ fst a) (getConsName $ fst b))

parsePattern :: Parser Exp
parsePattern = parseApp

capitalized :: Parser String
capitalized = do c <- satisfy isUpper
                 cs <- many letter
                 m_whiteSpace
                 return $ c:cs

parseConstructor :: Parser Exp
parseConstructor = do c <- capitalized
                      return $ Constructor c

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
                  boundVars <- many1 m_identifier
                  m_symbol "->"
                  body <- parseExp
                  return $ foldr Abs body boundVars

parseLambda :: Parser Exp
parseLambda = parseLambda'
          <|> parseOp'

parseApp :: Parser Exp
parseApp = do xs <- many1 $ parseAtom
              return $ foldl1 App xs

parseAtom :: Parser Exp
parseAtom = parseKeyword <|> parseList <|> parseBool <|> parseNum <|> parseVar <|> m_parens parseExp <|> parseConstructor

parseKeyword = try (m_symbol "foldr" >> return Foldr)
           <|> try (m_symbol "tail" >> return Tail)


parseList :: Parser Exp
parseList = do m_symbol "["
               xs <- sepBy parseExp (m_symbol ",")
               m_symbol "]"
               return $ List xs

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
          [ Infix (m_reservedOp ":" >> return (\l -> \r ->(App (App Cons l) r)))  AssocRight]
        , [ Infix (m_reservedOp "*" >> return (\l -> \r ->(App (App Mult l) r)))  AssocRight]
        , [ Infix (m_reservedOp "+" >> return (\l -> \r ->(App (App Add l) r)))  AssocLeft, Infix (m_reservedOp "-" >> return (\l -> \r ->(App (App Sub l) r)))  AssocLeft]
        , [ Infix (m_reservedOp "==" >> return (\l -> \r ->(App (App Eq l) r)))  AssocNone, Infix (m_reservedOp "<=" >> return (\l -> \r ->(App (App Leq l) r)))  AssocNone, Infix (m_reservedOp ">=" >> return (\l -> \r ->(App (App Geq l) r)))  AssocNone]
        , [ Infix (m_reservedOp "&&" >> return (\l -> \r ->(App (App And l) r))) AssocRight]
        , [ Infix (m_reservedOp "||" >> return (\l -> \r ->(App (App Or l) r))) AssocRight]
        ]


parseE :: String -> Exp
parseE c = case Text.ParserCombinators.Parsec.Prim.parse (parseExp <* eof) "" c of Left err -> error (show err); Right e -> e

parseDef :: Parser (Either FunDef DataDef)
parseDef = (Left <$> parseFunDef) <|> (Right <$> parseDataDef)

parse :: String -> Program
parse c = let defs = splitDefs c
              tlds = map (\d -> (case Text.ParserCombinators.Parsec.Prim.parse (parseDef <* eof) "" d of Left err -> error (show err); Right e -> e)) defs
              funDefs = lefts tlds
              dataDefs = rights tlds
           in
           P funDefs dataDefs

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

parseDataDef :: Parser DataDef
parseDataDef = do m_symbol "data"
                  n <- capitalized
                  m_symbol "="
                  conss <- sepBy1 parseExp $ m_symbol "|"
                  return $ DDef n (sortBy (\a b -> compare (getConsName a) (getConsName b)) conss)

parseFunDef :: Parser FunDef
parseFunDef = do id <- m_identifier
                 boundVars <- many m_identifier
                 m_symbol "="
                 e <- parseExp
                 return $ Def id $ foldr Abs e boundVars
