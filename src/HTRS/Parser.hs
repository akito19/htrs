module HTRS.Parser where

import HTRS.Term
import Data.List
import Text.ParserCombinators.Parsec
import System.Environment

parseTerm :: Parser Term
parseTerm = parseT1

parseT1 :: Parser Term
parseT1 = do
    ts <- many1 parseT2
    return (foldl1 App ts)

parseT2 :: Parser Term
parseT2 = try parseVariable
          <|> try parseConstant
          <|> try parseNumber
          <|> try parseList
          <|> try parseParent

parseDigits :: Parser String
parseDigits = do
    spaces
    x <- many1 digit
    spaces
    return x

parseUident :: Parser String
parseUident = do
    spaces
    c <- upper
    x <- many alphaNum
    spaces
    return (c : x)

parseLident :: Parser String
parseLident = do
    spaces
    c <- lower
    x <- many alphaNum
    spaces
    return (c : x)

convertNumToTerm :: Int -> Term
convertNumToTerm n = consistTerm $ replicate n (Con "s")

consistTerm :: [Term] -> Term
consistTerm []       = (Con "0")
consistTerm (p : ps) = (App p (consistTerm ps))

consistList :: [Term] -> Term
consistList []       = (Con "nil")
consistList (p : ps)
  | null ps   = (App (App (Con "cons") p) (Con "nil"))
  | otherwise = (App (App (Con "cons") p) (consistList ps))

parseKeyword :: String -> Parser ()
parseKeyword s = do
    string s
    spaces
    return ()

parseVariable :: Parser Term
parseVariable = do
    x <- parseUident
    return (Var x)

parseNumber :: Parser Term
parseNumber = do
    x <- try parseDigits
    let n = read x :: Int
    return $ convertNumToTerm n

parseConstant :: Parser Term
parseConstant = do
    x <- try parseLident
    return (Con x)

parseList :: Parser Term
parseList = do
    parseKeyword "["
    nums <- sepBy parseTerm (parseKeyword ",")
    parseKeyword "]"
    return $ consistList nums

parseParent :: Parser Term
parseParent = do
    parseKeyword "("
    ts <- parseT1
    parseKeyword ")"
    return ts

parseRule :: Parser Rule
parseRule = do
    l <- parseTerm
    parseKeyword "="
    r <- parseTerm
    parseKeyword ";"
    return (l, r)

parseTRS :: Parser TRS
parseTRS = do
    trs <- many parseRule
    return trs

readTRS s
  | Right rs <- parse parseTRS "" s = rs
