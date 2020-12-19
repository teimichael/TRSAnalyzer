module Parser
    ( readTRSFile
    ) where

import Core.TRS
import Text.ParserCombinators.Parsec
-- auxiliary functions

convert :: [String] -> TRS -> TRS    
convert xs trs = substituteTRS trs sigma
    where sigma = [ (x, F x []) | x <- variablesTRS trs, not (elem x xs) ]
        
-- Scanners

identifier :: Parser String
identifier = do
  x <- many1 (noneOf "(), \t\r\n")
  spaces
  return x

keyword :: String -> Parser ()
keyword s = do
  _ <- string s
  spaces
  return ()
            
parseTerm :: Parser Term
parseTerm = try parseFunction <|> parseVariable
 
parseVariable :: Parser Term
parseVariable = do
  x <- identifier
  return (V x)

parseFunction :: Parser Term
parseFunction = do
  f <- identifier
  keyword "("
  t <- sepBy parseTerm (keyword ",")
  keyword ")"
  return (F f t)

parseRule :: Parser Rule
parseRule = do
  l <- parseTerm
  keyword "->"
  r <- parseTerm
  return (l, r)

parseVAR :: Parser ([String], TRS)
parseVAR = do
  keyword "VAR"
  xs <- many identifier
  return (xs, [])
          
parseRULES :: Parser ([String], TRS)
parseRULES = do
  keyword "RULES"
  rs <- many parseRule
  return ([], rs)


parseAnything :: Parser ()
parseAnything =
  do { _ <- identifier; return () } <|>
  do { keyword "("; _ <- many parseAnything; keyword ")" }

parseComment :: Parser ([String], TRS)
parseComment = do
  _ <- many parseAnything
  return ([], [])

parseSection :: Parser ([String], TRS)
parseSection = do
  keyword "("
  (xs, trs) <- try parseVAR <|> try parseRULES <|> parseComment
  keyword ")"
  return (xs, trs)


parseTRS :: Parser TRS
parseTRS = do
  spaces
  ps <- many parseSection
  eof
  let (xss, trss) = unzip ps in
    return (convert (concat xss) (concat trss))

readTRSFile :: String -> IO (Either ParseError TRS)
readTRSFile path = parseFromFile parseTRS path
