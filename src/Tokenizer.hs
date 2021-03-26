module Tokenizer
( tokenize
, Token(..)
, Literal(..)
, TokenInfo(..)
, formatError
) where

import Control.Monad (guard, void)
import Data.Char
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Pos
import Text.Parsec.Char
import Text.Parsec.Combinator

-- Represent a token and its position in the source file
data TokenInfo = TokenInfo Token SourcePos deriving (Eq, Show)

-- Represent a token
data Token
  = Ident String
  | Operator String
  | Keyword String
  | Literal Literal
  | NewLine -- removed after second phase of tokenizer, should probably enforce it through types
  deriving (Eq, Show)

data Literal
 = FloatLit String
 | IntLit String
 | StringLit String
 | RuneLit Char
 | BoolLit Bool
 deriving (Eq, Show)
    
formatError :: ParseError -> String
formatError e = "Error: invalid token at " ++ show e

-- List of keywords
keywords :: [String]
keywords = [ "break", "case", "chan", "const", "continue", "default", "defer"
           , "else", "fallthrough", "for", "func", "go", "goto", "if"
           , "import", "interface", "map", "package", "range", "return"
           , "select", "struct", "switch", "type", "var", "print", "println"
           ]

-- List of operators in order of priority
operators :: [String]
operators = [ "+=", "-=", "&=", "^=", "*=", "/=", "%=", "!=", "<=", ">=", "=="
            , ">>=", "<<=", "&^=", "|=", "&&", "||", "++", "--", "+", "-"
            , "&^", "&", "^", "*", "/", "%", "!", "<<", ">>", "<", ">", "|"
            , ":=", "=", "...", "(", ")", "[", "]", "{", "}", ",", ";", ".", ":"
            ]

-- Single line comment
lineComment :: Parser Token
lineComment = lexeme $ do
  string "//"
  manyTill anyChar $ try eol
  return NewLine
  where eol = (void $ string "\r\n") <|> (void $ char '\n') <|> eof

-- Multi line comment
genComment :: Parser Token
genComment = lexeme $ do
  string "/*"
  manyTill anyChar (try $ string "*/")
  return NewLine

newLine :: Parser Token
newLine = lexeme $ (void (char '\n') <|> void (string "\r\n")) *> return NewLine

-- Consume whitespaces 
whitespaces :: Parser ()
whitespaces = skipMany $ try whitespace
  where whitespace = void $ oneOf " \t"

-- Generate a parser that consumes and ignore trailing whitespace
lexeme :: Parser a -> Parser a
lexeme p = p <* whitespaces

-- Read an identifier
ident :: Parser Token
ident = lexeme $ do
  x <- asciiLetter <|> char '_'
  xs <- many (asciiLetter <|> digit <|> char '_')
  return $ Ident (x:xs)
  where asciiLetter = oneOf "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM"

hexChar :: Char -> Int
hexChar 'A' = 10
hexChar 'a' = 10
hexChar 'B' = 11
hexChar 'b' = 11
hexChar 'C' = 12
hexChar 'c' = 12
hexChar 'D' = 13
hexChar 'd' = 13
hexChar 'E' = 14
hexChar 'e' = 14
hexChar 'F' = 15
hexChar 'f' = 15
hexChar a@_ = digitToInt a

intLit :: Parser Token
intLit = lexeme $ hexLit <|> octalLit <|> decimalLit
  where
    ok = return . Literal . IntLit
    decimalLit = do
      x <- oneOf "123456789"
      xs <- many digit
      ok (x:xs)
    octalLit = try $ do
      char '0'
      x <- many $ oneOf "01234567"
      ok $ show $ foldl (\a b -> 8 * a + hexChar b) 0 x
    hexLit = try $ do
      char '0'
      oneOf "xX"
      x <- many1 $ oneOf "0123456789abcdefABCDEF"
      ok $ show $ foldl (\a b -> 16 * a + hexChar b) 0 x

floatLit :: Parser Token
floatLit = lexeme $ do
  left <- many digit
  char '.'
  right <- many digit
  if left == [] && right == []
    then parserFail ""
    else return $ Literal $ FloatLit $ "0" ++ left ++ "." ++ right

rawStringLit :: Parser Token
rawStringLit = lexeme $ do
  char '`'
  body <- manyTill anyChar (try $ char '`')
  return $ Literal $ StringLit body

interpretedStringLit :: Parser Token
interpretedStringLit = lexeme $ do
  char '"'
  x <- manyTill stringChar (try $ char '"')
  return $ Literal $ StringLit x
  where
    stringChar = escapeSeq <|> unicodeChar
    escapeSeq = do
      char '\\'
      choice (map (\(x,y) -> char x *> return y) escapeSeqs)
    escapeSeqs =
      [ ('a', '\a')
      , ('b', '\b')
      , ('f', '\f')
      , ('n', '\n')
      , ('r', '\r')
      , ('t', '\t')
      , ('v', '\v')
      , ('\\', '\\')
      , ('"', '"')
      ]
    unicodeChar = noneOf "\n" 

runeLit :: Parser Token
runeLit = lexeme $ do
  char '\''
  s <- escapeSeq <|> unicodeChar
  char '\''
  return $ Literal $ RuneLit s
  where
    escapeSeq = do
      char '\\'
      choice (map (\(x,y) -> char x *> return y) escapeSeqs)
    escapeSeqs =
      [ ('a', '\a')
      , ('b', '\b')
      , ('f', '\f')
      , ('n', '\n')
      , ('r', '\r')
      , ('t', '\t')
      , ('v', '\v')
      , ('\\', '\\')
      , ('\'', '\'')
      ]
    unicodeChar = noneOf "\n'" 

boolLit :: Parser Token
boolLit = lexeme $ trueLit <|> falseLit
  where trueLit  = string "true"  >>= (\_ -> return $ Literal $ BoolLit True)
        falseLit = string "false" >>= (\_ -> return $ Literal $ BoolLit False)

-- Generate a parser for a keyword
keyword :: String -> Parser Token
keyword k = try $ lexeme $ string k <* notFollowedBy (alphaNum <|> char '_') >>= return . Keyword

-- Generate a parser for an operator
operator :: String -> Parser Token
operator k = try $ lexeme $ string k >>= return . Operator

-- Parse a single token
token :: Parser TokenInfo
token = do
  pos <- getPosition
  t <- choice tokens
  return $ TokenInfo t pos
  where
    tokens = 
      [ choice (map keyword keywords)
      , try lineComment
      , try genComment
      , newLine
      , ident
      , try floatLit, intLit, rawStringLit, interpretedStringLit, runeLit
      , choice (map operator operators)
      ]

-- Tokenize the complete input
tokenize :: String -> Either ParseError [TokenInfo]
tokenize src = parse parser "" src >>= Right . insertSemiColons
  where parser = between whitespaces eof $ many Tokenizer.token

-- Remove NewLines tokens and replace them with semicolons where appropriate
insertSemiColons :: [TokenInfo] -> [TokenInfo]
insertSemiColons [] = []
insertSemiColons [tokInfo]
  | tok == NewLine   = []
  | shouldInsert tok = tokInfo : [TokenInfo (Operator ";") (incSourceColumn pos 1)]
  | otherwise        = [tokInfo]
  where TokenInfo tok pos = tokInfo
insertSemiColons ((TokenInfo NewLine _):xs) = insertSemiColons xs
insertSemiColons (tokInfo:(TokenInfo NewLine pos):xs) =
  if shouldInsert tok
    then tokInfo : (TokenInfo (Operator ";") pos) : insertSemiColons xs
    else tokInfo : insertSemiColons xs
  where TokenInfo tok _ = tokInfo
insertSemiColons (x:xs) = x : insertSemiColons xs

shouldInsert :: Token -> Bool
shouldInsert (Literal _)   = True
shouldInsert (Ident _)     = True
shouldInsert (Keyword k)   = k `elem` [ "break", "continue", "fallthrough", "return" ]
shouldInsert (Operator op) = op `elem` [ "++", "--", ")", "]", "}" ]
shouldInsert (otherwise)   = False
