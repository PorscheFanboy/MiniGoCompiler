module Parser
where

import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Pos
import Text.Parsec.Prim

import Tokenizer

---------------
-- Utilities --
---------------
            
type Parser = Parsec [TokenInfo] ()

-- Parse a tokens list into an AST
parseTokens :: [TokenInfo] -> Either ParseError Program
parseTokens = Text.Parsec.Prim.parse program ""

-- Call `f` on the next token
nextTok :: (Token -> Maybe a) -> Parser a
nextTok f = token showTok posFromTok testTok
    where posFromTok (TokenInfo tok pos) = pos
          showTok (TokenInfo tok pos) = show tok
          testTok (TokenInfo tok pos) = f tok

-- Read an identifier
ident :: Parser String
ident = nextTok (\x -> case x of Ident s   -> Just s
                                 otherwise -> Nothing)

-- Read a token without parameter
tok :: Token -> Parser Token
tok x = nextTok (\t -> if t == x then Just t
                                 else Nothing)

-- Read an operator
op :: String -> Parser String
op x = tok (Operator x) *> return x

-- Read a keyword
keyword :: String -> Parser String
keyword x = tok (Keyword x) *> return x

literal :: Parser Literal
literal = nextTok (\x -> case x of Literal l -> Just l
                                   otherwise -> Nothing)

parens :: Parser a -> Parser a
parens = between (op "(") (op ")")

braces :: Parser a -> Parser a
braces = between (op "{") (op "}")

formatError :: ParseError -> String
formatError e = "Error: failed to parse at " ++ show e

-----------------------
-- Program structure --
-----------------------

data Program = Program PackageDecl [TopLevelDecl] deriving (Eq, Show)

data PackageDecl = Package String deriving (Eq, Show)

data TopLevelDecl
  = Declaration Declaration
  | FuncDecl FuncDecl
  deriving (Eq, Show)

data Declaration
  = TypeDecl TypeDecl
  | VarDecl VarDecl
  deriving (Eq, Show)

program :: Parser Program
program = do
  pkgDecl <- packageDecl
  op ";"
  topLevelDecls <- many (topLevelDecl <* op ";")
  eof
  return $ Program pkgDecl topLevelDecls

packageDecl :: Parser PackageDecl
packageDecl = do
  keyword "package"
  pkg <- ident
  return $ Package pkg

topLevelDecl :: Parser TopLevelDecl
topLevelDecl = (declaration >>= (return . Declaration))
             <|> (funcDecl >>= (return . FuncDecl))

declaration :: Parser Declaration
declaration = (varDecl >>= return . VarDecl)
            <|> (typeDecl >>= return . TypeDecl)

-----------
-- Types --
-----------

data Type
  = UndefinedType
  | UserType String
  | TypeDef Int String Type
  | StructType [([String], Type)]
  | ArrayType Int Type
  | SliceType Type
  | VoidType
  | IntType
  | FloatType
  | StringType
  | RuneType
  | BoolType
  | FuncType FuncSignature
  | TypeCast Type
  deriving (Eq, Show)

type' :: Parser Type
type' = choice 
  [ try arrayType
  , between (op "(") (op ")") type'
  , sliceType
  , structType
  , ident >>= (return . UserType)
  ]

arrayType :: Parser Type
arrayType = do
  op "["
  lit <- literal
  op "]"
  elType <- type'
  case lit of
    IntLit l -> return $ ArrayType (read l) elType
    otherwise -> fail ""

sliceType :: Parser Type
sliceType = op "[" *> op "]" *> type' >>= return . SliceType

structType :: Parser Type
structType = do
  keyword "struct"
  fields <- braces (many fieldDecl)
  return $ StructType fields
  where
    fieldDecl = do
      ns <- sepBy1 ident (op ",")
      t <- type'
      op ";"
      return (ns, t)

data TypeDecl
  = SingleTypeSpec TypeSpec
  | MultiTypeSpec [TypeSpec]
  deriving (Eq, Show)

data TypeSpec = TypeSpec String Type deriving (Eq, Show)

typeDecl :: Parser TypeDecl
typeDecl = keyword "type" *> (singleTypeSpec <|> multiTypeSpec)
  where multiTypeSpec = parens (many (typeDef <* op ";")) >>= return . MultiTypeSpec
        singleTypeSpec  = typeDef >>= return . SingleTypeSpec

typeDef = do
  x <- ident
  t <- type'
  return $ TypeSpec x t

---------------
-- Variables --
---------------

data VarDecl
  = SingleVarDecl VarSpec
  | MultiVarDecl [VarSpec]
  deriving (Eq, Show)

data VarSpec = VarSpec [String] [Expr] Type deriving (Eq, Show)

varDecl :: Parser VarDecl
varDecl = (try $ keyword "var" *> varSpec >>= return . SingleVarDecl)
        <|> (keyword "var" *> parens (many $ varSpec <* op ";") >>= return . MultiVarDecl)

varSpec :: Parser VarSpec
varSpec = do
  names <- sepBy1 ident (op ",")
  vType <- option UndefinedType type'
  values <- option [] (op "=" *> exprs1)
  return $ VarSpec names values vType

---------------
-- Functions --
---------------
data FuncDecl 
  = BasicFunc String FuncSignature [Stmt]
  | NullFuncDecl
  deriving (Eq, Show)

data FuncSignature = FuncSignature [([String], Type)] Type deriving (Eq, Show)

funcDecl :: Parser FuncDecl
funcDecl = do
  keyword "func"
  name <- ident
  signature <- funcSignature
  body <- block
  return $ BasicFunc name signature body

funcSignature :: Parser FuncSignature
funcSignature = do
  ps <- parens (sepBy param (op ","))
  ret <- option VoidType type'
  return $ FuncSignature ps ret
  where param  = do names <- sepBy1 ident (op ",")
                    t <- type'
                    return (names, t)

----------------
-- Statements --
----------------
data Stmt
  = DeclarationStmt Declaration
  | ReturnStmt (Maybe Expr)
  | BreakStmt
  | ContinueStmt
  | BlockStmt [Stmt]
  | IfStmt [(Maybe Stmt, Expr, [Stmt])] (Maybe [Stmt])
  | SwitchStmt (Maybe Stmt) (Maybe Expr) [(SwitchCase, [Stmt])]
  | ForConditionStmt Expr [Stmt]
  | ForClauseStmt Stmt Expr Stmt [Stmt]
  | ExprStmt Expr
  | IncStmt Expr
  | DecStmt Expr
  | EmptyStmt
  | AssignStmt [Expr] [Expr]
  | AssignOpStmt String Expr Expr
  | ShortVarDeclStmt [String] [Expr]
  | PrintStmt [Expr]
  | PrintLnStmt [Expr]
  | NullStmt
  deriving (Eq, Show)

data SwitchCase
  = SwitchDefaultCase
  | SwitchExprCase [Expr]
  deriving (Eq, Show)

data AssignOp = AssignEqual | AssignAdd | AssignMult deriving (Eq, Show)

block :: Parser [Stmt]
block = braces (many (stmt <* op ";"))

stmt :: Parser Stmt
stmt = choice $ map try
  [ printStmt
  , printLnStmt
  , block >>= return . BlockStmt
  , try $ declaration >>= return . DeclarationStmt
  , ifStmt
  , forClauseStmt
  , forConditionStmt
  , keyword "break" *> return BreakStmt
  , keyword "continue" *> return ContinueStmt
  , returnStmt
  , switchStmt
  , simpleStmt
  ]

simpleStmt :: Parser Stmt
simpleStmt = choice $ map try
  [ shortVarDeclStmt
  , assignOpStmt
  , incStmt
  , decStmt
  , assignStmt
  , exprStmt
  , emptyStmt
  ]

exprStmt :: Parser Stmt
exprStmt = expr >>= return . ExprStmt

printStmt :: Parser Stmt
printStmt = keyword "print" *> parens exprs >>= return . PrintStmt

printLnStmt :: Parser Stmt
printLnStmt = keyword "println" *> parens exprs >>= return . PrintLnStmt

assignStmt :: Parser Stmt
assignStmt = do
  left <- exprs1
  op "="
  right <- exprs1
  return $ AssignStmt left right

shortVarDeclStmt :: Parser Stmt
shortVarDeclStmt = do
  left <- sepBy1 ident (op ",")
  op ":="
  right <- exprs1
  return $ ShortVarDeclStmt left right

incStmt :: Parser Stmt
incStmt = expr <* op "++" >>= return . IncStmt

decStmt :: Parser Stmt
decStmt = expr <* op "--" >>= return . DecStmt

forConditionStmt :: Parser Stmt
forConditionStmt = do
  keyword "for"
  cond <- option (LitExpr UndefinedType (BoolLit True)) expr
  body <- block
  return $ ForConditionStmt cond body

forClauseStmt :: Parser Stmt
forClauseStmt = do 
  keyword "for"
  init <- option EmptyStmt simpleStmt
  op ";"
  cond <- option (LitExpr UndefinedType (BoolLit True)) expr
  op ";"
  post <- option EmptyStmt simpleStmt
  body <- block
  return $ ForClauseStmt init cond post body

assignOpStmt :: Parser Stmt
assignOpStmt = do
  left <- expr
  assignOp <- choice (map (\x -> op ( x ++ "=")) ops)
  right <- expr
  return $ AssignOpStmt assignOp left right
  where ops = [ "+", "-", "|", "^", "*", "/", "%", "<<", ">>", "&", "&^" ]

returnStmt :: Parser Stmt
returnStmt = keyword "return" *> optionMaybe expr >>= return . ReturnStmt

ifStmt :: Parser Stmt
ifStmt = do
  ifClause <- ifStmt'
  elseIfClauses <- many (try $ keyword "else" *> ifStmt')
  elseClause <- optionMaybe (keyword "else" *> block)
  return $ IfStmt (ifClause:elseIfClauses) elseClause 

ifStmt' = try ifStmtInit <|> ifStmtNoInit

ifStmtInit = do
  keyword "if"
  init <- option EmptyStmt simpleStmt
  op ";"
  cond <- expr
  body <- block
  return (Just init, cond, body)

ifStmtNoInit = do
  keyword "if"
  cond <- expr
  body <- block
  return (Nothing, cond, body)

switchStmt :: Parser Stmt
switchStmt = do
  keyword "switch"
  init <- optionMaybe (try $ simpleStmt <* op ";")
  e <- optionMaybe (try expr)
  clauses <- braces (many clause)
  return $ SwitchStmt init e clauses
  where
    clause = do
      c <- switchCase
      op ":"
      bs <- (many (stmt <* op ";"))
      return (c, bs)
    switchCase = (keyword "case" *> exprs1 >>= return . SwitchExprCase)
              <|> (keyword "default" *> return SwitchDefaultCase)

emptyStmt :: Parser Stmt
emptyStmt = lookAhead (op ";") *> return EmptyStmt

-----------------
-- Expressions --
-----------------

data Expr
  = VariableExpr Type Type String
  | SelectorExpr Type Expr String
  | IndexExpr Type Expr Expr
  | ConversionExpr Type Type Expr
  | LitExpr Type Literal
  | BinopExpr Type String Expr Expr
  | UnopExpr Type String Expr
  | AppendExpr Type Expr Expr
  | FuncCall Type Expr [Expr]
  | NullExpr
  deriving (Eq, Show)

exprs :: Parser [Expr]
exprs = sepBy expr (op ",")

exprs1 = sepBy1 expr (op ",")

-- List of binary operators in precedence order
binops :: [[String]]
binops =
  [ [ "*", "/", "%", "<<", ">>", "&", "&^" ]
  , [ "+", "-", "|", "^" ]
  , [ "==", "!=", "<", "<=", ">", ">=" ]
  , [ "&&" ]
  , [ "||" ]
  ]

-- List of unary operators
unops :: [String]
unops = [ "+", "-", "!", "^" ]

expr :: Parser Expr
expr = foldl chainl1 unexpr (map mkBinops binops)

unexpr :: Parser Expr
unexpr = do
  unaryMod <- optionMaybe $ choice $ map op unops
  case unaryMod of
    Just op -> unexpr >>= return . (UnopExpr UndefinedType op)
    Nothing -> basicExpr >>= modifier
  where
    modifier e = choice $ map try
      [ funcCall e >>= modifier
      , selectorExpr e >>= modifier
      , indexExpr e >>= modifier
      , return e
      ]

funcCall :: Expr -> Parser Expr
funcCall e = do
  params <- parens exprs
  return $ FuncCall UndefinedType e params

selectorExpr :: Expr -> Parser Expr
selectorExpr e = do
  op "."
  selector <- ident
  return $ SelectorExpr UndefinedType e selector

indexExpr :: Expr -> Parser Expr
indexExpr e = do
  index <- between (op "[") (op "]") expr
  return $ IndexExpr UndefinedType e index

basicExpr :: Parser Expr
basicExpr = choice
  [ parens expr
  , try appendExpr
  , literal >>= return . LitExpr UndefinedType
  , identExpr
  ]

identExpr :: Parser Expr
identExpr = do
  x <- ident
  if x == "append" then
    parserFail ""
  else
    return (VariableExpr UndefinedType UndefinedType x)

appendExpr :: Parser Expr
appendExpr = do
  x <- ident
  if x == "append"
    then do
      op "("
      e1 <- expr
      op ","
      e2 <- expr
      op ")"
      return $ AppendExpr UndefinedType e1 e2
  else parserFail ""

conversion :: Parser Expr
conversion = do
  t <- type'
  e <- parens expr
  return $ ConversionExpr UndefinedType t e

-- Generate a parser for a list of binary expression with the same precedence
mkBinops :: [String] -> Parser (Expr -> Expr -> Expr)
mkBinops = choice . map (\x -> op x *> return (BinopExpr UndefinedType x))
