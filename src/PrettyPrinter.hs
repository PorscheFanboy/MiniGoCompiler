module PrettyPrinter
( prettyPrint
) where

import Control.Monad.Trans.State
import Data.List
import Parser hiding (printStmt)
import Tokenizer

data PrettyPrinter = PrettyPrinter Int String

type PPState = State PrettyPrinter

indent :: PPState a -> PPState a
indent p = incIndentation *> p <* decIndentation

incIndentation :: PPState ()
incIndentation = do
  PrettyPrinter indent src <- get
  put $ PrettyPrinter (indent+1) src

decIndentation :: PPState ()
decIndentation = do
  PrettyPrinter indent src <- get
  put $ PrettyPrinter (indent-1) src

-- Create a new line and indents it
line :: String -> PPState ()
line s = do
  PrettyPrinter indent src <- get
  let src' = src ++ "\n" ++ (replicate indent '\t') ++ s
  put $ PrettyPrinter indent src'

-- Append text to the current line
text :: String -> PPState ()
text s = do
  PrettyPrinter indent src <- get
  let src' = src ++ s
  put $ PrettyPrinter indent src'

prettyPrint :: Program -> String
prettyPrint prog = src
  where PrettyPrinter _ src = execState (printProg prog) (PrettyPrinter 0 "")

printProg :: Program -> PPState ()
printProg (Program (Package pkg) topLevelDecls) = do
  line $ "package " ++ pkg ++ ";"
  line ""
  mapM_ (printTopLevelDecl) topLevelDecls

printTopLevelDecl :: TopLevelDecl -> PPState ()
printTopLevelDecl (FuncDecl f) = line "" *> printFuncDecl f
printTopLevelDecl (Declaration (TypeDecl t)) = printTypeDecl t
printTopLevelDecl (Declaration (VarDecl v)) = printVarDecl v

printFuncDecl :: FuncDecl -> PPState ()
printFuncDecl (BasicFunc name sig body) = do
  line $ "func " ++ name ++ formatSignature sig ++ " {"
  indent (mapM_ (printStmt line) body)
  line "}"
  where
    formatSignature (FuncSignature ps r) = 
      "(" ++ intercalate ", " (map formatParam ps) ++ ") " ++ formatType r
    formatParam (ps, t) = intercalate ", " ps ++ " " ++ formatType t


printTypeDecl :: TypeDecl -> PPState ()
printTypeDecl (SingleTypeSpec (TypeSpec s (StructType st))) = do
    line $ "type " ++ s ++ " struct {"
    indent $ mapM_ printStruct st
    line $ "}"
printTypeDecl (SingleTypeSpec ts) = do
    printType "type " ts
printTypeDecl (MultiTypeSpec lst) = do
    line $ "type ("
    indent $ mapM_ (printType "") lst
    line $ ")"

printType :: String -> TypeSpec -> PPState ()
printType prefix (TypeSpec s t) = do
    line $ prefix ++ s ++ " " ++ formatType t

printStruct :: ([String], Type) -> PPState()
printStruct (strs, typ) = do
    line $ formatList strs ++ " " ++ formatType typ

printVarDecl :: VarDecl -> PPState ()
printVarDecl (SingleVarDecl varSpec) = do
    printDecl "var " varSpec
printVarDecl (MultiVarDecl decls) = do
    line $ "var ("
    indent $ mapM_ (printDecl "") decls
    line $ ")"

printDecl :: String -> VarSpec -> PPState ()
printDecl prefix (VarSpec strs exprs t) = do
    case exprs of
        [] -> line $ prefix ++ formatList strs ++ " " ++ formatType t
        _ -> line $ prefix ++ formatList strs ++ " " ++ formatType t ++ " = " ++ formatExprList exprs

printStmt :: (String -> PPState()) -> Stmt -> PPState ()
printStmt separator statement =
    case statement of
        AssignStmt e1 e2 -> do
            separator $ formatExprList e1 ++ " = " ++ formatExprList e2
        AssignOpStmt op e1 e2 -> do
            separator $ formatExpression e1 ++ " " ++ op ++ " " ++ formatExpression e2
        DeclarationStmt (VarDecl (SingleVarDecl (VarSpec strs exprs t))) -> do
            separator $ "var " ++ formatList strs ++ " " ++ formatType t ++ " = " ++ formatExprList exprs
        DeclarationStmt (TypeDecl td) -> do printTypeDecl td
        ReturnStmt Nothing -> do separator $ "return;"
        ReturnStmt (Just e) -> do separator $ "return " ++ formatExpression e ++ ";"
        BreakStmt -> do separator $ "break;"
        ContinueStmt -> do separator $ "continue;"
        ForConditionStmt e body -> do
            separator $ "for " ++ formatExpression e ++ " {"
            indent (mapM_ (printStmt line) body)
            separator "}"
        ForClauseStmt s1 e s2 body -> do
            separator $ "for "
            printStmt text s1
            text $ "; " ++ formatExpression e ++ ";"
            printStmt text s2
            text $ " {"
            indent (mapM_ (printStmt line) body)
            separator "}"
        IncStmt e -> do separator $ formatExpression e ++ "++"
        DecStmt e -> do separator $ formatExpression e ++ "--"
        ShortVarDeclStmt strs exprs -> do 
            separator $ formatList strs ++ " := " ++ formatExprList exprs
        PrintStmt exprs -> do
            line $ "print" ++ "(" ++ formatExprList exprs ++ ")"
        PrintLnStmt exprs -> do
            line $ "println" ++ "(" ++ formatExprList exprs ++ ")"
        ExprStmt expr -> do
            line $ formatExpression expr
        IfStmt (x:xs) elseStmt -> do
            line $ ""
            printIfs "if " x
            mapM_ (printIfs " else if ") xs
            printElse elseStmt
        SwitchStmt (Just stmt) (Just expr) stmts -> do
            line $ "switch "
            printStmt text stmt
            text $ "; " ++formatExpression expr ++ " {"
            indent $ mapM_ printCases stmts
            line $ "}"
        SwitchStmt Nothing (Just expr) stmts -> do
            line $ "switch " ++ formatExpression expr ++ " {"
            indent $ mapM_  printCases stmts
            line $ "}"
        SwitchStmt (Just stmt) Nothing stmts -> do
            line $ "switch "
            printStmt text stmt
            text $ "; {"
            indent $ mapM_ printCases stmts
            line $ "}"
        SwitchStmt Nothing Nothing stmts -> do
            line $ "switch {"
            indent $ mapM_ printCases stmts
            line $ "}"
        EmptyStmt -> text " "
        _ -> do
            separator $ ";"

printCases :: (SwitchCase, [Stmt]) -> PPState ()
printCases (SwitchDefaultCase, stmts) = do
    line $ "default: "
    indent $ mapM_ (printStmt line) stmts
printCases ((SwitchExprCase exprs), stmts) = do
    line $ "case " ++ formatExprList exprs ++ " :"
    indent $ mapM_ (printStmt line) stmts



printIfs :: String -> ((Maybe Stmt), Expr, [Stmt]) -> PPState ()
printIfs prefix ((Just init), expr, stmts) = do
    text $ prefix
    printStmt text init 
    text $ "; " ++ formatExpression expr ++ "{"
    indent $ mapM_ (printStmt line) stmts
    line $ "}"
printIfs prefix (Nothing, expr, stmts) = do
    text $ prefix
    text $ formatExpression expr ++ " {"
    indent $ mapM_ (printStmt line) stmts
    line $ "}"


printElse :: (Maybe [Stmt]) -> PPState ()
printElse (Just stmts) = do
    text $ " else {"
    indent $ mapM_ (printStmt line) stmts
    line $ "}"
printElse Nothing = do
    return ()





formatType :: Type -> String
formatType VoidType = ""
formatType UndefinedType = ""
formatType (UserType s) = s
formatType (IntType) = "int"
formatType (FloatType) = "float64"
formatType (StringType) = "string"
formatType (RuneType) = "rune"
formatType (BoolType) = "boolean"
formatType (SliceType t) = "[]" ++ formatType t
formatType (ArrayType l t) = "[" ++ show l ++ "] " ++ formatType t
formatType _ = ""



formatExpression :: Expr -> String
formatExpression (VariableExpr _ _ str) = str
formatExpression (LitExpr _ (IntLit x)) = x
formatExpression (LitExpr _ (FloatLit x)) = x
formatExpression (LitExpr _ (StringLit str)) = show str
formatExpression (LitExpr _ (BoolLit x)) = show x
formatExpression (BinopExpr _ op e1 e2) = "(" ++ formatExpression e1 ++ " " ++ op ++ " " ++ formatExpression e2 ++ ")"
formatExpression (UnopExpr _ op e) = op ++ "(" ++ formatExpression e ++ ")"
formatExpression (AppendExpr _ e1 e2) = "append(" ++ formatExpression e1 ++ ", " ++ formatExpression e2 ++ ")"
formatExpression (ConversionExpr _ t e) = formatType t ++ "(" ++ formatExpression e ++ ")"
formatExpression (FuncCall _ e lst) = formatExpression e ++ "(" ++ formatExprList lst ++ ")"
formatExpression (IndexExpr _ e1 e2) = formatExpression e1 ++ "[" ++ formatExpression e2 ++ "]"
formatExpression (SelectorExpr _ e1 str) = formatExpression e1 ++ "." ++ str
formatExpression _ = ""



formatList :: [String] -> String
formatList [] = ""
formatList [x] = x
formatList (x:xs) = x ++ ", " ++ formatList xs

formatExprList :: [Expr] -> String
formatExprList [] = ""
formatExprList [x] = formatExpression x
formatExprList (x:xs) = formatExpression x ++ ", " ++ formatExprList xs