module Typechecker
( symbolTable
, SymbolTable(..)
, printSymbolTable
, flattenFuncParams
, getProgram
)
where

import Prelude hiding (print)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.List(find, intercalate)
import qualified Data.HashSet as Set
import Parser
import Tokenizer(Literal(..))

type Scope = [(String, Symbol)]

data SymbolTable = SymbolTable
  { scopes     :: [Scope]
  -- ^ stack containing each scope currently accessible
  , returnType :: Maybe Type
  -- ^ return type of the current function or Nothing
  , log        :: String
  -- ^ printed version of the table
  , returnOnAllPaths :: Bool
  -- ^ whether the current function has a return value on all paths
  , tree :: Program
  }

data Symbol
  = VariableSymbol Type
  | ConstantSymbol Type
  | TypeSymbol Type
  deriving (Eq, Show)

type TypeError = String

---------------------
-- Table formating --
---------------------

printSymbolTable :: SymbolTable -> String
printSymbolTable (SymbolTable _ _ log _ _) = log

getProgram :: SymbolTable -> Program
getProgram (SymbolTable _ _ _ _ prog) = prog

getExprType :: Expr -> TableState Type
getExprType (VariableExpr t _ _) = return t 
getExprType (SelectorExpr t _ _) = return t
getExprType (IndexExpr t _ _) = return t
getExprType (ConversionExpr t _ _) = return t
getExprType (LitExpr t _) = return t
getExprType (BinopExpr t _ _ _) = return t
getExprType (UnopExpr t _ _) = return t
getExprType (AppendExpr t _ _) = return t
getExprType (FuncCall t _ _) = return t

printSymbol :: (String, Symbol) -> String
printSymbol (name, VariableSymbol t) = name ++ " [variable] = " ++ formatType t
printSymbol (name, ConstantSymbol t) = name ++ " [constant] = " ++ formatType t
printSymbol (name, TypeSymbol t) = name ++ " [type] = " ++ formatType t
-- TODO: print defined types with arrows

indent :: [a] -> String
indent xs = replicate (length xs) '\t'

formatType :: Type -> String
formatType VoidType = "void"
formatType UndefinedType = "undefined"
formatType (TypeDef _ s t) = s ++ ":" ++ formatType t
formatType (UserType s) = s
formatType (IntType) = "int"
formatType (FloatType) = "float64"
formatType (StringType) = "string"
formatType (RuneType) = "rune"
formatType (BoolType) = "bool"
formatType (SliceType t) = "[]" ++ formatType t
formatType (StructType fields) =
  let joinFields (n, t) = intercalate ", " n ++ " " ++ formatType t
    in "struct { " ++ intercalate "; " (map joinFields fields) ++ "; }"
formatType (FuncType (FuncSignature params ret)) =
  let joinParams (n, t) = intercalate ", " $ map (\_ -> formatType t) n
    in "(" ++ intercalate ", " (map joinParams params)  ++ ") => " ++ formatType ret
formatType (ArrayType l t) = "[" ++ show l ++ "]" ++ formatType t
formatType t = show t

typeError = lift . Left
undefinedError n = lift $ Left $ "Cannot find symbol " ++ n ++ " in the visible scopes"
assignError t1 t2 = lift $ Left $
  "Cannot assign a value of type "
  ++ formatType t2
  ++ " to a variable of type "
  ++ formatType t1
initFunctionError = lift $ Left $ "`init` and `main` functions must be functions with no parameters or return type "

------------------------
-- State manipulation --
------------------------

-- Represent the current state.
type TableState = StateT SymbolTable (Either TypeError)

-- Add a symbol to the most recent scope
addSymbol :: String -> Symbol -> TableState ()
addSymbol name symbol = do
  addSymbolStealth name symbol
  if name /= "_"
    then print $ printSymbol (name, symbol)
    else return ()

-- Add a symbol to the table without printing it
addSymbolStealth :: String -> Symbol -> TableState ()
addSymbolStealth "_" symbol = return ()
addSymbolStealth name symbol = do
  SymbolTable scopes retType log ret prog <- get
  if length scopes == 2 && name `elem` ["main", "init"]
    then checkInitFunction symbol
    else return ()
  prevSymbol <- findCurrentSymbol name
  if prevSymbol /= Nothing
    then typeError $ "Symbol " ++ name ++ " is already defined in this scope"
    else do
      let (x:xs) = scopes
      let scopes' = ((name,symbol):x):xs
      if name == "init"
        then return ()
        else put $ SymbolTable scopes' retType log ret prog

-- Print a line with the indentation matching the current scope
print :: String -> TableState ()
print line = do
  SymbolTable scopes retType log ret prog <- get
  let log' = log ++ indent scopes ++ line ++ "\n"
  put $ SymbolTable scopes retType log' ret prog

-- Create a new scope
pushScope :: TableState ()
pushScope = do
  SymbolTable scopes retType log ret prog <- get
  let log' = log ++ indent scopes ++ "{\n"
  put $ SymbolTable ([]:scopes) retType log' ret prog

-- Pop the scope
popScope :: TableState ()
popScope = do
  SymbolTable scopes retType log ret prog <- get
  let (x:xs) = scopes
  let log' = log ++ indent xs ++ "}\n"
  put $ SymbolTable xs retType log' ret prog

-- Call f in its in own scope
scope :: TableState a -> TableState a
scope f = pushScope *> f <* popScope

-- Set the return type of the current function
setReturnType :: (Maybe Type) -> FuncDecl -> TableState FuncDecl
setReturnType retType fdecl = do
  SymbolTable scopes _ log ret prog <- get
  put $ SymbolTable scopes retType log ret prog
  return fdecl

getReturnType :: TableState (Maybe Type)
getReturnType = do
  SymbolTable _ retType _ _ _ <- get
  return retType

-- Find the symbol most recently associated to a name
findSymbol :: String -> TableState (Maybe Symbol)
findSymbol name = do
  SymbolTable scopes _ _ _ _ <- get
  case find (\(name', _) -> name == name') (concat scopes) of
    Just (n, s) -> return (Just s)
    Nothing -> return Nothing

-- Find a symbol if it is defined in the most recent scope
findCurrentSymbol :: String -> TableState (Maybe Symbol)
findCurrentSymbol name = do
  SymbolTable scopes _ _ _ _ <- get
  let (x:xs) = scopes
  case find (\(name', _) -> name == name') x  of
    Just (n, s) -> return (Just s)
    Nothing -> return Nothing

getReturn :: TableState Bool
getReturn = do
  SymbolTable scopes retType log ret _ <- get
  return ret

setReturn :: Bool -> Stmt -> TableState Stmt
setReturn ret' s = do
  SymbolTable scopes retType log ret prog <- get
  put $ SymbolTable scopes retType log ret' prog
  return s

getLevel :: TableState Int
getLevel = do
  SymbolTable scopes _ _ _ _ <- get
  return $ length scopes

-------------
-- General --
-------------

-- Entry point function
symbolTable :: Program -> Either TypeError SymbolTable
symbolTable (Program pkg decls) =
  execStateT (buildSymbolTable decls) (SymbolTable [] Nothing "" False (Program pkg []))

buildSymbolTable :: [TopLevelDecl] -> TableState ()
buildSymbolTable decls = do
  scope $ do
    addSymbol "int" (TypeSymbol IntType)
    addSymbol "float64" (TypeSymbol FloatType)
    addSymbol "rune" (TypeSymbol RuneType)
    addSymbol "bool" (TypeSymbol BoolType)
    addSymbol "string" (TypeSymbol StringType)
    addSymbol "true" (ConstantSymbol BoolType)
    addSymbol "false" (ConstantSymbol BoolType)
    scope $ do
      mapM_ typeTopLevelDecl decls

findType :: Type -> TableState Type
findType (UserType s) = do
  symb <- findSymbol s
  case symb of
    Just(TypeSymbol t) -> return t
    otherwise    -> typeError $ "Identifier " ++ s ++ " does not refer to a type within this scope"
findType (ArrayType l t) = do findType t >>= return . (ArrayType l)
findType (SliceType t) = findType t >>= return . SliceType
findType UndefinedType = return UndefinedType
findType (StructType fields) = mapM (\(s, t) -> findType t >>= \t' -> return (s, t')) fields >>= return . StructType
-- TODO: struct type
findType t = return t

resolveType :: Type -> TableState Type
resolveType t = do
  t' <- findType t
  case t' of
    TypeDef _ s td -> resolveType td
    t'             -> return t'

typeEquals :: Type -> Type -> TableState Bool
typeEquals _ VoidType = return False
typeEquals (StructType f1) (StructType f2) = do
  StructType f1' <- findType (StructType f1)
  StructType f2' <- findType (StructType f2)
  if flattenFuncParams f1' == flattenFuncParams f2'
    then return True
    else return False
typeEquals t1 t2 = do
  t1' <- findType t1
  t2' <- findType t2
  return $ t1' == t2'

isBaseType :: Expr -> TableState ()
isBaseType expr = do
  t1 <- typeExpr expr >>= getExprType
  t1' <- resolveType t1
  if t1' `elem` [IntType, FloatType, StringType, RuneType, BoolType]
    then return ()
    else typeError $ "Expects base types, received " ++ formatType t1'

checkInitFunction :: Symbol -> TableState ()
checkInitFunction (VariableSymbol (FuncType sig)) =
  if sig == FuncSignature [] VoidType
    then return ()
    else initFunctionError
checkInitFunction otherwise = initFunctionError

------------------
-- Declarations --
------------------

typeTopLevelDecl :: TopLevelDecl -> TableState ()
typeTopLevelDecl (Declaration decl) = do
  d <- typeDeclaration decl
  SymbolTable scopes retType log ret (Program pkg decls) <- get
  put $ SymbolTable scopes retType log ret (Program pkg (decls ++ [Declaration d]))
typeTopLevelDecl (FuncDecl f) = do
  d <- typeFuncDecl f
  SymbolTable scopes retType log ret (Program pkg decls) <- get
  put $ SymbolTable scopes retType log ret (Program pkg (decls ++ [FuncDecl d]))

typeDeclaration :: Declaration -> TableState Declaration
typeDeclaration (VarDecl (SingleVarDecl spec)) = do
  spec' <- typeVarSpec spec
  return $ VarDecl (SingleVarDecl spec')
typeDeclaration (VarDecl (MultiVarDecl specs)) = do
  specs' <- mapM typeVarSpec specs
  return $ (VarDecl (MultiVarDecl specs'))
typeDeclaration (TypeDecl (SingleTypeSpec spec)) = do
  spec' <- typeTypeSpec spec
  return $ TypeDecl (SingleTypeSpec spec')
typeDeclaration (TypeDecl (MultiTypeSpec specs)) = do
  specs' <- mapM typeTypeSpec specs
  return $ (TypeDecl (MultiTypeSpec specs'))

typeFuncDecl :: FuncDecl -> TableState FuncDecl
typeFuncDecl (BasicFunc name sig body) = do
  addSymbolStealth name symbol
  setReturnType (Just retType) NullFuncDecl
  setReturn False NullStmt
  print $ printSymbol (name, symbol)
  mapM_ (\(n, t) -> resolveType t) ps
  body' <- scope $ do
          mapM_ (\(n, t) -> addSymbol n (VariableSymbol t)) (flattenFuncParams ps)
          lst <- mapM typeStmt body
          return lst
  ret <- getReturn
  if ret || retType == VoidType
    then return (BasicFunc name sig body')
    else typeError ("Not all paths of function " ++ name ++ " return a value")
  setReturnType Nothing (BasicFunc name sig body')
  where FuncSignature ps retType = sig
        symbol = (VariableSymbol (FuncType sig))

typeTypeSpec :: TypeSpec -> TableState TypeSpec
typeTypeSpec (TypeSpec n t) = do
  
  l <- getLevel
  addSymbol n (TypeSymbol (TypeDef (l-1) n t))
  scope $ do
    t' <- findType t
    addSymbol n (TypeSymbol (TypeDef l n t'))
    return $ TypeSpec n t

typeVarSpec :: VarSpec -> TableState VarSpec
-- Form: var x1, ..., xn T
typeVarSpec (VarSpec left [] t)
  | t == UndefinedType = typeError "No type associated with the variable delcaration"
  | otherwise          = do
    t1 <- findType t
    mapM_ (\n -> addSymbol n (VariableSymbol t1)) left
    return (VarSpec left [] t)

-- Form: var x1, ..., xn = e1, ..., en
typeVarSpec (VarSpec left right UndefinedType) = do
  lst <- (flip mapM) (zip left right) $ \(n, e) -> do
            t1' <- typeExpr e
            t1 <- getExprType t1'
            if isValue t1
              then typeExpr e >>= getExprType >>= \t -> do
                addSymbol n (VariableSymbol t1)
                return (n, t1')
              else typeError "The right side of the declaration is not a value"
  return (VarSpec (fst (unzip lst)) (snd (unzip lst)) UndefinedType)



-- Form: var x1, ..., xn T = e1, ... en
typeVarSpec (VarSpec left right t1) = do
  lst <- (flip mapM) (zip left right) $ \(n, e) -> do
              t2' <- typeExpr e
              t2 <- getExprType t2'
              assignable <- t1 `typeEquals` t2
              t <- findType t1
              if assignable
                  then do
                    addSymbol n (VariableSymbol t)
                    return (n, t2')
                  else assignError t t2
  return (VarSpec (fst (unzip lst)) (snd (unzip lst)) t1)

----------------
-- Statements --
----------------

-- Check if a value can be assigned to an expression.
isAssignable :: Expr -> TableState Bool
isAssignable e =
  case e of
    VariableExpr _ _ n     ->
      if n == "_"
        then return True
        else do
          symb <- findSymbol n
          case symb of
            Just (VariableSymbol _) -> return True
            Just (ConstantSymbol _) -> return False
            Just (TypeSymbol _)     -> return False
            Nothing                 -> return False
    SelectorExpr _ x _   -> isAssignable x
    IndexExpr _ x _      -> isAssignable x
    ConversionExpr _ _ _ -> return False
    LitExpr _ _          -> return False
    BinopExpr _ _ _ _    -> return False
    UnopExpr _ _   _     -> return False
    AppendExpr _ _  _    -> return False
    FuncCall typ f p       ->
      typeExpr (FuncCall typ f p) >>= getExprType >>= \t -> case t of
        SliceType _ -> return True 
        otherwise   -> return False

typeStmt :: Stmt -> TableState Stmt
-- typeStmt (ExprStmt e) = void $ typeExpr e
typeStmt (ExprStmt expr) = do
  e <- typeExpr expr
  case e of
    FuncCall _ func _ -> do 
      t <- getExprType func
      case t of 
        (FuncType _) -> return (ExprStmt e)
        _ -> typeError "Expression statements must be function calls"
    _ -> typeError "Expression statements must be function calls"

typeStmt (ReturnStmt Nothing) = do
  retType <- getReturnType
  setReturn True NullStmt
  case retType of 
    Nothing -> return (ReturnStmt Nothing)
    Just VoidType -> return (ReturnStmt Nothing)
    Just t  -> typeError $ "Return expects an argument of type " ++ formatType t

typeStmt (ReturnStmt (Just e)) = do
  t1' <- typeExpr e
  t1 <- getExprType t1'
  retType <- getReturnType
  setReturn True NullStmt
  case retType of
    Nothing -> typeError "Return does not expect any value"
    Just t2 -> do
      valid <- t1 `typeEquals` t2
      if valid
        then return (ReturnStmt (Just t1'))
        else typeError $ "Return expects an argument of type " ++ formatType t2

typeStmt (ShortVarDeclStmt left right) = do
  let notBlanks = filter (\x -> x /= "_") left
  if length (Set.fromList notBlanks) /= length notBlanks
    then typeError "Cannot assign several values to the same variable in short variable declaration"
    else do
      right' <- mapM typeExpr right
      return (ShortVarDeclStmt left right')
  created <- mapM typeShortVarDecl (zip left right)
  if length (filter id created) == 0
    then typeError "Short declaration contains no new variable"
    else do
      right' <- mapM typeExpr right
      return (ShortVarDeclStmt left right')

typeStmt (DeclarationStmt decl) = do
  d <- typeDeclaration decl
  return (DeclarationStmt d)

typeStmt (AssignStmt lefts rights) = do
  lst <- (flip mapM) (zip lefts rights) $ \(l, r) -> do
            lT' <- typeExpr l
            lT <- getExprType lT'
            rT' <- typeExpr r
            rT <- getExprType rT'
            valid <- lT `typeEquals` rT
            assignable <- isAssignable l
            if (valid || lT == UndefinedType) && assignable && isValue rT
              then return (lT', rT')
              else assignError lT rT
  return (AssignStmt (fst (unzip lst)) (snd (unzip lst)))

typeStmt (AssignOpStmt op left right) = do
  leftTT <- typeExpr left
  leftT <- getExprType leftTT
  leftT' <- resolveType leftT
  rightTT <- typeExpr right
  rightT <- getExprType rightTT
  equals <- leftT `typeEquals` rightT
  assignable <- isAssignable left
  if equals && leftT' `elem` opType (init op) && assignable && isValue rightT
     then return (AssignOpStmt op leftTT rightTT)
     else typeError $ "Invalid type passed to `" ++ op ++ "`-assignment"

typeStmt (BlockStmt stmts) =
  scope $ do
    lst <- mapM typeStmt stmts
    return (BlockStmt lst)


typeStmt (ForConditionStmt (LitExpr UndefinedType (BoolLit True)) stmts) = scope $ do
  setReturn True NullStmt
  stmts' <- scope $ do
    lst <- mapM typeStmt stmts
    return lst
  return (ForConditionStmt (LitExpr UndefinedType (BoolLit True)) stmts')

typeStmt (ForConditionStmt e stmts) = scope $ do
  eT' <- typeExpr e
  eT <- getExprType eT' >>= resolveType
  if eT == BoolType
    then do
      lst <- scope $ do
        lst' <- mapM typeStmt stmts
        return lst'
      ret <- getReturn
      if ret && e == LitExpr UndefinedType (BoolLit True)
        then return (ForConditionStmt eT' lst)
        else setReturn False (ForConditionStmt eT' lst)
    else typeError "Expression in a for loop must resolve to type `bool`"

typeStmt (ForClauseStmt EmptyStmt (LitExpr UndefinedType (BoolLit True)) EmptyStmt stmts) = scope $ do
  setReturn True NullStmt
  stmts' <- scope $ do
    lst <- mapM typeStmt stmts
    return lst
  return (ForClauseStmt EmptyStmt (LitExpr UndefinedType (BoolLit True)) EmptyStmt stmts')

typeStmt (ForClauseStmt init e post stmts) = scope $ do
  init' <- typeStmt init
  e' <- typeExpr e
  t <- getExprType e' >>= resolveType
  if t == BoolType
    then do
      post' <- typeStmt post
      lst <- scope $ do
        lst' <- mapM typeStmt stmts
        return lst'
      ret <- getReturn
      if ret && e == LitExpr UndefinedType (BoolLit True)
        then return (ForClauseStmt init' e' post' lst)
        else setReturn False (ForClauseStmt init' e' post' lst)
    else typeError "Expression in a for loop must resolve to type `bool`"
          
typeStmt (PrintStmt lst) = do
  mapM_ isBaseType lst
  lst' <- mapM typeExpr lst
  return (PrintStmt lst')

typeStmt (PrintLnStmt lst) = do
  mapM_ isBaseType lst
  lst' <- mapM typeExpr lst
  return (PrintLnStmt lst')

typeStmt (IncStmt e) = do
  e' <- typeExpr e
  t <- getExprType e' >>= resolveType
  assignable <- isAssignable e
  if not assignable
    then typeError "Only assignable expressions can be incremented/decremented"
    else return NullStmt
  if (elem t [IntType, FloatType, RuneType])
    then return (IncStmt e')
    else typeError $ "incompatible type in increment/decrement [received " ++ formatType t ++  ", expected numeric (int, rune, float64)]"

typeStmt (DecStmt e) = do
  e' <- typeExpr e
  t <- getExprType e' >>= resolveType
  assignable <- isAssignable e
  if not assignable
    then typeError "Only assignable expressions can be incremented/decremented"
    else return NullStmt
  if (elem t [IntType, FloatType, RuneType])
    then return (DecStmt e')
    else typeError $ "incompatible type in increment/decrement [received " ++ formatType t ++  ", expected numeric (int, rune, float64)]"


typeStmt (IfStmt ifCases mElseCase) = scope $ do
  alreadyReturned <- getReturn
  returns <- (flip mapM) ifCases $ \(mInit, cond, stmts) -> do
        setReturn False NullStmt
        case mInit of
          Just s  -> typeStmt s
          Nothing -> return NullStmt
        t <- typeExpr cond >>= getExprType >>= resolveType
        if t == BoolType
          then do
            scope $ mapM_ typeStmt stmts
            getReturn
          else typeError "Expression in an if condition must resolve to type `bool`"
  ifCases' <- (flip mapM) ifCases $ \(mInit, cond, stmts) -> do
        e <- typeExpr cond
        lst <- scope $ mapM typeStmt stmts
        case mInit of
          Just s -> scope $ do
            mInit' <- mapM typeStmt mInit
            return (mInit', e, lst)
          Nothing -> return $ (Nothing, e, lst)
  ret <- case mElseCase of
    Just ss -> do
      setReturn False NullStmt
      scope $ mapM_ typeStmt ss
      getReturn
    Nothing  -> return False
  mElseCase' <- case mElseCase of
    Just ss -> do 
      scope $ do
        lst <- mapM typeStmt ss
        return (Just lst)
    Nothing -> return Nothing
  if all ((==) True) (ret:returns)
    then setReturn True NullStmt
    else setReturn alreadyReturned NullStmt
  return $ IfStmt ifCases' mElseCase'

typeStmt (SwitchStmt mInit Nothing cases) =
  typeStmt (SwitchStmt mInit (Just (LitExpr UndefinedType (BoolLit True))) cases)

typeStmt (SwitchStmt mInit (Just e) cases) = do
  scope $ do
    alreadyReturned <- getReturn
    case mInit of
      Just init -> typeStmt init
      Nothing   -> return NullStmt
    e' <- typeExpr e 
    eT <- getExprType e'
    case eT of
      SliceType _ -> typeError "Cannot switch on slice type"
      otherwise -> return ()
    if not (isValue eT)
      then typeError $ formatType eT ++ " is not a value"
      else do
          returns <- (flip mapM) cases $ \(clause, stmts) -> do
            isDefault <- case clause of
              SwitchDefaultCase -> return True
              SwitchExprCase es -> do
                (flip mapM_) es $ \clause -> do
                  clauseT <- typeExpr clause >>= getExprType
                  valid <- clauseT `typeEquals` eT
                  if valid
                    then return NullExpr
                    else typeError "Cases in a switch statement must have the same type as the value being switched on"
                return False
            setReturn False NullStmt
            scope $ mapM_ typeStmt stmts
            ret <- getReturn
            return (ret, isDefault)
          if all (\(ret, _) -> ret) returns && find (\(_, d) -> d) returns /= Nothing
            then setReturn True NullStmt
          else setReturn alreadyReturned NullStmt
  scope $ do
    mInit' <- do
      case mInit of
        Just init -> do
          init' <- typeStmt init
          return $ Just init'
        Nothing -> return Nothing
    e' <- typeExpr e
    cases' <- (flip mapM) cases $ \(clause, stmts) -> do
        clause' <- case clause of
          SwitchDefaultCase -> return SwitchDefaultCase
          SwitchExprCase es -> do
            lst <- mapM typeExpr es
            return $ SwitchExprCase lst
        stmts' <- scope $ do
          lst <- mapM typeStmt stmts
          return lst
        return (clause', stmts')
    return $ SwitchStmt mInit' (Just e') cases'

typeStmt BreakStmt = return BreakStmt
typeStmt ContinueStmt = return ContinueStmt


typeStmt s = return NullStmt

-- Type a short variable declaration.
-- Returns True if the variable was not declared before in the most
-- recent scope and False otherwise
typeShortVarDecl :: (String, Expr) -> TableState Bool
typeShortVarDecl (left, right) = do
  symb <- findCurrentSymbol left
  t <- typeExpr right >>= getExprType
  if not (isValue t)
    then typeError "The right side of the short variable declaration is not a value"
    else case symb of
      Just (VariableSymbol t1) -> do
        t2 <- typeExpr right >>= getExprType
        assignable <- t1 `typeEquals` t2
        if assignable
          then return False
          else assignError t1 t2
      Just _ -> typeError "Cannot assign a value to a type"
      Nothing -> do
        t <- typeExpr right >>= getExprType
        addSymbol left (VariableSymbol t)
        if left == "_"
          then return False
          else return True

-----------------
-- Expressions --
-----------------

-- List the types that an operator admits
opType :: String -> [Type]
opType x
  | x == "+" = [IntType, FloatType, StringType, RuneType]
  | x `elem` ["*", "-", "/"] = [IntType, FloatType, RuneType]
  | x `elem` ["%", "|", "&", "<<", ">>", "&^", "^"] = [IntType, RuneType]
  | x `elem` ["&&", "||"] = [BoolType]
  | x `elem` ["<", "<=", ">", ">="] = [IntType, FloatType, StringType, RuneType]
  -- == and != are special cases and are not handled here

typeTypeCast :: Type -> Type -> TableState Type
typeTypeCast t1 t2 = do
  t1' <- resolveType t1
  t2' <- resolveType t2
  case () of
    _ | t1' `elem` [IntType, FloatType, RuneType] && t2' `elem` [IntType, FloatType, RuneType]
        -> return t1
      | t1' == StringType && t2' `elem` [RuneType, IntType]
        -> return t1
      | t1' == t2' && t1' `elem` [IntType, FloatType, StringType, RuneType, BoolType]
        -> return t1
    _ -> typeError $ "Cannot cast from " ++ formatType t2 ++ " to " ++ formatType t1

-- Check if an expression is valid and return its type
typeExpr :: Expr -> TableState Expr
typeExpr (LitExpr _ lit) =
  case lit of 
    FloatLit _  -> return $ LitExpr FloatType lit
    IntLit   _  -> return $ LitExpr IntType lit
    StringLit _ -> return $ LitExpr StringType lit
    RuneLit _   -> return $ LitExpr RuneType lit
    BoolLit _   -> return $ LitExpr BoolType lit

typeExpr (VariableExpr _ _ n) =
  if (n == "_") 
    then return $ VariableExpr UndefinedType UndefinedType n
    else do
      symb <- findSymbol n
      case symb of
        Just (VariableSymbol t) -> do
          t' <- resolveType t
          return $ VariableExpr t t' n
        Just (ConstantSymbol t) -> do
          t' <- resolveType t
          return $ VariableExpr t t' n
        Just (TypeSymbol t)     -> do
          t' <- resolveType t
          return $ VariableExpr (TypeCast t) t' n
        Nothing                 -> undefinedError n

typeExpr (UnopExpr _ op e) = do
  e' <- typeExpr e
  t <- getExprType e' >>= resolveType
  let valid = case op of
        "+" -> t `elem` [IntType, FloatType, RuneType]
        "-" -> t `elem` [IntType, FloatType, RuneType]
        "!" -> t == BoolType
        "^" -> t `elem` [IntType, RuneType]
  t' <- resolveType t
  if valid
    then return (UnopExpr t' op e')
    else typeError $ "Unary operator `" ++ op ++ "` cannot be applied to value of type " ++ formatType t

typeExpr (BinopExpr _ "==" e1 e2) = do
  e1' <- typeExpr e1
  t1 <- getExprType e1'
  e2' <- typeExpr e2
  t2 <- getExprType e2'
  if not (isValue t1) || not (isValue t2)
    then typeError "Not comparable types"
    else do
      valid <- t1 `typeEquals` t2
      if not valid
        then typeError "Comparison operators require both values to have the same type"
        else return ()
      if isComparable t1
        then return (BinopExpr BoolType "==" e1' e2')
        else typeError "Not comparable types"
  where
    isComparable :: Type -> Bool
    isComparable (SliceType x) = False
    isComparable (ArrayType l t) = isComparable t
    isComparable (StructType fields) = all (\(n, t) -> isComparable t) (flattenFuncParams fields)
    isComparable other = True

typeExpr (BinopExpr t "!=" e1 e2) = do
  exp <- typeExpr (BinopExpr t "==" e1 e2)
  case exp of
    BinopExpr t "==" e1 e2 -> return $ BinopExpr BoolType "!=" e1 e2
    otherwise -> return $ BinopExpr BoolType "!=" e1 e2

typeExpr (BinopExpr _ op e1 e2) = do
  e1' <- typeExpr e1
  t1 <- getExprType e1'
  t1' <- resolveType t1
  e2' <- typeExpr e2
  t2 <- getExprType e2'
  equal <- t1 `typeEquals` t2
  case (equal, t1' `elem` opType op) of
    (False, _) -> typeError "Binary operators require both values to have the same type"
    (_, False) -> typeError $ "'" ++ op ++ "' cannot be used on values of type " ++ formatType t1
    otherwise  ->
      if op `elem` ["<", "<=", ">", ">="]
        then return (BinopExpr BoolType op e1' e2')
        else return (BinopExpr t1 op e1' e2')

typeExpr (FuncCall _ func args) = do
  func' <- typeExpr func
  funcType <- getExprType func'
  args' <- mapM typeExpr args
  argsType <- mapM getExprType args'
  case funcType of
    FuncType (FuncSignature ps retType) -> checkFuncArgs ps argsType *> return (FuncCall retType func' args')
    TypeCast t1 ->
      case argsType of
        [t2]       -> do
          t <- typeTypeCast t1 t2
          return $ FuncCall t func' args'
        otherwise -> typeError "Invalid typecast"
    otherwise -> typeError "Call can only be performed on functions"

typeExpr (IndexExpr _ expr idx) = do
  e1 <- typeExpr expr
  t1 <- getExprType e1
  t1' <- resolveType t1
  e2 <- typeExpr idx
  t2 <- getExprType e2 >>= resolveType
  if t2 /= IntType
    then typeError "Index must be an int"
    else
      case t1' of
        ArrayType len typ -> return (IndexExpr typ e1 e2)
        SliceType typ -> return (IndexExpr typ e1 e2)
        _ -> typeError $ "Indexing target expects list target (slice, array), received " ++ formatType t1'

typeExpr (AppendExpr _ e1 e2) = do
  e1' <- typeExpr e1
  t1 <- getExprType e1'
  t1' <- resolveType t1
  e2' <- typeExpr e2
  t2 <- getExprType e2'
  case t1' of
    SliceType typ -> do
      valid <- typ `typeEquals` t2
      if valid
        then return (AppendExpr t1 e1' e2')
        else typeError $ "append expression slice type is incompatible with element type [" ++ formatType typ ++ "!=" ++ formatType t2 ++ "]"
    _ -> typeError "append expects slice type as first argument"

typeExpr (SelectorExpr _ e str) = do
  e' <- typeExpr e
  t <- getExprType e' >>= resolveType
  case t of
    StructType lst -> case find (\x -> x /= Nothing) (map (\(y, t) -> find (\(z, typ) -> z == str) (zip y (repeat t))) lst) of
                        Just (Just (_, typ)) -> return (SelectorExpr typ e' str)
                        Nothing -> typeError $ "target struct does not have field " ++ str
    _ -> typeError $ "selector target must be a struct type, received " ++ formatType t

typeExpr e = return e

flattenFuncParams :: [([String], Type)] -> [(String, Type)]
flattenFuncParams [] = []
flattenFuncParams (x:xs) = 
  let (y:ys, t) = x in
    case ys of [] -> (y, t) : flattenFuncParams xs
               ys -> (y, t) : flattenFuncParams ((ys, t):xs)

checkFuncArgs :: [([String], Type)] -> [Type] -> TableState ()
checkFuncArgs params args = do
  if length (flattenFuncParams params) /= length args
    then typeError $ "Function call expected " ++ show (length params) ++ " arguments, got " ++ show (length args) ++ " / " ++ (show params)
    else mapM_ checkParam (zip (flattenFuncParams params) args)
  where
    checkParam ((_, t1), t2) = do
      valid <- t1 `typeEquals` t2
      if valid then return ()
               else typeError $ "Function call expected an argument of type " ++ formatType t1 ++ " but got " ++ formatType t2


isValue :: Type -> Bool
isValue VoidType = False
isValue (FuncType _) = False
isValue (TypeCast _) = False
isValue _ = True

