module Codegen
( generateCode
)
where

import Data.Char
import Data.Maybe
import Data.List(intercalate, find, partition)
import Control.Monad.Trans.State

import Tokenizer(Literal(..))
import Parser
import Typechecker(flattenFuncParams)

data CodegenState = CodegenState
  { output :: String
  -- ^ Python code
  , globalVars :: String
  , variables :: [[String]]
  , types :: [[(String, Type)]]
  , indent :: Int
  -- ^ current indentation
  , label :: Int
  , forLabels :: [Int]
  }

initState = CodegenState "" "" [[]] [[]] 0 0 []

type Codegen = State CodegenState

-- Generate a Python program from the AST.
-- Assumes that the program passsed as argument has been typechecked.
generateCode :: Program -> String
generateCode prog =
  let CodegenState output gv _ _ _ _ _ = execState (genProg prog) initState
    in header ++ gv ++ header1 ++ output-- ++ footer

header :: String
header = intercalate "\n"
  [ "BITS 64"
  , "DEFAULT REL"
  , "extern _puts, _printf, _malloc, _free"
  , "section .data"
  , "int_str db \"%lld\", 10, 0"
  , ""
  ]

header1 = intercalate "\n"  
  [ "section .text"
  , "global _main"
  , ""
  ]

-- mainFun :: String
-- mainFun = "\nif __name__ == '__main__' and 'main0' in globals(): main0()\n"

commas = intercalate ","

------------------------
-- State manipulation --
------------------------

-- Write a new line properly indented
writeLn :: String -> Codegen ()
writeLn s = do
  CodegenState output gv vars types indent l fl <- get
  let output' = output ++ s ++ "\n"
  put $ CodegenState output' gv vars types indent l fl

addGlobal :: String -> Codegen ()
addGlobal s = do
  CodegenState output gv vars types indent l fl <- get
  let gv' = gv ++ s ++ "\n"
  put $ CodegenState output gv' vars types indent l fl

-- Create a new scope. Note that this does not modify the indentation
pushScope :: Codegen ()
pushScope = do
  CodegenState output gv vars types indent l fl <- get
  put $ CodegenState output gv ([]:vars) ([]:types) indent l fl

popScope :: Codegen ()
popScope = do
  CodegenState output gv vars types indent l fl<- get
  let (_:vars') = vars
  let (_:types') = types
  put $ CodegenState output gv vars' types' indent l fl

getVariables :: Codegen [[String]]
getVariables = do
  CodegenState _ gv vars _ _ _ _<- get
  return vars

globals :: Codegen [String]
globals = do
  vars <- getVariables
  case vars of
    [] -> return []
    xs -> return $ map (\x -> x ++ "0") (last vars)

-- Call f in a new scope
scope :: Codegen a -> Codegen a
scope f = pushScope *> f <* popScope

-- Increment the indentation
incIndent :: Int -> Codegen ()
incIndent n = do
  CodegenState output gv vars types indent l fl<- get
  put $ CodegenState output gv vars types (indent + n) l fl

-- Call f in a new indented scope
indented :: Codegen a -> Codegen a
indented f = incIndent 1 *> scope f <* incIndent (-1)

-- Declare a variable in the current scope
declareVariable :: String -> Codegen String
declareVariable vName = do
  CodegenState output gv vars types indent l fl<- get
  let v:vs = vars
  put $ CodegenState output gv ((vName:v):vs) types indent l fl
  x <- getVariableName vName
  return $ fromJust x

declareType :: String -> Type -> Codegen ()
declareType tName tType = do
  CodegenState output gv vars types indent l fl <- get
  let t:ts = types
  let types' = ((tName, tType):t):ts
  put $ CodegenState output gv vars types' indent l fl

resolveType :: Type -> Codegen Type
resolveType (UserType "int") = return IntType
resolveType (UserType "string") = return StringType
resolveType (UserType "float64") = return FloatType
resolveType (UserType "bool") = return BoolType
resolveType (UserType "rune") = return RuneType
resolveType (UserType s) = do
  CodegenState output gv _ types _ _ _ <- get
  let n = findType s types
  resolveType n
  where 
    findType name (t:ts) =
      case find (\(n,_) -> n == name) t of
        Nothing -> findType name ts
        Just (_, rT) -> rT
resolveType other = return other -- TODO

--
getVariableName :: String -> Codegen (Maybe String)
getVariableName vName = do
  vars <- getVariables
  let n = findVariable vars
  if n == -1
    then return Nothing
    -- else return $ Just $ vName ++ show n
    else case vName ++ show n of
           "true0" -> return $ Just $ "True"
           "false0" -> return $ Just $ "False"
           vn -> return $ Just vn
  where
    findVariable [] = -1
    findVariable (v:vs) =
      if vName `elem` v
        then length vs
        else findVariable vs

------------------
-- Declarations --
------------------

getExprType :: Expr -> Type
getExprType (VariableExpr _ t _) = t 
getExprType (SelectorExpr t _ _) = t
getExprType (IndexExpr t _ _) = t
getExprType (ConversionExpr t _ _) = t
getExprType (LitExpr t _) = t
getExprType (BinopExpr t _ _ _) = t
getExprType (UnopExpr t _ _) = t
getExprType (AppendExpr t _ _) = t
getExprType (FuncCall t _ _) = t

genProg :: Program -> Codegen ()
genProg (Program pkg topLevelDecls) = do 
  declareVariable "true"
  declareVariable "false"
  mapM_ genTopLevelDecl topLevelDecls

genTopLevelDecl :: TopLevelDecl -> Codegen ()
genTopLevelDecl (Declaration (TypeDecl t)) = genTypeDecl t
genTopLevelDecl (Declaration (VarDecl v)) = genVarDecl v
genTopLevelDecl (FuncDecl f) = genFuncDecl f

genVarDecl :: VarDecl -> Codegen ()
genVarDecl (SingleVarDecl spec) = genVarSpec spec
genVarDecl (MultiVarDecl specs) = mapM_ genVarSpec specs

genTypeDecl :: TypeDecl -> Codegen ()
genTypeDecl (SingleTypeSpec (TypeSpec n t)) = declareType n t
genTypeDecl (MultiTypeSpec ts) = mapM_ (genTypeDecl . SingleTypeSpec) ts

findParamPos :: String -> [[String]] -> Maybe Int
findParamPos str stack = findParamPosHelper str lst 2 where
  lst = getSndLast stack
  findParamPosHelper s [] _ = Nothing
  findParamPosHelper s (x:xs) acc
    | s == x = Just acc
    | otherwise = findParamPosHelper s xs (acc+1)

findVarPos :: String -> [[String]] -> Maybe Int
findVarPos str stack = findVarPosHelper str (concat (take (length stack - 2) stack)) where
  findVarPosHelper str [] = Nothing
  findVarPosHelper str (x:xs)
    | str == x = Just $ length xs + 1
    | otherwise = findVarPosHelper str xs

getSndLast :: [a] -> a
getSndLast (x:y:[]) = x
getSndLast (x : xs) = getSndLast xs

findPos :: String -> [([String], Type)] -> Int
findPos s lst = findPosHelper s (concat (map (\(x,y) -> x) lst)) 0 where
  findPosHelper s (x:xs) acc
    | s == x = acc
    | otherwise = findPosHelper s xs (acc+1)


genVarSpec :: VarSpec -> Codegen ()
genVarSpec (VarSpec left [] t) = do
  t' <- resolveType t
  names <- mapM declareVariable left
  value <- defaultValue t'
  stack <- getVariables
  if length stack == 1
    then do
      case t' of
        IntType -> addGlobal $ (head left) ++ " dq 0"
        SliceType _ -> addGlobal $ (head left) ++ " dq 0"
    else do
      case t' of
        (StructType lst) -> do
          let sz = 8 * length (concat (map (\(x, y) -> x) lst))
          let mk = findVarPos (head left) stack
          case mk of
            Nothing -> do
              return ()
            Just k -> do
              writeLn $ "sub    rsp, 16"
              writeLn $ "mov    rdi, " ++ show sz 
              writeLn $ "call   _malloc"
              writeLn $ "mov    [rbp-" ++ show (8*k) ++ "], rax"
        otherwise -> do
          let mk = findVarPos (head left) stack
          case mk of
            Nothing -> do
              return ()
            Just k -> do
              writeLn $ "sub    rsp, 16\n" ++
                        "mov    rax, " ++ value ++ "\n" ++
                        "mov    [rbp-" ++ show (8*k) ++ "], rax\n"


  -- let initializers = replicate (length left) value
  -- writeLn $ commas names ++ " = " ++ commas initializers
genVarSpec (VarSpec left right _) = do
  exprs <- mapM genExprRight right
  names <- mapM declareVariable left
  stack <- getVariables
  if length stack == 1
    then do
      case head right of
        LitExpr IntType (IntLit k) -> addGlobal $ (head left) ++ " dq " ++ k
    else do
  -- writeLn $ show stack
      let mk = findVarPos (head left) stack
      case mk of
        Nothing -> do
          return ()
        Just k -> do
          exp <- genExprRight $ head right
          writeLn $ "sub    rsp, 16\n" ++
                    exp ++
                    "mov    [rbp-" ++ show (8*k) ++ "], rax\n"


  -- writeLn $ commas names ++ " = " ++ (commas exprs)


genFuncDecl :: FuncDecl -> Codegen ()
genFuncDecl (BasicFunc name sig body) = do
  globs <- globals
  ps' <- (flip mapM) (zip (flattenFuncParams ps) [1..]) (\((n, t), k) ->
    if n == "_"
      then declareVariable $ "_" ++ show k
      else declareVariable n)
  scope $ do
    ps' <- (flip mapM) (zip (flattenFuncParams ps) [1..]) (\((n, t), k) ->
      if n == "_"
        then declareVariable $ "_" ++ show k
        else declareVariable n)
    writeLn $ "_" ++ name ++ ":\n" ++
              "push   rbp\n" ++
              "mov    rbp, rsp\n"
    scope $ do genStmts body
    writeLn $ "mov    rsp, rbp\n" ++
              "pop    rbp\n" ++
              "ret"
    where
      FuncSignature ps retType = sig


  -- if name == "init"
  --   then do
  --     scope $ do
  --       genStmts body
  --       return ()
  --   else do
  --     globs <- globals
  --     declareVariable name
  --     scope $ do 
  --       ps' <- (flip mapM) (zip (flattenFuncParams ps) [1..]) (\((n, t), k) ->
  --         if n == "_"
  --           then declareVariable $ "_" ++ show k
  --           else declareVariable n)
  --       writeLn $ "\ndef " ++ name ++ "0(" ++ commas ps' ++ "):"
  --       if globs /= []
  --         then indented $ writeLn $ "global " ++ commas globs
  --         else return ()
  --       CodegenState _ vars _ _ <- get
  --       indented $ writeLn $ show vars
  --       indented $ genStmts body
  --     return ()
  --     where
  --       FuncSignature ps retType = sig

-- Return the default value assigned to a variable
defaultValue :: Type -> Codegen String
defaultValue IntType = return "0"
defaultValue FloatType = return "0.0"
defaultValue StringType = return "\"\""
defaultValue BoolType = return "False"
defaultValue RuneType = return "0"
defaultValue (ArrayType l t) = do
  v <- defaultValue t
  -- return $ "[" ++ v ++ " for x in range(" ++ show l ++ ")]"
  return $ "[" ++ v ++ "] * " ++ show l
defaultValue (UserType s) = resolveType (UserType s) >>= defaultValue
defaultValue (SliceType _) = return "0"
defaultValue (StructType _) = return "0"


-- defaultValue (StructType fields) = do -- is this correct?
--   result <- (flip mapM) (concat $ mapM (\(lst, t) -> (zip lst (repeat t))) fields) $ \(n, rt) -> do
--     v <- defaultValue rt
--     return $ "'" ++ n ++ "':" ++ v
--   return $ "{" ++ commas result ++ "}" 


----------------
-- Statements --
----------------


genStmts :: [Stmt] -> Codegen ()
genStmts []    = writeLn ""
genStmts stmts = mapM_ genStmt stmts

genStmt :: Stmt -> Codegen ()
genStmt (DeclarationStmt (VarDecl v)) = genVarDecl v
genStmt (DeclarationStmt (TypeDecl decl)) = genTypeDecl decl

genStmt EmptyStmt = writeLn "' '"

genStmt (ReturnStmt Nothing) = writeLn $  "mov    rsp, rbp\n" ++
                                          "pop    rbp\n" ++
                                          "ret"

genStmt (ReturnStmt (Just e)) = do
  e' <- genExprRight e
  -- writeLn $ "ret " ++ e
  writeLn $ e' ++
            "mov    rsp, rbp\n" ++
            "pop    rbp\n" ++
            "ret"

genStmt (BreakStmt) = do
  CodegenState output gv vars types indent l (x:xs) <- get
  writeLn $ "jmp    " ++ "for_done" ++ show x


genStmt (ContinueStmt) = do
  CodegenState output gv vars types indent l (x:xs) <- get
  writeLn $ "jmp    " ++ ".for_post" ++ show x

genStmt (BlockStmt stmts) = scope $ genStmts stmts

genStmt (ForConditionStmt cond body) = do
  CodegenState output gv vars types indent l lst <- get
  put $ CodegenState output gv vars types indent (l+1) (l:lst)
  writeLn $ "for_loop" ++ show l ++ ":\n"
  cond' <- genExpr cond
  writeLn $ cond'
  writeLn $ "cmp    rax, 0"
  writeLn $ "je     for_done" ++ show l
  -- writeLn $ "while " ++ cond' ++ ":"
  indented $ genStmts body
  writeLn $ "jmp for_loop" ++ show l
  writeLn $ "for_done" ++ show l ++ ":"
  CodegenState output gv vars types indent k (x:xs) <- get
  put $ CodegenState output gv vars types indent k xs




genStmt (ForClauseStmt init cond post body) = scope $ do
  CodegenState output gv vars types indent l lst <- get
  put $ CodegenState output gv vars types indent (l+1) (l:lst)
  genStmt init
  writeLn $ "for_loop" ++ show l ++ ":"
  cond' <- genExpr cond
  writeLn $ cond'
  writeLn $ "cmp    rax, 0"
  writeLn $ "je     for_done" ++ show l
  indented $ genStmts body
  writeLn $ "for_post" ++ show l ++ ":"
  genStmt post
  writeLn $ "jmp    for_loop" ++ show l
  writeLn $ "for_done" ++ show l ++ ":"
  CodegenState output gv vars types indent k (x:xs) <- get
  put $ CodegenState output gv vars types indent k xs




genStmt (ShortVarDeclStmt left right) = genVarSpec (VarSpec left right UndefinedType)

genStmt (IncStmt e) = do
  e' <- genExpr e
  er' <- genExprRight (BinopExpr IntType "+" e (LitExpr IntType (IntLit "1")))
  writeLn $ e'
  writeLn $ "mov    rbx, rax"
  writeLn $ er'
  writeLn $ "mov    [rbx], rax"

  -- stack <- getVariables
  -- let mk = findVarPos name stack 
  -- case mk of
  --   Nothing -> do
  --     let pk = findParamPos name stack
  --     case pk of
  --       Nothing -> do
  --         writeLn $ "lea    rax, [" ++ name ++ "]"
  --         writeLn $ "mov    rbx, [rax]\n"
  --         writeLn $ "inc    rbx"
  --         writeLn $ "mov    [rax], rbx\n"
  --       Just k -> writeLn $ "mov    rax, [rbp+" ++ show (8*k) ++ "]\n"
  --   Just k -> writeLn $ "mov    rax, [rbp-" ++ show (8*k) ++ "]\n"



genStmt (DecStmt e) = do
  e' <- genExpr e
  er' <- genExprRight (BinopExpr IntType "-" e (LitExpr IntType (IntLit "1")))
  writeLn $ e'
  writeLn $ "mov    rbx, rax"
  writeLn $ er'
  writeLn $ "mov    [rbx], rax"





genStmt (AssignStmt left right) =
  case (left, right) of
    ([IndexExpr t e1 e2], [e3]) -> do
      el <- genExpr (IndexExpr t e1 e2)
      er <- genExprRight e3
      writeLn $ "sub    rsp, 8\n" ++
                el ++
                "mov    [rsp], rax\n" ++
                er ++
                "mov    rbx, rax\n" ++
                "mov    rax, [rsp]\n" ++
                "mov    [rax], rbx\n" ++
                "add    rsp, 8\n"

    -- ([(VariableExpr t1 t2 name1)], [(AppendExpr _ (VariableExpr _ _ name2) e2)]) ->
    --   if name2 == name1
    --     then do
    --       CodegenState output gv vars types indent l lst <- get
    --       put $ CodegenState output gv vars types indent (l+1) lst
    --       stack <- getVariables
    --       e2' <- genExpr e2
    --       let mk = findVarPos name1 stack
    --       case mk of
    --         Nothing -> do
    --           let pk = findParamPos name1 stack
    --           case pk of
    --             Nothing -> do
    --               writeLn $ "lea    rbx, [" ++ name1 ++ "]"
    --               writeLn $ "mov    rax, [rbx]"
    --               writeLn $ "cmp    rax, 0"
    --               writeLn $ "jne    .append" ++ show l
    --               writeLn $ "mov    rdi, 1000000"
    --               writeLn $ "call   _malloc"
    --               writeLn $ "mov    [rbx], rax"
    --               writeLn $ "mov    [rax], 1"
    --               writeLn $ ".append" ++ show l ++ ":"
    --               writeLn $ "mov    rbx, [rax]"
    --               writeLn $ "mov    rcx, rax"
    --               writeLn $ e2'
    --               writeLn $ "mov    [rcx+8*rbx], rax"
    --             Just k -> return ()
    --         Just k -> return ()
    --       -- writeLn $ left' ++ ".append(" ++ e2' ++ ")"
    --     else do
    --       left' <- mapM genExpr left
    --       right' <- mapM genExprRight right
    --       writeLn $ commas left' ++ " = " ++ (commas  right')
    -- (l, r) -> do
      -- left' <- mapM genExpr l
      -- right' <- mapM genExprRight r
      -- writeLn $ commas left' ++ " = " ++ (commas  right')
    ([VariableExpr t1 t2 name], [exp]) -> do
      el <- genExpr $ VariableExpr t1 t2 name
      er <- genExprRight exp
      writeLn $ "sub    rsp, 8"
      writeLn $ el
      writeLn $ "mov    [rsp], rax"
      writeLn $ er
      -- writeLn $ "mov    [rsp], rax"
      writeLn $ "mov    rbx, [rsp]"
      writeLn $ "mov    [rbx], rax"
      writeLn $ "add    rsp, 8"

      -- stack <- getVariables
      -- let mk = findVarPos name stack
      -- case mk of
      --   Nothing -> do return ()
      --   Just k -> do
      --     e <- genExprRight exp
      --     writeLn $ e ++ "mov [rbp-" ++ show (8*k) ++ "], rax\n"
    ([SelectorExpr t e name], [exp]) -> do
      el <- genExpr $ SelectorExpr t e name
      er <- genExprRight exp
      writeLn $ "sub    rsp, 8"
      writeLn $ el
      writeLn $ "mov    [rsp], rax"
      writeLn $ er
      -- writeLn $ "mov    [rsp], rax"
      writeLn $ "mov    rbx, [rsp]"
      writeLn $ "mov    [rbx], rax"
      writeLn $ "add    rsp, 8"
    ([l], [r]) -> do
      writeLn $ show l
      writeLn $ show r

      -- writeLn $ "asdf"


genStmt (AssignOpStmt op left right) = do
  left' <- genExpr left
  right' <- genExpr right
  case op of
    -- "+=" -> writeLn $ 
    "&^=" -> writeLn $ left' ++ "=" ++ left' ++ " & (~(" ++ right' ++ "))"
    "/=" ->
      case getExprType left of
        IntType -> writeLn $ left' ++ "=" ++ "(" ++ left' ++ ") // (" ++ right' ++ ")"
        FloatType -> writeLn $ left' ++ "=" ++ "(" ++ left' ++ ") / (" ++ right' ++ ")"
    otherwise -> writeLn $ left' ++ " " ++ op ++ " " ++ right'

genStmt (ExprStmt e) = genExpr e >>= writeLn


genStmt (IfStmt clauses mElseClause) = scope $ do
  if checkInit clauses
    then do
      case (clauses, mElseClause) of
        ([(ini, con, s)], Nothing) -> do
          CodegenState output gv vars types indent l fl <- get
          put $ CodegenState output gv vars types indent (l+1) fl
          writeLn $ ".ifcond_" ++ show l ++ ":"
          con' <- genExpr con
          writeLn $ con'
          writeLn $ "cmp    rax, 0"
          writeLn $ "je     if_done_" ++ show l
          s' <- mapM_ genStmt s
          writeLn $ "if_done_" ++ show l ++ ":"
        ([(ini, con, s)], (Just ss)) -> do
          CodegenState output gv vars types indent l fl <- get
          put $ CodegenState output gv vars types indent (l+1) fl
          writeLn $ ".ifcond_" ++ show l ++ ":"
          con' <- genExpr con
          writeLn $ con'
          writeLn $ "cmp    rax, 0"
          writeLn $ "je     if_else" ++ show l
          mapM_ genStmt s
          writeLn $ "jmp    if_done_" ++ show l
          writeLn $ "if_else" ++ show l ++ ":"
          mapM_ genStmt ss
          writeLn $ "if_done_" ++ show l ++ ":"
          -- genIfClause "if" (ini, con, s)
        -- (ini, con, s) : xs -> do
        --   genIfClause "if" (ini, con, s)
        --   mapM_ (genIfClause "elif") xs
      -- case mElseClause of
      --   Just s -> genElseClause s
      --   Nothing -> return ()
    else do
      case clauses of
        [(ini, con, s)] -> do


          vars <- getVariables
          let next = "next" ++ show (length vars) ++ "_"
          writeLn $ next ++ " = True"
          mapM_ (genIfClause1 next) clauses
          maybe (return ()) (genElseClause1 next) mElseClause
  where
    genIfClause str (mInit, cond, stmts) = scope $ do
      maybe (return ()) genStmt mInit
      cond' <- genExpr cond
      writeLn $ str ++  "(" ++ cond' ++ "):"
      indented $ genStmts stmts
    genElseClause stmts = do
      writeLn $ "else:"
      indented $ genStmts stmts
    genIfClause1 next (mInit, cond, stmts) = scope $ do
      maybe (return ()) genStmt mInit
      cond' <- genExpr cond
      writeLn $ "if " ++ next ++ " and (" ++ cond' ++ "):"
      indented $ genStmts stmts
      indented $ writeLn $ next ++ " = False"
    genElseClause1 next stmts = do
      writeLn $ "if " ++ next ++ ":"
      indented $ genStmts stmts
    checkInit [] = True
    checkInit ((Just s, _, _):xs) = False
    checkInit ((Nothing, _, _):xs) = checkInit xs



genStmt (PrintStmt es) = do
  es' <- mapM genExpr es
  writeLn $ "print_([" ++ intercalate "," es' ++ "])"

genStmt (PrintLnStmt es) = do
  let tt = getExprType (head es)
  case tt of 
    _ -> do
      es' <- genExprRight (head es)
      writeLn $ es' ++
                "mov    rsi, rax\n" ++
                "xor    rax, rax\n" ++
                "lea    rdi, [int_str]\n" ++
                "call   _printf\n"
    -- _ -> do
    --   es' <- mapM genExpr es
    --   writeLn $ "printLn_([" ++ intercalate "," es' ++ "])"

genStmt (SwitchStmt mInit mE cases) = scope $ do
  let e = fromMaybe (LitExpr BoolType (BoolLit True)) mE
  case mInit of
    Just init -> genStmt init
    Nothing   -> return ()
  tmp <- declareVariable "temp"
  e' <- genExprRight e
  writeLn $ tmp ++ " = " ++ e'
  writeLn "for i in range(1):"
  let (defaultCase, otherCases) = partition (\(x, stmts) -> x == SwitchDefaultCase) cases
  indented $ mapM_ (genCase tmp) otherCases
  case defaultCase of
    [] -> return ()
    [(SwitchDefaultCase, stmts)] -> indented $ genStmts stmts
  where
    genCase e (SwitchExprCase vs, stmts) = do
      vs' <- mapM genExpr vs
      writeLn $ "if (" ++ intercalate ") or (" (map (\v -> e ++ ") == (" ++ v) vs') ++ "):"
      indented $ genStmts stmts
      indented $ writeLn "break"

genStmt s = writeLn $ ""



-----------------
-- Expressions --
-----------------

stdVars = ["float64", "int", "string", "rune", "boolean"]

-- Return python string corresponding to the function
-- used to cast the value.
genTypeCast :: Type -> Codegen String
genTypeCast IntType = return "int"
genTypeCast FloatType = return "float"
genTypeCast RuneType = return "int"
genTypeCast StringType = return "castToStr_"
genTyepCast other = return ""



genExprRight (VariableExpr t' t s) =
  -- mName <- getVariableName s
  -- case mName of
  --   Just s'  ->
      case t' of
        SliceType _ -> do
          e <- genExpr (VariableExpr t' t s)
          return $ e ++ "\n" ++ "mov    rax, [rax]\n"
        -- ArrayType _ _ -> return $ s' ++ "[:]"
        -- StructType _ -> return $ s' ++ ".copy()"
        IntType -> do
          e <- genExpr (VariableExpr t' t s)
          return $ e ++ "\n" ++ "mov    rax, [rax]\n"
        (UserType "int") -> do
          e <- genExpr (VariableExpr t' t s)
          return $ e ++ "\n" ++ "mov    rax, [rax]\n"
          -- stack <- getVariables
          -- let mk = findVarPos s stack
          -- case mk of
          --   Nothing -> do
          --     let pk = findParamPos s stack
          --     case pk of
          --       Nothing -> do
          --         return $ "lea    rax, [" ++ s ++ "]\n" ++ "mov    rax, [rax]\n"
          --       Just k -> return $ "mov    rax, [rbp+" ++ show (8*k) ++ "]\n"
          --   Just k -> return $ "mov    rax, [rbp-" ++ show (8*k) ++ "]\n"
        RuneType -> do
          stack <- getVariables
          let mk = findVarPos s stack
          case mk of
            Nothing -> do
              let pk = findParamPos s stack
              case pk of
                Nothing -> do
                  return $ "lea    rax, [" ++ s ++ "]\n" ++ "mov    rax, [rax]\n"
                Just k -> return $ "mov    rax, [rbp+" ++ show (8*k) ++ "]\n"
            Just k -> return $ "mov    rax, [rbp-" ++ show (8*k) ++ "]\n"
        (UserType "rune") -> do
          stack <- getVariables
          let mk = findVarPos s stack
          case mk of
            Nothing -> do
              let pk = findParamPos s stack
              case pk of
                Nothing -> do
                  return $ "lea    rax, [" ++ s ++ "]\n" ++ "mov    rax, [rax]\n"
                Just k -> return $ "mov    rax, [rbp+" ++ show (8*k) ++ "]\n"
            Just k -> return $ "mov    rax, [rbp-" ++ show (8*k) ++ "]\n"
        -- otherwise -> return $ show (VariableExpr t' t s)
        otherwise -> do
          e <- genExpr (VariableExpr t' t s)
          return $ e ++ "\n" ++ "mov    rax, [rax]\n"
    -- Nothing -> resolveType (UserType s) >>= genTypeCast
genExprRight (IndexExpr t e1 e2) = do
  -- return $ show (IndexExpr t e1 e2)
  -- e1' <- genExprRight e1
  -- idx <- genExprRight e2
  e' <- genExpr (IndexExpr t e1 e2)
  case getExprType e1 of
    (SliceType _) -> return $ e' ++
                              "mov    rax, [rax]\n"
    -- (SliceType _) -> return $ "sub    rsp, 8\n" ++
    --                           e1' ++ "\n" ++
    --                           "mov    [rsp], rax\n" ++
    --                           idx ++ "\n" ++
    --                           "add    rax, 1\n" ++
    --                           "imul   rax, 8\n" ++
    --                           -- "mov    [rsp], rax\n" ++
    --                           -- "mov    rax, [rsp+8]\n" ++
    --                           "add    rax, [rsp]\n" ++
    --                           "mov    rax, [rax]\n" ++
    --                           "add    rsp, 8\n"
genExprRight (SelectorExpr t e name) = do
  e' <- genExpr (SelectorExpr t e name)
  return $ e' ++ "\n" ++
           "mov    rax, [rax]\n"
genExprRight s = genExpr s


-- Generate the Python code corresponding to an expression
genExpr :: Expr -> Codegen String
genExpr (VariableExpr t' t s) = do
  stack <- getVariables
  let mk = findVarPos s stack
  case mk of 
    Nothing -> do
      let pk = findParamPos s stack
      case pk of
        Nothing -> return $ "lea    rax, [" ++ s ++ "]\n"
        Just k -> return $ "mov    rax, rbp\n" ++ "add    rax, " ++ show (8*k) ++ "\n"
    Just k -> return $ "mov    rax, rbp\n" ++ "sub    rax, " ++ show (8*k) ++ "\n"



  -- mName <- getVariableName s
  -- case mName of
  --   Just s  -> return s
  --   Nothing -> resolveType (UserType s) >>= genTypeCast

genExpr (LitExpr t lit) = return $ genLit lit

genExpr (BinopExpr t op l r) = do
  l' <- genExprRight l
  r' <- genExprRight r
  case op of
    -- "+" -> return $ "sub    rsp, 16\n" ++ l' ++
    --                 "mov    qword [rsp + 8], rax\n" ++ r' ++
    --                 "mov    qword [rsp], rax\n" ++
    --                 "mov    rax, [rsp + 8]\n" ++
    --                 "add    rax, [rsp]\n" ++
    --                 "add    rsp, 16\n"
    "+" -> return $ "sub    rsp, 8\n" ++ l' ++
                    "mov    qword [rsp], rax\n" ++ r' ++
                    "add    rax, [rsp]\n" ++
                    "add    rsp, 8\n"
    "-" -> return $ "sub    rsp, 16\n" ++ l' ++
                    "mov    qword [rsp + 8], rax\n" ++ r' ++
                    "mov    qword [rsp], rax\n" ++ 
                    "mov    rax, [rsp + 8]\n" ++
                    "sub    rax, [rsp]\n" ++
                    "add    rsp, 16\n"
    -- "*" -> return $ "sub    rsp, 16\n" ++ l' ++
    --                 "mov    qword [rsp + 8], rax\n" ++ r' ++
    --                 "mov    qword [rsp], rax\n" ++ 
    --                 "mov    rax, [rsp + 8]\n" ++
    --                 "imul   rax, [rsp]\n" ++
    --                 "add    rsp, 16\n"
    "*" -> return $ "sub    rsp, 8\n" ++ l' ++
                    "mov    qword [rsp], rax\n" ++ r' ++
                    "imul   rax, [rsp]\n" ++
                    "add    rsp, 8\n"
    "%" -> return $ "push   rcx\n" ++
                    "push   rdx\n" ++
                    "sub    rsp, 8\n" ++ l' ++
                    "mov    dword [rsp], eax\n" ++ r' ++
                    "mov    ecx, eax\n" ++
                    "mov    eax, dword [rsp]\n" ++
                    "xor    rdx, rdx\n" ++
                    "idiv   ecx\n" ++
                    "xor    rax, rax\n" ++
                    "mov    eax, edx\n" ++
                    "add    rsp, 8\n" ++
                    "pop    rdx\n" ++
                    "pop    rcx\n"
    "/" -> return $ "push   rcx\n" ++
                    "push   rdx\n" ++
                    "sub    rsp, 8\n" ++ l' ++
                    "mov    dword [rsp], eax\n" ++ r' ++
                    "mov    ecx, eax\n" ++
                    "mov    eax, dword [rsp]\n" ++
                    "xor    rdx, rdx\n" ++
                    "idiv   ecx\n" ++
                    "add    rsp, 8\n" ++
                    "pop    rdx\n" ++
                    "pop    rcx\n"
    "==" -> do
      CodegenState output gv vars types indent l fl <- get
      put $ CodegenState output gv vars types indent (l+1) fl
      return $ "sub    rsp, 16\n" ++ l' ++
               "mov    qword [rsp + 8], rax\n" ++ r' ++
               "mov    qword [rsp], rax\n" ++ 
               "mov    rax, [rsp + 8]\n" ++
               "cmp    rax, [rsp]\n" ++
               "je     .if_true_" ++ show l ++ "\n" ++
               "mov    rax, 0\n" ++
               "jmp    .if_done" ++ show l ++ "\n" ++
               ".if_true_" ++ show l ++ ":\n" ++
               "mov    rax, 1\n" ++
               ".if_done" ++ show l ++ ":\n" ++
               "add    rsp, 16\n"
    "!=" -> do
      CodegenState output gv vars types indent l fl <- get
      put $ CodegenState output gv vars types indent (l+1) fl
      return $ "sub    rsp, 16\n" ++ l' ++
               "mov    qword [rsp + 8], rax\n" ++ r' ++
               "mov    qword [rsp], rax\n" ++ 
               "mov    rax, [rsp + 8]\n" ++
               "cmp    rax, [rsp]\n" ++
               "jne     .if_true_" ++ show l ++ "\n" ++
               "mov    rax, 0\n" ++
               "jmp    .if_done" ++ show l ++ "\n" ++
               ".if_true_" ++ show l ++ ":\n" ++
               "mov    rax, 1\n" ++
               ".if_done" ++ show l ++ ":\n" ++
               "add    rsp, 16\n"
    "<" -> do
      CodegenState output gv vars types indent l fl <- get
      put $ CodegenState output gv vars types indent (l+1) fl
      return $ "sub    rsp, 16\n" ++ l' ++
               "mov    qword [rsp + 8], rax\n" ++ r' ++
               "mov    qword [rsp], rax\n" ++ 
               "mov    rax, [rsp + 8]\n" ++
               "cmp    rax, [rsp]\n" ++
               "jl     .if_less_" ++ show l ++ "\n" ++
               "mov    rax, 0\n" ++
               "jmp    .if_lessdone" ++ show l ++ "\n" ++
               ".if_less_" ++ show l ++ ":\n" ++
               "mov    rax, 1\n" ++
               ".if_lessdone" ++ show l ++ ":\n" ++
               "add    rsp, 16\n"
    ">" -> do
      CodegenState output gv vars types indent l fl <- get
      put $ CodegenState output gv vars types indent (l+1) fl
      return $ "sub    rsp, 16\n" ++ l' ++
               "mov    qword [rsp + 8], rax\n" ++ r' ++
               "mov    qword [rsp], rax\n" ++ 
               "mov    rax, [rsp + 8]\n" ++
               "cmp    rax, [rsp]\n" ++
               "jg     .if_less_" ++ show l ++ "\n" ++
               "mov    rax, 0\n" ++
               "jmp    .if_lessdone" ++ show l ++ "\n" ++
               ".if_less_" ++ show l ++ ":\n" ++
               "mov    rax, 1\n" ++
               ".if_lessdone" ++ show l ++ ":\n" ++
               "add    rsp, 16\n"
    ">=" -> do
      CodegenState output gv vars types indent l fl <- get
      put $ CodegenState output gv vars types indent (l+1) fl
      return $ "sub    rsp, 16\n" ++ l' ++
               "mov    qword [rsp + 8], rax\n" ++ r' ++
               "mov    qword [rsp], rax\n" ++ 
               "mov    rax, [rsp + 8]\n" ++
               "cmp    rax, [rsp]\n" ++
               "jge    .if_less_" ++ show l ++ "\n" ++
               "mov    rax, 0\n" ++
               "jmp    .if_lessdone" ++ show l ++ "\n" ++
               ".if_less_" ++ show l ++ ":\n" ++
               "mov    rax, 1\n" ++
               ".if_lessdone" ++ show l ++ ":\n" ++
               "add    rsp, 16\n"
    "<=" -> do
      CodegenState output gv vars types indent l fl <- get
      put $ CodegenState output gv vars types indent (l+1) fl
      return $ "sub    rsp, 16\n" ++ l' ++
               "mov    qword [rsp + 8], rax\n" ++ r' ++
               "mov    qword [rsp], rax\n" ++ 
               "mov    rax, [rsp + 8]\n" ++
               "cmp    rax, [rsp]\n" ++
               "jle    .if_less_" ++ show l ++ "\n" ++
               "mov    rax, 0\n" ++
               "jmp    .if_lessdone" ++ show l ++ "\n" ++
               ".if_less_" ++ show l ++ ":\n" ++
               "mov    rax, 1\n" ++
               ".if_lessdone" ++ show l ++ ":\n" ++
               "add    rsp, 16\n"
    "&&" -> do
      CodegenState output gv vars types indent l fl <- get
      put $ CodegenState output gv vars types indent (l+1) fl
      return $ "sub    rsp, 16\n" ++ l' ++
               "mov    qword [rsp + 8], rax\n" ++ r' ++
               "mov    qword [rsp], rax\n" ++ 
               "mov    rax, [rsp + 8]\n" ++
               "and    rax, [rsp]\n" ++
               -- "je     .if_less_" ++ show l ++ "\n" ++
               -- "mov    rax, 0\n" ++
               -- "jmp    .if_lessdone" ++ show l ++ "\n" ++
               -- ".if_less_" ++ show l ++ ":\n" ++
               -- "mov    rax, 1\n" ++
               -- ".if_lessdone" ++ show l ++ ":\n" ++
               "add    rsp, 16\n"
    "||" -> do
      CodegenState output gv vars types indent l fl <- get
      put $ CodegenState output gv vars types indent (l+1) fl
      return $ "sub    rsp, 16\n" ++ l' ++
               "mov    qword [rsp + 8], rax\n" ++ r' ++
               "mov    qword [rsp], rax\n" ++ 
               "mov    rax, [rsp + 8]\n" ++
               "or     rax, [rsp]\n" ++
               -- "je     .if_less_" ++ show l ++ "\n" ++
               -- "mov    rax, 0\n" ++
               -- "jmp    .if_lessdone" ++ show l ++ "\n" ++
               -- ".if_less_" ++ show l ++ ":\n" ++
               -- "mov    rax, 1\n" ++
               -- ".if_lessdone" ++ show l ++ ":\n" ++
               "add    rsp, 16\n"
    "&^" -> return $ "(" ++ l' ++ ") & (~(" ++ r' ++ "))"
    otherwise -> return $ "(" ++ l' ++ ")" ++ op ++ "(" ++ r' ++ ")"

genExpr (UnopExpr t op e) = do
  e' <- genExprRight e
  case op of 
    "^" -> return $ "~ (" ++ e' ++ ")"
    "!" -> return $ "not (" ++ e' ++ ")"
    otherwise -> return $ op ++ "(" ++ e' ++ ")"

genExpr (FuncCall t f args) = do
  f' <- genExpr f
  case f of 
    VariableExpr _ _ name -> do
      args' <- mapM genExprRight args
      if length args' == 0
        then return $ "call   _" ++ name ++ "\n"
        else if odd (length args')
               then return $ "sub    rsp, 8\n" ++ (intercalate "push   rax\n" args') ++ "push   rax\n" ++ "call   _" ++ name ++ "\n" ++
                   "add    rsp, " ++ show (8*((length args)+1)) ++ "\n"
             else return $ (intercalate "push   rax\n" args') ++ "push   rax\n" ++ "call   _" ++ name ++ "\n" ++
                   "add    rsp, " ++ show (8* (length args)) ++ "\n"

  -- return $ f' ++ "JIANNN\n" ++ show f ++ "\n"
  -- return $ f' ++ "(" ++ (intercalate ", " args') ++ ")"

genExpr (IndexExpr t e1 e2) = do
  -- return $ show (IndexExpr t e1 e2)
  e1' <- genExprRight e1
  idx <- genExprRight e2
  case getExprType e1 of
    (SliceType _) -> return $ "sub    rsp, 8\n" ++
                              e1' ++ "\n" ++
                              "mov    [rsp], rax\n" ++
                              idx ++ "\n" ++
                              "add    rax, 1\n" ++
                              "imul   rax, 8\n" ++
                              "add    rax, [rsp]\n" ++
                              "add    rsp, 8\n"



genExpr (SelectorExpr t e s) = do
  e' <- genExprRight e
  case e of
    (VariableExpr _ (StructType lst) _) -> do
      let k = findPos s lst
      return $ e' ++ "\n" ++
               "add    rax, " ++ show (k * 8) ++ "\n"

  -- return $ show (SelectorExpr t e s)
  -- return $ e' ++ "['" ++ s ++ "']"


genExpr (AppendExpr t e1 e2) = do
  CodegenState output gv vars types indent l lst <- get
  put $ CodegenState output gv vars types indent (l+1) lst
  e1' <- genExprRight e1
  e2' <- genExprRight e2
  --- rsp+16 : base address
  return $ "push   rbx\n" ++
           "sub    rsp, 16\n" ++
           e1' ++
           "cmp    rax, 0\n" ++
           "jne    .add_elem" ++ show l ++ "\n" ++
           "mov    rdi, 88888888\n" ++
           "call   _malloc\n" ++
           "mov    qword [rax], 0\n" ++
           ".add_elem" ++ show l ++ ":\n" ++
           "mov    [rsp+8], rax\n" ++
           "mov    rbx, rax\n" ++ --rbx contains the base address now
           "mov    rax, [rax]\n" ++ --move count into rax
           "add    rax, 1\n" ++ 
           "mov    [rbx], rax\n" ++  
           "imul   rax, 8\n" ++
           "add    rax, rbx\n" ++
           "mov    [rsp], rax\n" ++ -- address to put in new elem, is now in rsp+8
           e2' ++
           "mov    rbx, [rsp]\n" ++
           "mov    [rbx], rax\n" ++
           "mov    rax, [rsp+8]\n" ++
           "add    rsp, 16\n" ++
           "pop    rbx\n"





genExpr e = return "# not implemented yet" -- TODO

genLit :: Literal -> String
genLit (FloatLit s) = "float(" ++ s ++ ")"
genLit (IntLit s) = "mov    rax, " ++ s ++ "\n"

genLit (StringLit s) = show s
genLit (RuneLit c) = "mov    rax, " ++ show (ord c) ++ "\n"
genLit (BoolLit True) = "mov    rax, 1\n"
genLit (BoolLit False) = "mov    rax, 0\n"