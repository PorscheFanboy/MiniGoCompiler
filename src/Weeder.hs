{-# LANGUAGE TupleSections #-}

module Weeder
( weed
, WeedError(..)
) where

import Control.Monad
import qualified Data.HashSet as Set

import Parser

type Weed a = a -> Either WeedError ()

type WeedError = String

ok = Right ()

sameLength :: [a] -> [b] -> Bool
sameLength as bs = length as == length bs

weed :: Weed Program
weed (Program pkg topLevelDecls)
  = mapM_ weedTopLevelDecl topLevelDecls
  *> if pkg == Package "_"
       then Left "Package identifiers cannot be `_`"
       else Right ()

weedTopLevelDecl :: Weed TopLevelDecl
weedTopLevelDecl (FuncDecl (BasicFunc n s body)) = mapM_ (weedStmt False False) body
weedTopLevelDecl (Declaration (TypeDecl t)) = weedTypeDecl t
weedTopLevelDecl (Declaration (VarDecl v)) = weedVarDecl v

weedVarDecl :: Weed VarDecl
weedVarDecl (SingleVarDecl v) = weedVarSpec v
weedVarDecl (MultiVarDecl vs) = mapM_ weedVarSpec vs

weedVarSpec (VarSpec left right t)
  | sameLength left right                   = mapM_ weedRightExpr right
  | t /= UndefinedType && length right == 0 = ok
  | otherwise                               = Left errorMsg
  where errorMsg = "var declarations should have the same number of values on both sides"

weedTypeDecl :: Weed TypeDecl
weedTypeDecl (SingleTypeSpec t) = weedTypeSpec t 
weedTypeDecl (MultiTypeSpec ts) = mapM_ weedTypeSpec ts

weedTypeSpec :: Weed TypeSpec
weedTypeSpec (TypeSpec s t) = weedType t

weedType :: Weed Type
weedType (StructType struct) = buildStructSet struct *> ok
weedType otherwise = ok

-- Weed a statement. 1st param is true if we are in a for loop, 2nd param is true
-- if we are in a switch loop
weedStmt :: Bool -> Bool -> Weed Stmt
weedStmt for switch stmt =
  case stmt of
    BreakStmt | not for && not switch
      -> Left "break statement must be inside a for/switch statement"
    ContinueStmt | not for
      -> Left $ "continue statement must be inside a for statement" ++  show switch ++ show for
    BlockStmt body
      -> mapM_ f body 
    IfStmt ifs else'
      -> mapM_ (\(init, cond, bs) -> mapM f bs *> weedRightExpr cond) ifs
      *> maybe ok (mapM_ f) else' 
    SwitchStmt mInit mCond cases
      -> mapM_ (\(c, bs)  -> mapM_ (weedStmt for True) bs *> weedSwitchCase c) cases
      *> if length (filter (\(c, bs) -> c == SwitchDefaultCase) cases) > 1
           then Left "Switch statements can only contain one default case"
           else Right ()
      *> maybe ok f mInit
      *> maybe ok weedRightExpr mCond
    ForConditionStmt e ss
      -> mapM_ (weedStmt True switch) ss
      *> weedRightExpr e
    ForClauseStmt s1 e s2 ss
      -> mapM_ (weedStmt True switch) ss
      *> f s1
      *> f s2
      *> case s2 of 
           ShortVarDeclStmt _ _ -> Left "Post-statement in for loop cannot be a short var declaration"
           otherwise            -> ok
      *> weedRightExpr e
    ReturnStmt (Just me)
      -> weedRightExpr me
    ExprStmt (FuncCall UndefinedType f ps)
      -> weedRightExpr (FuncCall UndefinedType f ps)
    ExprStmt otherwise
      -> Left "Expression statements must be a function call"
    IncStmt e
      -> weedRightExpr e
    DecStmt e
      -> weedRightExpr e
    AssignStmt left right
      -> if sameLength left right then ok else Left "var assignments should have the same number of values on both sides"
      *> mapM_ weedLeftExpr left
      *> mapM_ weedRightExpr right
    AssignOpStmt _ left right
      -> weedLeftExpr left
      *> weedRightExpr right
    ShortVarDeclStmt left right
      -> if sameLength left right then ok else Left "var declaration should have the same number of values on both sides"
      *> mapM_ weedRightExpr right
    PrintStmt es
      -> mapM_ weedRightExpr es
    PrintLnStmt es
      -> mapM_ weedRightExpr es
    otherwise -> ok
  where
    f = weedStmt for switch
    weedSwitchCase SwitchDefaultCase = ok
    weedSwitchCase (SwitchExprCase es) = mapM_ weedRightExpr es

weedLeftExpr :: Weed Expr
weedLeftExpr e = ok

weedRightExpr :: Weed Expr
weedRightExpr (VariableExpr UndefinedType UndefinedType "_") = Left "blank identifier cannot be used inside a right value."
weedRightExpr (SelectorExpr UndefinedType e _) = weedRightExpr e
weedRightExpr (IndexExpr UndefinedType e1 e2) = weedRightExpr e1 *> weedRightExpr e2
weedRightExpr (ConversionExpr UndefinedType _ e) = weedRightExpr e
weedRightExpr (BinopExpr UndefinedType _ e1 e2) = weedRightExpr e1 *> weedRightExpr e2
weedRightExpr (UnopExpr UndefinedType _ e) = weedRightExpr e
weedRightExpr (AppendExpr UndefinedType e1 e2) = weedRightExpr e1 *> weedRightExpr e2
weedRightExpr (FuncCall UndefinedType e1 es) = weedRightExpr e1 *> mapM_ weedRightExpr es
weedRightExpr e = ok

buildStructSet :: [([String], Type)] -> Either WeedError (Set.HashSet String)
buildStructSet [] = Right Set.empty
buildStructSet ((ns, _):xs) = do
  fields <- buildStructSet xs
  foldM tryInsert fields ns
  where
    tryInsert :: (Set.HashSet String) -> String -> Either WeedError (Set.HashSet String)
    tryInsert fields n =
      if (n /= "_") && Set.member n fields then
        Left $ "field " ++ n ++ " is defined twice in the structure"
      else
        Right $ Set.insert n fields