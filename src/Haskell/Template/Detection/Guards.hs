{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Haskell.Template.Detection.Guards (detectRedundantGuardCondition) where

import Control.Monad (forM, join, liftM2, liftM3, void, (<=<))
import Data.List (singleton)
import Data.List.Extra (nubOrd)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE (fromList, toList)
import Data.Maybe (fromJust, mapMaybe, maybeToList)
import Language.Haskell.Exts (
  Decl (FunBind, PatBind),
  Exp (..),
  GuardedRhs (..),
  Literal (..),
  Match (InfixMatch, Match),
  Module (Module),
  ModuleName (..),
  Name (..),
  Pat (PVar),
  QName (Qual, UnQual),
  QOp (..),
  Rhs (GuardedRhss, UnGuardedRhs),
  SrcSpanInfo,
  Stmt (Qualifier),
  ann,
 )
import Language.Haskell.Names (
  NameInfo (GlobalSymbol),
  Scoped (..),
  Symbol (..),
 )
import Witherable (witherM)
import Z3.Monad (
  AST,
  Result (..),
  Z3,
  assert,
  check,
  evalZ3,
  mkAdd,
  mkAnd,
  mkBoolVar,
  mkDiv,
  mkEq,
  mkFalse,
  mkGe,
  mkGt,
  mkImplies,
  mkInt,
  mkIntSort,
  mkIntVar,
  mkIte,
  mkLe,
  mkLt,
  mkMod,
  mkMul,
  mkNot,
  mkNumeral,
  mkOr,
  mkRealSort,
  mkRealVar,
  mkRem,
  mkStringSymbol,
  mkSub,
  mkTrue,
 )

-- | Shorthand for a scoped expression
type ScopedExp a = Exp (Scoped a)

{- | Returns the name of a (potentially) qualified name.
|
| Returns `Nothing` when `Special` is passed as the argument.
-}
qName :: QName l -> Maybe (Name l)
qName (Qual _ _ n) = Just n
qName (UnQual _ n) = Just n
qName _ = Nothing

{- | Returns the actual name of a (potentially) qualified name together with the module name if under global scope.
|
| Returns `Nothing` when `Special` is passed as the argument.
-}
qActName :: QName (Scoped a) -> Maybe (Maybe String, String)
qActName q = case qName q of
  Just (Ident (globalModName -> mn) n) -> Just (mn, n)
  Just (Symbol (globalModName -> mn) n) -> Just (mn, n)
  Nothing -> Nothing
  where
    globalModName (Scoped (GlobalSymbol (symbolModule -> ModuleName () name) _) _) = Just name
    globalModName _ = Nothing

{- | Returns the actual name of a (potentially) qualified name together with its scope information.
|
| Returns `Nothing` when `Special` is passed as the argument.
-}
qActScopedName :: QName (Scoped a) -> Maybe (NameInfo a, String)
qActScopedName q = case qName q of
  Just (Ident (Scoped scope _) n) -> Just (scope, n)
  Just (Symbol (Scoped scope _) n) -> Just (scope, n)
  Nothing -> Nothing

{- | Returns the actual name of a (potentially) qualified operator together with the module name if under global scope.
|
| Returns `Nothing` when the operators name uses `Special` as the constructor.
-}
qOpActName' :: QOp (Scoped a) -> Maybe (Maybe String, String)
qOpActName' (QVarOp _ (qActName -> res)) = res
qOpActName' (QConOp _ (qActName -> res)) = res

-- | Unwraps an expression from parentheses
unwrapParens :: Exp a -> Exp a
unwrapParens (Paren _ e) = unwrapParens e
unwrapParens e = e

-- | Returns a list of guard conditions which are redundant
checkForRedundancy :: Eq a => [NonEmpty (ScopedExp a, Z3 AST)] -> IO [ScopedExp a]
checkForRedundancy [] = pure []
checkForRedundancy [_] = pure []
checkForRedundancy (x : xs) = checking (NE.toList $ fmap snd x) (map NE.toList xs)
  where
    checking _ [] = pure []
    checking skipped ([] : remaining) = checking skipped remaining
    checking skipped remaining = do
      let allRemaining = concat remaining

      results <- forM allRemaining $ \(e, c) -> do
        isRedundant <- prove $ do
          known <- mkAnd =<< mapM (>>= mkNot) skipped
          inQuestion <- c

          mkImplies known inQuestion
        return ((e, c), isRedundant)

      let redundant = map fst $ filter snd results

      restResults <-
        checking
          ((mkAnd =<< mapM snd (head remaining)) : skipped)
          (map (filter ((`notElem` map fst redundant) . fst)) $ drop 1 remaining)

      return $ map fst redundant ++ restResults

detectRedundantGuardCondition
  :: Module (Scoped SrcSpanInfo)
  -> IO [(Name (Scoped SrcSpanInfo), [Exp (Scoped SrcSpanInfo)])]
detectRedundantGuardCondition mod' =
  flip witherM (extractFromModule mod') $ \(name, constraints) -> do
    redundant <- checkForRedundancy constraints
    case redundant of
      [] -> pure Nothing
      xs -> pure $ Just (name, nubOrd xs)

-- | Extracts all guard conditions from a definition using guards
extractFromModule
  :: Module (Scoped SrcSpanInfo)
  -> [(Name (Scoped SrcSpanInfo), [NonEmpty (ScopedExp SrcSpanInfo, Z3 AST)])]
extractFromModule (Module _ _ _ _ decls) = concatMap fromDecl decls
  where
    fromDecl (FunBind _ matches) = map fromMatch matches
    fromDecl (PatBind _ (PVar _ name) rhs _) = [(name, fromRhs rhs)]
    fromDecl _ = []

    fromRhs (UnGuardedRhs _ _) = []
    fromRhs (GuardedRhss _ gRhss) = mapMaybe extractConstraintsFromGuardedRhs gRhss

    fromMatch (Match _ name _ rhs _) = (name, fromRhs rhs)
    fromMatch (InfixMatch _ _ name _ rhs _) = (name, fromRhs rhs)
extractFromModule _ = []

{- | Extracts all conditions from a given guard.
|
| Returns `Nothing` if one guard is not a pure boolean expression.
-}
extractConstraintsFromGuardedRhs
  :: GuardedRhs (Scoped SrcSpanInfo)
  -> Maybe (NonEmpty (ScopedExp SrcSpanInfo, Z3 AST))
extractConstraintsFromGuardedRhs (GuardedRhs _ statements _) = case concat <$> mapM extractFromStmt statements of
  Nothing -> Nothing
  Just [] -> Nothing
  Just xs -> Just $ NE.fromList xs
  where
    extractFromStmt (Qualifier _ conditionExpr) = Just $ astToZ3 conditionExpr
    extractFromStmt _ = Nothing

-- | Returns the z3 variable name representing a scoped variable
scopedVarName :: Show a => (NameInfo a, String) -> String
scopedVarName (scope, name) = show scope ++ "::" ++ name

-- | Returns the z3 variable name representing the length of the list encapsulated in the expression
listVarName :: ScopedExp () -> String
listVarName expr = "::length_" ++ show expr

-- | Computes the z3 ast for a given expression inside an arithmetical expression
arithAstToZ3 :: Show a => ScopedExp a -> Maybe (ScopedExp a, Z3 AST)
arithAstToZ3 (unwrapParens -> expr) =
  (expr,) <$> case unwrapParens expr of
    ( Var
        _
        ( qActScopedName ->
            Just scopedName
          )
      ) -> Just $ mkRealVar =<< mkStringSymbol (scopedVarName scopedName)
    (Lit _ (Int _ _ valueString)) ->
      Just $ mkNumeral valueString =<< mkIntSort
    (Lit _ (Frac _ _ valueString)) ->
      Just $ mkNumeral valueString =<< mkRealSort
    (App _ (Var _ (qActName -> Just (mModName, name))) arg) ->
      arithAppToZ3 mModName name [arg]
    (App _ (unwrapParens -> App _ (unwrapParens -> Var _ (qActName -> Just (mModName, name))) arg1) arg2) ->
      arithAppToZ3 mModName name [arg1, arg2]
    (InfixApp _ l (qOpActName' -> Just (mModName, name)) r) ->
      arithAppToZ3 mModName name [l, r]
    (Let _ _ e) ->
      snd <$> arithAstToZ3 e
    (NegApp _ e) ->
      ((\v -> mkSub [v]) <=< snd) <$> arithAstToZ3 e
    _ ->
      Nothing

-- | Computes the z3 ast for a given function or operator application inside an arithmetical expression
arithAppToZ3 :: Show a => Maybe String -> String -> [ScopedExp a] -> Maybe (Z3 AST)
arithAppToZ3 (Just "GHC.Num") "+" [l, r] =
  combineMaybeArithExprs (\a b -> mkAdd [a, b]) l r
arithAppToZ3 (Just "GHC.Num") "-" [l, r] =
  combineMaybeArithExprs (\a b -> mkSub [a, b]) l r
arithAppToZ3 (Just "GHC.Num") "*" [l, r] =
  combineMaybeArithExprs (\a b -> mkMul [a, b]) l r
arithAppToZ3 (Just "GHC.Real") "/" [l, r] =
  combineMaybeArithExprs mkDiv l r
arithAppToZ3 (Just "GHC.Real") "div" [l, r] =
  combineMaybeArithExprs mkDiv l r
arithAppToZ3 (Just "GHC.Real") "mod" [l, r] =
  combineMaybeArithExprs mkMod l r
arithAppToZ3 (Just "GHC.Real") "rem" [l, r] =
  combineMaybeArithExprs mkRem l r
arithAppToZ3 (Just "GHC.List") "length" [ls] = Just $ do
  v <- mkIntVar =<< mkStringSymbol (listVarName $ fmap void ls)
  assert =<< mkGe v =<< mkInt 0 =<< mkIntSort
  return v
arithAppToZ3 (Just "Data.List") "sum" [unwrapParens -> List _ es] =
  (mkAdd <=< sequence) <$> mapM (fmap snd . arithAstToZ3) es
arithAppToZ3 (Just "Data.List") "product" [unwrapParens -> List _ es] =
  (mkMul <=< sequence) <$> mapM (fmap snd . arithAstToZ3) es
arithAppToZ3 _ _ _ = Nothing

-- | Computes the z3 ast for a given expression
astToZ3 :: Show a => ScopedExp a -> [(ScopedExp a, Z3 AST)]
astToZ3 (unwrapParens -> expr) = case expr of
  (Var _ (qActScopedName -> Just scopedName)) ->
    -- variable outside arithmetic comparison
    singleton $ (expr,) $ mkBoolVar =<< mkStringSymbol (scopedVarName scopedName)
  (App _ (Var _ (qActName -> Just (mModName, name))) arg) ->
    -- application outside arithmetic comparison
    appToZ3 expr mModName name [arg]
  (InfixApp _ l (qOpActName' -> Just (mModName, name)) r) ->
    -- infix application outside arithmetic comparison
    appToZ3 expr mModName name [l, r]
  (Con _ (qActName -> Just (Just "GHC.Types", "True"))) ->
    -- literal True
    singleton (expr, mkTrue)
  (Con _ (qActName -> Just (Just "GHC.Types", "False"))) ->
    -- literal False
    singleton (expr, mkFalse)
  (Let _ _ e) -> astToZ3 e
  (If _ cond t e) ->
    map (expr,) $
      liftM3 (\(_, a) (_, b) (_, c) -> join $ liftM3 mkIte a b c) (astToZ3 cond) (astToZ3 t) (astToZ3 e)
  _ -> []

-- | Computes the z3 ast for a given function or operator application
appToZ3 :: Show a => ScopedExp a -> Maybe String -> String -> [ScopedExp a] -> [(ScopedExp a, Z3 AST)]
appToZ3 expr (Just "GHC.Classes") opName [l, r]
  | opName `elem` map fst opMap =
      map (expr,) $ maybeToList $ combineMaybeArithExprs op l r
  where
    opMap = [("<", mkLt), ("<=", mkLe), (">", mkGt), (">=", mkGe), ("==", mkEq)]
    op = fromJust $ lookup opName opMap
appToZ3 expr (Just "GHC.Classes") "&&" [l, r] =
  appToZ3 expr (Just "GHC.List") "and" [List (ann l) [l, r]]
appToZ3 expr (Just "GHC.Classes") "||" [l, r] =
  appToZ3 expr (Just "GHC.List") "or" [List (ann l) [l, r]]
appToZ3 expr (Just "GHC.Classes") "/=" [l, r] =
  map (expr,) $ maybeToList ((mkNot =<<) <$> combineMaybeArithExprs mkEq l r)
appToZ3 expr (Just "GHC.Classes") "not" [e] =
  (expr,) . (mkNot <=< snd) <$> astToZ3 e
appToZ3 expr (Just "GHC.List") "null" [ls] =
  singleton $ (expr,) $ do
    v <- mkIntVar =<< mkStringSymbol (listVarName $ fmap void ls)
    mkEq v =<< mkInt 0 =<< mkIntSort
appToZ3 _ (Just "GHC.List") "and" [unwrapParens -> List _ es] =
  concat $ mapM astToZ3 es
appToZ3 expr (Just "GHC.List") "or" [unwrapParens -> List _ es] =
  singleton $ (expr,) $ mkOr =<< mapM (mkAnd <=< (mapM snd . astToZ3)) es
appToZ3 _ _ _ _ =
  []

{- | Computes the z3 ast for a composition of two (arithmetic) expressions.
|
| Only returns `Just` when both expressions are arithmetic expressions.
-}
combineMaybeArithExprs :: Show a => (AST -> AST -> Z3 AST) -> ScopedExp a -> ScopedExp a -> Maybe (Z3 AST)
combineMaybeArithExprs f l r = liftM2 (\(_, a) (_, b) -> join $ liftM2 f a b) (arithAstToZ3 l) (arithAstToZ3 r)

-- | Proves if a given proposition in form of a z3 ast is valid
prove :: Z3 AST -> IO Bool
prove prop = do
  res <- evalZ3 $ do
    p <- prop

    assert =<< mkNot p

    check
  case res of
    Unsat -> pure True
    _ -> pure False
