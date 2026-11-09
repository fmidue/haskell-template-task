module Haskell.Template.Detection (collectIfsWithMaybeChecks) where

import Control.Monad (guard, void)
import Data.Data (Data)
import Data.Generics (everything, everywhere, listify, mkQ, mkT, everywhere')
import Data.List.Extra (notNull)
import Data.Maybe (listToMaybe, mapMaybe, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set (empty, fromList, member, singleton, union)
import Language.Haskell.Exts (
  Alt (..),
  Decl (..),
  Exp (..),
  Match (..),
  Module (..),
  ModuleName (..),
  Name (..),
  Pat (..),
  QName (..),
  QOp (..),
  Rhs (..),
  SrcSpanInfo,
 )
import Language.Haskell.Names (NameInfo (..), Scoped (..), symbolModule)

collectIfsWithMaybeChecks :: Module (Scoped SrcSpanInfo) -> [(Exp (Scoped SrcSpanInfo), Exp ())]
collectIfsWithMaybeChecks (Module _ _ _ _ decls) = concat $ mapMaybe process decls
  where
    process d = do
      let opportunities = everything (++) (mkQ [] (maybeToList . exprToIfInfo)) d
      guard $ notNull opportunities

      let
        allSymbols = Set.fromList (mapMaybe declName decls) `Set.union` allNames d
        freeName n
          | Set.member n allSymbols = freeName (n ++ "'")
          | otherwise = n
        rename :: String -> Name () -> Name ()
        rename n (Ident l "_valueNamePlaceholder") = Ident l n
        rename _ nExpr = nExpr

      pure $ map (everywhere $ mkT (rename $ freeName "value")) opportunities
collectIfsWithMaybeChecks _ = []

declName :: Decl a -> Maybe String
declName (FunBind _ ms) = listToMaybe $ map fromMatch ms
  where
    fromMatch (Match _ (Ident _ n) _ _ _) = n
    fromMatch (Match _ (Symbol _ n) _ _ _) = n
    fromMatch (InfixMatch _ _ (Ident _ n) _ _ _) = n
    fromMatch (InfixMatch _ _ (Symbol _ n) _ _ _) = n
declName (PatBind _ (PVar _ pn) _ _) = Just $ fromName pn
  where
    fromName (Ident _ n) = n
    fromName (Symbol _ n) = n
declName _ = Nothing

exprToIfInfo :: Exp (Scoped SrcSpanInfo) -> Maybe (Exp (Scoped SrcSpanInfo), Exp ())
exprToIfInfo expr@(If _ cond t e) = case removeParens cond of
  (App _ (Var _ vn) maybeVal)
    | hasName maybeMod "isJust" vn -> transform maybeVal t e
    | hasName maybeMod "isNothing" vn -> transform maybeVal e t
  (InfixApp _ l (QVarOp _ opName) r) -> case getMaybeVal l r of
    Just maybeVal
      | hasName eqMod "==" opName -> transform maybeVal e t
      | hasName eqMod "/=" opName -> transform maybeVal t e
    _ -> Nothing
  _ -> Nothing
  where
    eqMod = Just "GHC.Classes"
    maybeMod = Just "Data.Maybe"
    isNothingExpression (Con _ vName) = hasName maybeMod "Nothing" vName
    isNothingExpression _ = False

    getMaybeVal l r
      | isNothingExpression l && isNothingExpression r = Nothing
      | isNothingExpression l = Just r
      | isNothingExpression r = Just l
      | otherwise = Nothing

    transform mValExpr j n = case removeParens mValExpr of
      mVal@(Var _ _)
        | hasFromJust mVal j -> Just (expr, asCaseExpression "_valueNamePlaceholder" mVal j n)
        | otherwise -> Nothing
      _ -> Nothing
exprToIfInfo _ = Nothing

allNames :: Data a => a -> Set String
allNames = everything Set.union $ mkQ Set.empty extract
  where
    extract :: Name (Scoped SrcSpanInfo) -> Set String
    extract (Ident _ n) = Set.singleton n
    extract (Symbol _ n) = Set.singleton n

hasFromJust :: Exp (Scoped SrcSpanInfo) -> Exp (Scoped SrcSpanInfo) -> Bool
hasFromJust mExpr expr = notNull $ listify isFromJustApp expr
  where
    isFromJustApp :: Exp (Scoped SrcSpanInfo) -> Bool
    isFromJustApp (App _ (Var _ vn) maybeVal) =
      hasName (Just "Data.Maybe") "fromJust" vn && fmap void (removeParens maybeVal) == fmap void mExpr
    isFromJustApp _ = False

asCaseExpression
  :: String
  -> Exp (Scoped SrcSpanInfo)
  -> Exp (Scoped SrcSpanInfo)
  -> Exp (Scoped SrcSpanInfo)
  -> Exp ()
asCaseExpression valueName maybeValue justCase nothingCase =
  Case
    ()
    (void maybeValue)
    [ Alt () (PApp () (UnQual () (Ident () "Nothing")) []) (UnGuardedRhs () $ void nothingCase) Nothing
    , Alt
        ()
        (PApp () (UnQual () (Ident () "Just")) [PVar () (Ident () valueName)])
        (UnGuardedRhs () $ replaceFromJustWithValue valueName maybeValue justCase)
        Nothing
    ]

replaceFromJustWithValue :: String -> Exp (Scoped SrcSpanInfo) -> Exp (Scoped SrcSpanInfo) -> Exp ()
replaceFromJustWithValue valueName maybeValue = void . everywhere' (mkT replace)
  where
    replace :: Exp (Scoped SrcSpanInfo) -> Exp (Scoped SrcSpanInfo)
    replace expr = case removeParens expr of
      App l (Var _ (UnQual _ (Ident _ "fromJust"))) fnBody
        | fmap void (removeParens fnBody) == fmap void maybeValue -> Var l (UnQual l (Ident l valueName))
      _ -> expr

removeParens :: Exp a -> Exp a
removeParens (Paren _ e) = removeParens e
removeParens e = e

hasName :: Maybe String -> String -> QName (Scoped l) -> Bool
hasName mModuleName qName (UnQual scope (Ident _ n)) = n == qName && isGloballyScopedInModule mModuleName scope
hasName mModuleName qName (Qual scope _ (Ident _ n)) = n == qName && isGloballyScopedInModule mModuleName scope
hasName mModuleName qName (UnQual scope (Symbol _ n)) = n == qName && isGloballyScopedInModule mModuleName scope
hasName mModuleName qName (Qual scope _ (Symbol _ n)) = n == qName && isGloballyScopedInModule mModuleName scope
hasName _ _ _ = False

isGloballyScopedInModule :: Maybe String -> Scoped l -> Bool
isGloballyScopedInModule Nothing _ = True
isGloballyScopedInModule (Just modName) (Scoped (GlobalSymbol sym _) _) = symbolModule sym == ModuleName () modName
isGloballyScopedInModule (Just _) _ = False
