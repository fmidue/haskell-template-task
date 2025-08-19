-- Based on a version of
-- (c) Bertram Felgenhauer, 2011
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Haskell.Template.Match where

import qualified Language.Haskell.Exts.SrcLoc     as S

import Control.Applicative              (Alternative (..))
import Control.Monad (
  MonadPlus (..),
  ap,
  liftM,
  msum,
  unless,
  void,
  when,
  )
import Control.Monad.State              (State, evalState, gets, put)
import Data.Function                    (on)
import Data.Generics
  (Data (..), GenericM, GenericM' (..), GenericQ, cast, gfoldlAccum, gmapQ)
import Data.List                        (insertBy)
import Data.Maybe                       (isJust)
import Language.Haskell.Exts.Syntax

data Where = OnlyTemplate | OnlySubmission
  deriving (Eq, Ord, Show)

data What
  = CompleteModule
  | Declaration
  | HeadOfModule
  | ModuleImport
  | Pragma
  deriving (Eq, Ord, Show)

data Location
   = SrcSpanInfo What Where S.SrcSpanInfo
   | SrcSpanInfoPair What S.SrcSpanInfo S.SrcSpanInfo
  deriving (Eq, Ord, Show)

data Result a = Continue | Fail [Location] | Ok a
    deriving (Eq, Ord, Show)

newtype M a = M { runM :: State Location (Result a) }

instance Monad M where
  return = pure
  a >>= b = M $ do
    a' <- runM a
    case a' of
      Ok a'' -> runM (b a'')
      Fail loc -> return $ Fail loc
      Continue -> return $ Continue

instance Functor M where
  fmap = liftM

instance Applicative M where
  pure = M . return . Ok
  (<*>) = ap

instance MonadPlus M where
  mzero = M $ return Continue
  a `mplus` b = M $ do
    a' <- runM a
    case a' of
      Continue -> runM b
      Fail la  -> do
        b' <- runM b
        case b' of
          Ok _ -> return b'
          Fail lb -> return $ Fail $ la ++ lb
          _    -> return a' -- Propagate failure information
      _ -> return a'

instance Alternative M where
  (<|>) = mplus
  empty = mzero

-- | underline the part of the input that is in the span,
-- and add some lines of contexts before and after. Returns lines.
highlight_ssi :: S.SrcSpanInfo -> Int -> String -> [String ]
highlight_ssi info context input =
  let spanI = S.srcInfoSpan info
      start@(start_line, _) = S.srcSpanStart spanI
      end@(end_line, _) = S.srcSpanEnd spanI
      (pre_mid, post) = splitAt end_line $ zip [ 1 .. ] $ lines input
      (pre, mid) = splitAt (start_line - 1) pre_mid
      underlined = do
        (row, line) <- mid
        let under = do
              (col, _) <- zip [1 ..] line
              let here = (row,col)
              return $ if start <= here && here <= end then '^' else ' '
        [line, under]
      ekat n = reverse . take n . reverse
  in  concat
         [ map snd $ ekat context pre
         , underlined
         , map snd $ take context post
         ]

data DeclarationKind l =
    TypeDeclaration {
        name  :: String
      }
  | FunctionDeclaration {
      name    :: String,
      funPart :: (Rhs l, [Pat l], Maybe (Binds l))
    }
  | Unnamed {
      decl :: Decl l
  }

instance Eq (DeclarationKind l) where
  k1@TypeDeclaration {} == k2@TypeDeclaration {} =
    name k1 == name k2
  k1@FunctionDeclaration {} == k2@FunctionDeclaration {} =
    name k1 == name k2
  Unnamed {} == Unnamed {} = True
  _       == _       = False

instance Ord l => Ord (DeclarationKind l) where
  k1@TypeDeclaration {} `compare` k2@TypeDeclaration {} =
    name k1 `compare` name k2
  k1@FunctionDeclaration {} `compare` k2@FunctionDeclaration {} =
    name k1 `compare` name k2
  Unnamed d1 `compare` Unnamed d2 = d1 `compare` d2
  TypeDeclaration {}     `compare` _ = GT
  FunctionDeclaration {} `compare` _ = GT
  Unnamed {}             `compare` _ = LT

type Decl' l b = (DeclarationKind l, b)

partitionByKind
  :: Ord l
  => [Decl' l b]
  -> ([Decl' l b], [Decl' l b], [Decl' l b])
partitionByKind []     = ([], [], [])
partitionByKind (n:ns) = case fst n of
  TypeDeclaration {} ->
    (insert' n ts, fs, us)
  FunctionDeclaration {} ->
    (ts, insert' n fs, us)
  Unnamed {} ->
    (ts, fs, insert' n us)
  where
    (ts, fs, us) = partitionByKind ns
    insert'      = insertBy (compare `on` fst)

-- fail, returning most recently seen location
failLoc :: forall a . M a
failLoc = M $ gets $ Fail . (:[])

continue :: forall a . M a
continue = M $ return Continue

onlyTemplate :: Annotated a => What -> a S.SrcSpanInfo -> Location
onlyTemplate w = SrcSpanInfo w OnlyTemplate . ann

onlySubmission :: Annotated a => What -> a S.SrcSpanInfo -> Location
onlySubmission w = SrcSpanInfo w OnlySubmission . ann

-- match reference to "undefined"
matchUndef :: forall a . Maybe (Exp a) -> Bool
matchUndef (Just (Var _ (UnQual _ (Ident _ "undefined")))) = True
matchUndef _ = False

{-|
Stores src span locations in state.
In contrast to 'matchSrcSpanInfo' it checks also arguments of constructors for
existing 'S.SrcSpanInfo'.
(uses 'matchSrcSpanInfo')
-}
matchSrcSpanInfoSub :: (Data a, Data b) => What -> a -> b -> M ()
matchSrcSpanInfoSub w f1 f2 = do
  let fs1 = filter isJust $ gmapQ cast f1
      fs2 = filter isJust $ gmapQ cast f2
  uncurry (matchSrcSpanInfo w) `mapM_` zip fs1 fs2
  matchSrcSpanInfo w (cast f1) (cast f2)

-- match locations: store given locations
matchSrcSpanInfo :: What -> Maybe S.SrcSpanInfo -> Maybe S.SrcSpanInfo -> M ()
matchSrcSpanInfo w (Just this) (Just that) =
  M (Ok <$> put (SrcSpanInfoPair w this that))
matchSrcSpanInfo w (Just this) Nothing =
  M (Ok <$> put (SrcSpanInfo w OnlyTemplate this))
matchSrcSpanInfo w Nothing (Just that) =
  M (Ok <$> put (SrcSpanInfo w OnlySubmission that))
matchSrcSpanInfo _ _ _ = continue

getFunctionName
  :: Decl l
  -> [(DeclarationKind l, Decl l)]
getFunctionName d = case d of
  TypeSig x xs tf ->
    [ (TypeDeclaration n, TypeSig x [i] tf)
    | i@(Ident _ n) <- xs]
  PatBind _ (PVar _ (Ident _ n)) rhs where_ ->
    [(FunctionDeclaration n (rhs, [], where_), d)]
  FunBind _ xs ->
    [ (FunctionDeclaration n (rhs, vs, where_), FunBind s [m])
    | m@(Match s (Ident _ n) vs rhs where_) <- xs]
  _ -> [(Unnamed d, d)]

matchList
  :: (Annotated a, Data (a S.SrcSpanInfo))
  => What
  -> [a S.SrcSpanInfo]
  -> [a S.SrcSpanInfo]
  -> [Location]
matchList w (x:xs) (y:ys) = matchLocation w x y ++ matchList w xs ys
matchList _ []     []     = []
matchList w (x:xs) []     = onlyTemplate   w x : matchList w xs []
matchList w []     (y:ys) = onlySubmission w y : matchList w [] ys

matchMaybe
  :: (Annotated a, Data (a S.SrcSpanInfo))
  => What
  -> Maybe (a S.SrcSpanInfo)
  -> Maybe (a S.SrcSpanInfo)
  -> [Location]
matchMaybe _ Nothing   Nothing   = []
matchMaybe w Nothing   (Just w2) = [onlySubmission w w2]
matchMaybe w (Just w1) Nothing   = [onlyTemplate w w1]
matchMaybe w (Just w1) (Just w2) = matchLocation w w1 w2

-- match a bunch of declarations.
matchDecl :: [Decl S.SrcSpanInfo] -> [Decl S.SrcSpanInfo] -> [Location]
matchDecl ds1 ds2 =
  types ++ functions ++ unnamed
  where
    types     = go Declaration ds1t ds2t
    functions = go Declaration ds1f ds2f
    unnamed   = go Declaration ds1u ds2u
    (ds1t, ds1f, ds1u) = splitNamed ds1
    (ds2t, ds2f, ds2u) = splitNamed ds2
    splitNamed ds = partitionByKind $ concatMap getFunctionName ds

go
  :: What
  -> [(DeclarationKind S.SrcSpanInfo, Decl S.SrcSpanInfo)]
  -> [(DeclarationKind S.SrcSpanInfo, Decl S.SrcSpanInfo)]
  -> [Location]
go w ds1@((n1, d1) : ds1') ds2@((n2, d2) : ds2')
  | Unnamed {} <- n1 = matchLocation w d1 d2 ++ go w ds1' ds2'
  | Unnamed {} <- n2 = matchLocation w d1 d2 ++ go w ds1' ds2'
  | n1' == n2' = case n1 of
      -- allow replacing 'foo = undefined' by one or more bindings of 'foo'.
      FunctionDeclaration _ (rhs1@(UnGuardedRhs _ u), vs1, w1) -> case n2 of
        FunctionDeclaration _ (rhs2, vs2, w2) ->
          matchList w vs1 vs2
          ++ (if matchUndef (Just u)
               then []
               else matchLocation w rhs1 rhs2)
          ++ go w ds1' ds2'
          ++ matchMaybe w w1 w2
        _ -> matchLocation w d1 d2 ++ go w ds1' ds2'
      _ -> matchLocation w d1 d2 ++ go w ds1' ds2'
  | n1' < n2' = onlyTemplate   w d1 : go w ds1' ds2
  | otherwise = onlySubmission w d2 : go w ds1  ds2'
  where
    n1' = name n1
    n2' = name n2
go _ [] [] = []
go w [] fs = onlySubmission w . snd <$> fs
go w fs [] = onlyTemplate   w . snd <$> fs

withCast
  :: (Data a, Data b, Data t1, Data t2)
  => (t1 -> t2 -> [Location]) -> a -> b -> M c
withCast m a b = case (cast a, cast b) of
  (Just a', Just b') -> M $ return $ Fail $ m a' b'
  _                  -> continue

-- match syntax trees
match :: Data a => What -> a -> GenericM M
match w f1 f2 = do
  msum [
    -- 1. locations (see above)
    void $ matchSrcSpanInfoSub w f1 f2,

    -- 2. declarations (see above)
    withCast matchDecl f1 f2,

    -- 3. undefined  may be replaced by any expression
    unless (matchUndef (cast f1 :: Maybe (Exp S.SrcSpanInfo))) continue,

    -- otherwise, compare constructors and match arguments
    do  when (toConstr f1 /= toConstr f2) failLoc
        void (gzipWithM' (match w) f1 f2)
    ]
  return f2

matchLocation :: (Data a, Data b) => What -> a -> b -> [Location]
matchLocation w f1 f2=
  case evalState (runM (match w f1 f2)) $ SrcSpanInfoPair w S.noSrcSpan S.noSrcSpan of
    Fail loc -> loc
    Ok _     -> []
    Continue -> []

matchModule :: Module S.SrcSpanInfo -> Module S.SrcSpanInfo -> M [Location]
matchModule (Module _ h1 p1 i1 d1) (Module _ h2 p2 i2 d2) = do
  let r = matchMaybe HeadOfModule h1 h2
        ++ matchList Pragma p1 p2
        ++ matchList ModuleImport i1 i2
        ++ matchDecl d1 d2
  if null r
    then M $ return $ Ok r
    else M $ return $ Fail r
matchModule m1 m2 = do
  matchSrcSpanInfoSub CompleteModule m1 m2
  failLoc

-- | test whether @m2@ is a suitable template for @m1@
-- in case of an error, the returned 'Location' gives the mismatching
-- position in the template
test :: Module S.SrcSpanInfo -> Module S.SrcSpanInfo -> Result [Location]
test m1 m2 =
  -- first: parse using haskell-src.
  -- ParseOk m1' = parseModule m1
  -- ParseOk m2' = parseModule m2
  evalState (runM (matchModule m1 m2)) $ SrcSpanInfoPair CompleteModule S.noSrcSpan S.noSrcSpan
------------------------------------------------------------------------------
-- | Twin map for monadic transformation
--
-- Unlike gzipWithM, process constructor arguments from left to right.
gzipWithM' :: Monad m => GenericQ (GenericM m) -> GenericQ (GenericM m)
gzipWithM' f x y = case gmapAccumM' perkid funs y of
                    ([], c) -> c
                    _       -> error "gzipWithM"
 where
  perkid (a:as) d = (as, unGM a d)
  funs = gmapQ (\k -> GM (f k)) x

-- | gmapM with accumulation
--
-- Unlike gmapAccumM, process constructor arguments from left to right.
gmapAccumM' :: forall a d m . (Data d, Monad m)
           => (forall e. Data e => a -> e -> (a, m e))
           -> a -> d -> (a, m d)
gmapAccumM' f (a ::a) (d :: d) = gfoldlAccum k t a d :: (a, m d)
 where
  k :: forall e r . Data r => a -> m (r -> e) -> r -> (a, m e)
  k x y z = let (x',z') = f x z
             in (x', y >>= \y' -> z' >>= \z'' -> return (y' z''))
  t x y = (x, return y)
