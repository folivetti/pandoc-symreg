{-# language DeriveTraversable #-}
{-# language StandaloneDeriving #-}
{-# language LambdaCase #-}
{-# language TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Data.SRTree.EqSat ( simplifyEqSat ) where

import Control.Applicative (liftA2)
import Control.Monad (unless)
import Data.AEq ( AEq((~==)) )
import Data.Eq.Deriving ( deriveEq1 )
import Data.Equality.Analysis ( Analysis(..) )
import Data.Equality.Graph ( EGraph, ClassId, Language, ENode(unNode), represent, merge, find, children )
import Data.Equality.Graph.Lens hiding ((^.))
import Data.Equality.Graph.Lens qualified as L
import Data.Equality.Matching
import Data.Equality.Matching.Database ( Subst )
import Data.Equality.Saturation
import Data.Equality.Saturation.Scheduler ( BackoffScheduler(BackoffScheduler) )
import Data.Foldable qualified as F
import Data.IntMap.Strict qualified as IM
import Data.Maybe (isJust, isNothing)
import Data.Ord.Deriving ( deriveOrd1 )
import Data.SRTree hiding (Fix(..))
import Data.SRTree.Eval
import Data.SRTree.Recursion qualified as R
import Data.Set qualified as S
import Text.Show.Deriving ( deriveShow1 )
import Data.Function ((&))


import Debug.Trace ( traceShow )

deriving instance Foldable SRTree
deriving instance Traversable SRTree

deriveEq1 ''SRTree
deriveOrd1 ''SRTree
deriveShow1 ''SRTree

instance Num (Pattern SRTree) where
  l + r = NonVariablePattern $ Bin Add l r
  l - r = NonVariablePattern $ Bin Sub l r
  l * r = NonVariablePattern $ Bin Mul l r
  abs   = NonVariablePattern . Uni Abs

  negate t    = fromInteger (-1) * t
  signum _    = undefined
  fromInteger = NonVariablePattern . Const . fromInteger

instance Fractional (Pattern SRTree) where
    (/) a b      = NonVariablePattern $ Bin Div a b
    fromRational = NonVariablePattern . Const . fromRational

instance Floating (Pattern SRTree) where
  pi      = NonVariablePattern $ Const pi
  exp     = NonVariablePattern . Uni Exp
  log     = NonVariablePattern . Uni Log
  sqrt    = NonVariablePattern . Uni Sqrt
  sin     = NonVariablePattern . Uni Sin
  cos     = NonVariablePattern . Uni Cos
  tan     = NonVariablePattern . Uni Tan
  asin    = NonVariablePattern . Uni ASin
  acos    = NonVariablePattern . Uni ACos
  atan    = NonVariablePattern . Uni ATan
  sinh    = NonVariablePattern . Uni Sinh
  cosh    = NonVariablePattern . Uni Cosh
  tanh    = NonVariablePattern . Uni Tanh
  asinh   = NonVariablePattern . Uni ASinh
  acosh   = NonVariablePattern . Uni ACosh
  atanh   = NonVariablePattern . Uni ATanh

  l ** r      = NonVariablePattern (Bin Power l r)
  logBase l r = undefined


instance Analysis (Maybe Double) SRTree where
    -- type Domain SRTreeF = Maybe Double
    makeA = evalConstant -- ((\c -> egr L.^._class c._data) <$> e)

    joinA Nothing Nothing  = Nothing
    joinA (Just a) Nothing = Just a
    joinA Nothing (Just b) = Just b
    joinA (Just a) (Just b)
      | isNaN a || isInfinite a = Just a
      | isNaN b || isInfinite b = Just b
      | (abs (a-b) <= 1e-6 || a ~== b || (a == 0 && b == (-0)) || (a == (-0) && b == 0) ) = Just a
      | otherwise = (error $ "Merged non-equal constants!" <> show a <> " " <> show b <> " " <> show (a==b))

    modifyA cl eg0
      = case eg0 L.^._class cl._data of
          Nothing -> eg0
          Just d  ->
                -- Add constant as e-node
            let (new_c,eg1) = represent (Fix (Const d)) eg0
                (rep, eg2)  = merge cl new_c eg1
                -- Prune all except leaf e-nodes
            in if isNaN d
                  then eg1 & _class new_c._nodes %~ S.filter (F.null .unNode)
                  else eg2 & _class rep._nodes %~ S.filter (F.null .unNode)


evalConstant :: SRTree (Maybe Double) -> Maybe Double
evalConstant = \case
    -- Exception: Negative exponent: BinOp Pow e1 e2 -> liftA2 (^) e1 (round <$> e2 :: Maybe Integer)
    Bin Div e1 e2 -> case liftA2 (/) e1 e2 of
                          Nothing -> Nothing
                          Just y  -> if isNaN y || isInfinite y then Nothing else Just y
    Bin Sub e1 e2 -> case liftA2 (-) e1 e2 of
                          Nothing -> Nothing
                          Just y  -> if isNaN y || isInfinite y then Nothing else Just y
    Bin Mul e1 e2 -> case liftA2 (*) e1 e2 of
                          Nothing -> Nothing
                          Just y  -> if isNaN y || isInfinite y then Nothing else Just y
    Bin Add e1 e2 -> case liftA2 (+) e1 e2 of
                          Nothing -> Nothing
                          Just y  -> if isNaN y || isInfinite y then Nothing else Just y
    Bin Power e1 e2 -> case liftA2 (**) e1 e2 of
                            Nothing -> Nothing
                            Just y  -> if isNaN y || isInfinite y then Nothing else Just y
    Uni f e1 -> case (evalFun f <$> e1) of
                     Nothing -> Nothing
                     Just y  -> if isNaN y || isInfinite y then Nothing else Just y
    Var _ -> Nothing
    Const x -> if isNaN x || isInfinite x then Nothing else Just x -- TODO: investigate why it cannot handle NaN
    Param _ -> Nothing

cost :: CostFunction SRTree (Int, String)
cost = \case
  Const x       -> (5, show x)
  Var   v       -> (1, 'x':show v)
  Bin Add c1 c2 -> (fst c1 + fst c2 + 1, "Add" <> snd c1 <> snd c2)
  Bin Mul c1 c2 -> (fst c1 + fst c2 + 1, "Mul" <> snd c1 <> snd c2)
  Bin op c1 c2   -> (fst c1 + fst c2 + 3, show op <> snd c1 <> snd c2)
  Uni fun c       -> (2 * fst c + 1, show fun <> snd c)
  Param _       -> (5, "Param")

unsafeGetSubst :: Pattern SRTree -> Subst -> ClassId
unsafeGetSubst (NonVariablePattern _) _ = error "unsafeGetSubst: NonVariablePattern; expecting VariablePattern"
unsafeGetSubst (VariablePattern v) subst = case IM.lookup v subst of
      Nothing -> error "Searching for non existent bound var in conditional"
      Just class_id -> class_id

all_not :: Double -> [Pattern SRTree] -> RewriteCondition (Maybe Double) SRTree
all_not val vs subst egr =
    all (\v -> egr L.^._class (unsafeGetSubst v subst)._data /= Just val) vs

any_not_const :: [Pattern SRTree] -> RewriteCondition (Maybe Double) SRTree
any_not_const vs subst egr =
    any (\v -> egr L.^._class (unsafeGetSubst v subst)._data == Nothing) vs

is_not_neg_const :: Pattern SRTree -> RewriteCondition (Maybe Double) SRTree
is_not_neg_const v1 subst egr =
    (fmap (>=0) (egr L.^._class (unsafeGetSubst v1 subst)._data) == Just True)

is_not_neg_consts :: Pattern SRTree -> Pattern SRTree -> RewriteCondition (Maybe Double) SRTree
is_not_neg_consts v1 v2 subst egr =
    (fmap (>=0) (egr L.^._class (unsafeGetSubst v1 subst)._data) == Just True) ||
    (fmap (>=0) (egr L.^._class (unsafeGetSubst v2 subst)._data) == Just True) -- &&
   -- ((fmap (<0) (egr L.^._class (unsafeGetSubst v1 subst)._data) == Just True) -- &&
   --  (fmap (<0) (egr L.^._class (unsafeGetSubst v2 subst)._data) == Just True)
   -- )

is_negative :: Pattern SRTree -> RewriteCondition (Maybe Double) SRTree
is_negative v subst egr =
    fmap (<0) (egr L.^._class (unsafeGetSubst v subst)._data) == Just True

is_const :: Pattern SRTree -> RewriteCondition (Maybe Double) SRTree
is_const v subst egr =
    case (egr L.^._class (unsafeGetSubst v subst)._data) of
         Nothing -> False
         Just x  -> not (isNaN x) && not (isInfinite x)

is_not_const :: Pattern SRTree -> RewriteCondition (Maybe Double) SRTree
is_not_const v subst egr =
    isNothing (egr L.^._class (unsafeGetSubst v subst)._data)

is_not_nan :: Pattern SRTree -> RewriteCondition (Maybe Double) SRTree
is_not_nan v subst egr =
  case egr L.^._class (unsafeGetSubst v subst)._data of
       Nothing -> True
       Just x  -> not (isNaN x) && not (isInfinite x)

has_no_term :: Pattern SRTree -> RewriteCondition (Maybe Double) SRTree
has_no_term v subst egr =
    any (is_term . unNode) (egr L.^._class (unsafeGetSubst v subst)._nodes)
  where
    is_term = \case
                    Var _ -> True
                    Param _ -> True
                    Const _ -> True
                    _ -> False

is_not_same_var :: Pattern SRTree -> Pattern SRTree -> RewriteCondition (Maybe Double) SRTree
is_not_same_var v1 v2 subst egr = find (unsafeGetSubst v1 subst) egr /= find (unsafeGetSubst v2 subst) egr

has_no_loop :: Pattern SRTree -> Pattern SRTree -> RewriteCondition (Maybe Double) SRTree
has_no_loop v1 v2 subst egr =
    let cId1 = find (unsafeGetSubst v1 subst) egr 
        cId2 = find (unsafeGetSubst v2 subst) egr
        c1 = map canon $ getChildren cId1
        c2 = map canon $ getChildren cId2
        gc1 = concatMap (map canon . getChildren) c1
        gc2 = concatMap (map canon . getChildren) c2
    in null (filter (==cId1) (c2 <> gc2)) && null (filter (==cId2) (c1 <> gc1))
  where
    getChildren cid = concatMap children $ egr L.^._class cid . _nodes
    canon c = find c egr

rewritesBasic :: [Rewrite (Maybe Double) SRTree]
rewritesBasic =
    [   -- commutativity
        "x" + "y" := "y" + "x" :| has_no_loop "x" "y" :| any_not_const ["x", "y"]
      --, "x" - "y" := "x" + (negate "y")  :| any_not_const ["x", "y"] :| has_no_loop "x" "y"
      , "x" * "y" := "y" * "x" :| has_no_loop "x" "y" :| any_not_const ["x", "y"]
      , "x" * "x" := "x" ** 2 :| is_not_const "x"
      , ("x" ** "a") * "x" := "x" ** ("a" + 1) :| is_const "a" :| is_not_const "x"
      , ("x" ** "a") * ("x" ** "b") := "x" ** ("a" + "b")  :| any_not_const ["x", "a", "b"]
      -- associativity
      , ("x" + "y") + "z" := "x" + ("y" + "z") :| any_not_const ["x", "y", "z"]
      , ("x" + "y") - "z" := "x" + ("y" - "z") :| any_not_const ["x", "y", "z"]
      , ("x" * "y") * "z" := "x" * ("y" * "z") :| any_not_const ["x", "y", "z"]
      --, ("a" * "x") * ("b" * "y") := ("a" * "b") * ("x" * "y") :| any_not_const ["x", "y", "a", "b"]
      --, "x" * ("y" / "z") := ("x" * "y") / "z" :| any_not_const ["x", "y", "z"]
      --, ("x" * "y") / "z" := "x" * ("y" / "z")  :| any_not_const ["x", "y", "z"]
      --, "x" * ("y" / "z") :=  "y" * ("x" / "z") :| any_not_const ["x", "y", "z"]
      -- distributive and factorization
      , ("x" * "y") + ("x" * "z") := "x" * ("y" + "z") :| any_not_const ["x", "y", "z"]
      , "x" - ("y" + "z") := ("x" - "y") - "z" :| any_not_const ["x", "y", "z"]
      , "x" - ("y" - "z") := ("x" - "y") + "z" :| any_not_const ["x", "y", "z"]
      --, negate ("x" + "y") := negate "x" - "y" :| any_not_const ["x", "y"]
      --, ("a" / "x") * ("b" / "y") := ("a" * "b") / ("x" * "y") :| any_not_const ["x", "y", "a", "b"] :| is_const "a" :| is_const "b"
      , ("a" * "x") / ("b" * "y") := ("a" / "b") * ("x" / "y") :| any_not_const ["x", "y", "a", "b"] :| is_const "a" :| is_const "b"
      --, "x" / ("y" * "z") := ("x" / "y") / "z" :| any_not_const ["x", "y", "z"]
      , ("x" * "y") / "z" := ("x" / "z") * "y" :| any_not_const ["x", "y", "z"] :| is_const "x" :| is_const "z"
      , ("x" * "y") / "x" := "y" :| any_not_const ["x", "y"]
   ]

-- Rules for nonlinear functions
rewritesFun :: [Rewrite (Maybe Double) SRTree]
rewritesFun = [
        log ("x" * "y") := log "x" + log "y" :| is_not_neg_consts "x" "y" :| all_not 0 ["x", "y"] :| all_not 1 ["x", "y"]  :| any_not_const ["x", "y"]
      , log ("x" / "y") := log "x" - log "y" :| is_not_neg_consts "x" "y" :| all_not 0 ["x", "y"] :| all_not 1 ["x", "y"] :| any_not_const ["x", "y"]
      , log ("x" ** "y") := "y" * log "x" :| is_not_neg_const "y" :| all_not 0 ["x", "y"] :| all_not 1 ["x", "y"] :| any_not_const ["x", "y"]
      , log (sqrt "x") := 0.5 * log "x" :| is_not_const "x"
      , log (exp "x") := "x" :| is_not_const "x"
      , exp (log "x") := "x" :| is_not_const "x"
      , "x" ** (1/2) := sqrt "x" :| is_not_const "x"
      , sqrt ("a" * "x") := sqrt "a" * sqrt "x" :| is_not_neg_consts "a" "x" :| any_not_const ["x", "a"]
      , sqrt ("a" * ("x" - "y")) := sqrt (negate "a") * sqrt ("y" - "x") :| is_negative "a" :| any_not_const ["x", "y", "a"]
      , sqrt ("a" * ("b" + "y")) := sqrt (negate "a") * sqrt ("b" - "y") :| is_negative "a" :| is_negative "b" :| any_not_const ["b", "y", "a"]
      , sqrt ("a" / "x") := sqrt "a" / sqrt "x" :| is_not_neg_consts "a" "x" :| any_not_const ["x", "a"]
      , abs ("x" * "y") := abs "x" * abs "y" :| all_not 0 ["x", "y"] :| all_not 1 ["x", "y"] :| any_not_const ["x", "y"]
    ]

-- Rules that reduces redundant parameters
constReduction :: [Rewrite (Maybe Double) SRTree]
constReduction = [
      -- identities
        0 + "x" := "x" :| is_not_const "x"
      , "x" - 0 := "x" :| is_not_const "x"
      , 1 * "x" := "x" :| is_not_const "x"
      , 0 * "x" := 0   :| is_not_const "x"
      , 0 / "x" := 0   :| is_not_const "x"
      -- cancellations
      , "x" - "x" := 0 :| is_not_const "x"
      , "x" / "x" := 1 :| all_not 0 ["x"] :| is_not_const "x"
      , "x" ** 1 := "x" :| is_not_const "x"
      , 0 ** "x" := 0 :| is_not_const "x"
      , 1 ** "x" := 1 :| is_not_const "x"
      -- multiplication of inverse
      , "x" * (1 / "x") := 1 :| all_not 0 ["x"] :| is_not_const "x"
      -- negate 
      , "x" + negate "y" := "x" - "y" :| is_not_const "y"
      , 0 - "x" := negate "x" :| is_not_const "x"
    ]

rewriteTree :: (Analysis a l, Language l, Ord cost) => [Rewrite a l] -> Int -> Int -> CostFunction l cost -> Fix l -> Fix l
rewriteTree rules n coolOff c t = fst $ equalitySaturation' (BackoffScheduler n coolOff) t rules c

rewriteAll, rewriteConst :: Fix SRTree -> Fix SRTree
rewriteAll   = rewriteTree  (rewritesBasic <> constReduction <> rewritesFun) 5000 5 cost
rewriteConst = rewriteTree (rewritesBasic <> constReduction) 100 10 cost

rewriteUntilNoChange :: [Fix SRTree -> Fix SRTree] -> Int -> Fix SRTree -> Fix SRTree
rewriteUntilNoChange _ 0 t = t
rewriteUntilNoChange rs n t
  | t == t'   = t'
  | otherwise = rewriteUntilNoChange (tail rs <> [head rs]) (n-1) t'
  where t' = head rs t

simplifyEqSat :: R.Fix SRTree -> R.Fix SRTree
-- simplifyEqSat = relabelParams . fromEqFix . rewriteUntilNoChange [rewriteAll] 2 . rewriteConst . toEqFix
simplifyEqSat = relabelParams . fromEqFix . rewriteAll . rewriteAll . rewriteAll . toEqFix
--simplifyEqSat = relabelParams . fromEqFix . rewriteConst . toEqFix

fromEqFix :: Fix SRTree -> R.Fix SRTree
fromEqFix = cata alg
  where
    alg (Const x) = R.Fix (Const x)
    alg (Var ix) = R.Fix (Var ix)
    alg (Param ix) = R.Fix (Param ix)
    alg (Bin op l r) = R.Fix (Bin op l r)
    alg (Uni f t) = R.Fix (Uni f t)

toEqFix :: R.Fix SRTree -> Fix SRTree
toEqFix = R.cata alg
  where
    alg (Const x) = Fix (Const x)
    alg (Var ix) = Fix (Var ix)
    alg (Param ix) = Fix (Param ix)
    alg (Bin op l r) = Fix (Bin op l r)
    alg (Uni f t) = Fix (Uni f t)
