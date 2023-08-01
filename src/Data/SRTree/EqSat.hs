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
import Data.Equality.Graph ( ClassId, Language, ENode(unNode) )
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
import Data.SRTree.Recursion qualified as R
import Data.Set qualified as S
import Text.Show.Deriving ( deriveShow1 )

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
    joinA ma mb = do
        a <- ma
        b <- mb
        !_ <- unless (abs (a-b) <= 1e-6 || a ~== b || (a == 0 && b == (-0)) || (a == (-0) && b == 0)) (error $ "Merged non-equal constants!" <> show a <> " " <> show b <> " " <> show (a==b))
        pure a
    modifyA cl = case cl L.^._data of
                 Nothing -> (cl, [])
                 Just d -> ((_nodes %~ S.filter (F.null .unNode)) cl, [Fix (Const d)])

evalConstant :: SRTree (Maybe Double) -> Maybe Double
evalConstant = \case
    -- Exception: Negative exponent: BinOp Pow e1 e2 -> liftA2 (^) e1 (round <$> e2 :: Maybe Integer)
    Bin Div e1 e2 -> liftA2 (/) e1 e2
    Bin Sub e1 e2 -> liftA2 (-) e1 e2
    Bin Mul e1 e2 -> liftA2 (*) e1 e2
    Bin Add e1 e2 -> liftA2 (+) e1 e2
    Bin Power e1 e2 -> liftA2 (**) e1 e2
    Uni f e1 -> evalFun f <$> e1
    Var _ -> Nothing
    Const x -> Just x -- TODO: investigate why it cannot handle NaN
    Param _ -> Nothing

instance Language SRTree

cost :: CostFunction SRTree Int
cost = \case
  Const _ -> 5
  Var _ -> 1
  Bin Add c1 c2 -> c1 + c2 + 1
  Bin Mul c1 c2 -> c1 + c2 + 1
  Bin _ c1 c2 -> c1 + c2 + 2
  Uni _ c -> c + 1
  Param _ -> 5

unsafeGetSubst :: Pattern SRTree -> Subst -> ClassId
unsafeGetSubst (NonVariablePattern _) _ = error "unsafeGetSubst: NonVariablePattern; expecting VariablePattern"
unsafeGetSubst (VariablePattern v) subst = case IM.lookup v subst of
      Nothing -> error "Searching for non existent bound var in conditional"
      Just class_id -> class_id

is_not_zero :: Pattern SRTree -> RewriteCondition (Maybe Double) SRTree
is_not_zero v subst egr =
    egr L.^._class (unsafeGetSubst v subst)._data /= Just 0

is_not_neg_consts :: Pattern SRTree -> Pattern SRTree -> RewriteCondition (Maybe Double) SRTree
is_not_neg_consts v1 v2 subst egr =
    (fmap (>=0) (egr L.^._class (unsafeGetSubst v1 subst)._data) == Just True) ||
    (fmap (>=0) (egr L.^._class (unsafeGetSubst v2 subst)._data) == Just True)

is_negative :: Pattern SRTree -> RewriteCondition (Maybe Double) SRTree
is_negative v subst egr =
    fmap (<0) (egr L.^._class (unsafeGetSubst v subst)._data) == Just True

is_const :: Pattern SRTree -> RewriteCondition (Maybe Double) SRTree
is_const v subst egr =
    isJust (egr L.^._class (unsafeGetSubst v subst)._data)

is_not_const :: Pattern SRTree -> RewriteCondition (Maybe Double) SRTree
is_not_const v subst egr =
    isNothing (egr L.^._class (unsafeGetSubst v subst)._data)

rewritesBasic :: [Rewrite (Maybe Double) SRTree]
rewritesBasic =
    [   -- commutativity
        "x" + "y" := "y" + "x"
      , "x" * "y" := "y" * "x"
      , "x" * "x" := "x" ** 2
      , ("x" ** "a") * "x" := "x" ** ("a" + 1)
      , ("x" ** "a") * ("x" ** "b") := "x" ** ("a" + "b")
      -- associativity
      , ("x" + "y") + "z" := "x" + ("y" + "z")
      , ("x" * "y") * "z" := "x" * ("y" * "z")
      -- , "x" * ("y" / "z") := ("x" * "y") / "z"
      --, ("x" * "y") / "z" := "x" * ("y" / "z")
      -- distributive and factorization
      , "x" - ("y" + "z") := ("x" - "y") - "z"
      , "x" - ("y" - "z") := ("x" - "y") + "z"
      , negate ("x" + "y") := negate "x" - "y"
      , ("x" - "a") := "x" + negate "a" :| is_const "a" :| is_not_const "x"
      , ("x" - ("a" * "y")) := "x" + (negate "a" * "y") :| is_const "a" :| is_not_const "y"
      , (1 / "x") * (1 / "y") := 1 / ("x" * "y")
   ]

-- Rules for nonlinear functions
rewritesFun :: [Rewrite (Maybe Double) SRTree]
rewritesFun = [
        log ("x" * "y") := log "x" + log "y" :| is_not_neg_consts "x" "x" :| is_not_zero "x" 
      , "x" ** "a" * "x" ** "b" := "x" ** ("a" + "b")
      , log ("x" / "y") := log "x" - log "y" :| is_not_neg_consts "x" "x" :| is_not_zero "x" 
      , log ("x" ** "y") := "y" * log "x" :| is_not_neg_consts "y" "y" :| is_not_zero "y"
      , log (sqrt "x") := 0.5 * log "x" :| is_not_const "x"
      , log (exp "x") := "x" :| is_not_const "x"
      , exp (log "x") := "x" :| is_not_const "x"
      , "x" ** (1/2) := sqrt "x"
      , sqrt ("a" * "x") := sqrt "a" * sqrt "x" :| is_not_neg_consts "a" "x"
      , sqrt ("a" * ("x" - "y")) := sqrt (negate "a") * sqrt ("y" - "x") :| is_negative "a"
      , sqrt ("a" * ("b" + "y")) := sqrt (negate "a") * sqrt ("b" - "y") :| is_negative "a" :| is_negative "b"
      , sqrt ("a" / "x") := sqrt "a" / sqrt "x" :| is_not_neg_consts "a" "x"
      , abs ("x" * "y") := abs "x" * abs "y" -- :| is_const "x"
    ]

-- Rules that reduces redundant parameters
constReduction :: [Rewrite (Maybe Double) SRTree]
constReduction = [
      -- identities
        0 + "x" := "x"
      , "x" + 0 := "x"
      , "x" - 0 := "x"
      --, 1 * "x" := "x"  -- this creates a loop, why?
      , "x" * 1 := "x"
      , 0 * "x" := 0
      , "x" * 0 := 0
      , 0 / "x" := 0
      -- cancellations 
      , "x" - "x" := 0
      , "x" / "x" := 1 :| is_not_zero "x"
      , "x" ** 1 := "x"
      , 0 ** "x" := 0
      , 1 ** "x" := 1
      -- multiplication of inverse
      , "x" * (1 / "x") := 1 :| is_not_zero "x"
      , ("x" * "y") + ("x" * "z") := "x" * ("y" + "z")
      -- negate 
      , "x" - ( (-1) * "y") := "x" + "y" :| is_not_const "y"
      , "x" + negate "y" := "x" - "y" :| is_not_const "y"
      , 0 - "x" := negate "x" :| is_not_const "x" 
      -- constant fusion
      --, "c" * ("a" * "x" + "b" * "y") := ("a" * "c") * "x" + ("b" * "c") * "y" :| is_const "a" :| is_const "b" :| is_const "c" :| is_not_const "x" :| is_not_const "y"
      , ("a" * "x") + ("b" * "y") := "a" * ("x" + ("b"/"a") * "y") :| is_const "a" :| is_const "b" :| is_not_const "x" :| is_not_const "y"
      , ("a" * "x") * ("b" * "y") := ("a" * "b") * ("x" * "y") :| is_const "a" :| is_const "b" :| is_not_const "x" :| is_not_const "y"
      , "a" / ("b" * "x") := ("a" / "b") / "x" :| is_const "a" :| is_const "b" :| is_not_const "x"
      , ("a" * "x") / "y" := "a" * ("x" / "y") :| is_const "a" :| is_not_const "x" :| is_not_const "y"
    ]

-- Rules that moves parameters to the outside and to the left
constFusion :: [Rewrite (Maybe Double) SRTree]
constFusion = [
        "a" * "x" + "b" := "a" * ("x" + ("b" / "a")) :| is_const "a" :| is_const "b" :| is_not_const "x"
      , "a" * "x" + "b" / "y" := "a" * ("x" + ("b" / "a") / "y") :| is_const "a" :| is_const "b" :| is_not_const "x" :| is_not_const "y"
      , "a" * "x" - "b" / "y" := "a" * ("x" - ("b" / "a") / "y") :| is_const "a" :| is_const "b" :| is_not_const "x" :| is_not_const "y"
      , "x" / ("b" * "y") := (1 / "b") * "x" / "y" :| is_const "b" :| is_not_const "x" :| is_not_const "y"
      , "x" / "a" + "b" := (1 / "a") * ("x" + ("b" * "a")) :| is_const "a" :| is_const "b" :| is_not_const "x"
      , "x" / "a" - "b" := (1 / "a") * ("x" - ("b" * "a")) :| is_const "a" :| is_const "b" :| is_not_const "x"
      , "b" - "x" / "a" := (1 / "a") * (("b" * "a") - "x") :| is_const "a" :| is_const "b" :| is_not_const "x"
      , "x" / "a" + "b" * "y" := (1 / "a") * ("x" + ("b" * "a") * "y") :| is_const "a" :| is_const "b" :| is_not_const "x" :| is_not_const "y"
      , "x" / "a" + "y" / "b" := (1 / "a") * ("x" + "y" / ("b" * "a")) :| is_const "a" :| is_const "b" :| is_not_const "x" :| is_not_const "y"
      , "x" / "a" - "b" * "y" := (1 / "a") * ("x" - ("b" * "a") * "y") :| is_const "a" :| is_const "b" :| is_not_const "x" :| is_not_const "y"
      , "x" / "a" - "b" / "y" := (1 / "a") * ("x" - "y" / ("b" * "a")) :| is_const "a" :| is_const "b" :| is_not_const "x" :| is_not_const "y"
      , ("a" * "x") / ("b" * "y") := ("a"/"b") * ("x"/"y") :| is_const "a" :| is_const "b" :| is_not_const "x" :| is_not_const "y"
    ]

rewriteTree :: (Analysis a l, Language l, Ord cost) => [Rewrite a l] -> Int -> Int -> CostFunction l cost -> Fix l -> Fix l
rewriteTree rules n coolOff c t = fst $ equalitySaturation' (BackoffScheduler n coolOff) t rules c

rewriteAll, rewriteConst :: Fix SRTree -> Fix SRTree
rewriteAll   = rewriteTree  (rewritesBasic <> constReduction <> constFusion <> rewritesFun) 2500 30 cost
rewriteConst = rewriteTree (rewritesBasic <> constReduction) 100 10 cost

rewriteUntilNoChange :: [Fix SRTree -> Fix SRTree] -> Int -> Fix SRTree -> Fix SRTree
rewriteUntilNoChange _ 0 t = t
rewriteUntilNoChange rs n t
  | t == t'   = t'
  | otherwise = rewriteUntilNoChange (tail rs <> [head rs]) (n-1) t'
  where t' = head rs t

simplifyEqSat :: R.Fix SRTree -> R.Fix SRTree
simplifyEqSat = relabelParams . fromEqFix . rewriteUntilNoChange [rewriteAll] 2 . rewriteConst . toEqFix

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
