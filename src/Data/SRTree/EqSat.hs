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
        n1 = egr L.^._class cId1 . _nodes
        n2 = egr L.^._class cId2 . _nodes
        c1 = map (\c -> find c egr) $ concatMap children n1
        c2 = map (\c -> find c egr) $ concatMap children n2
     in null (filter (==cId1) c2) && null (filter (==cId2) c1)


rewritesBasic :: [Rewrite (Maybe Double) SRTree]
rewritesBasic =
    [   -- commutativity
        "x" + "y" := "y" + "x" :| has_no_loop "x" "y" -- :| all_not 0 ["x", "y"]
      , "a" - "b" := "a" + (negate "b")
      , "x" * "y" := "y" * "x" :| has_no_loop "x" "y" -- :| all_not 0 ["x", "y"] :| all_not 1 ["x", "y"]
      , "x" * "x" := "x" ** 2 :| is_not_const "x"
      , ("x" ** "a") * "x" := "x" ** ("a" + 1) :| is_const "a"
      , ("x" ** "a") * ("x" ** "b") := "x" ** ("a" + "b")
      -- associativity
      , ("x" + "y") + "z" := "x" + ("y" + "z") -- :| all_not 0 ["x", "y", "z"]
      , ("x" * "y") * "z" := "x" * ("y" * "z") -- :| all_not 0 ["x", "y", "z"] :| all_not 1 ["x", "y", "z"]
      , "x" * ("y" / "z") := ("x" * "y") / "z"
      , ("x" * "y") / "z" := "x" * ("y" / "z")
      -- distributive and factorization
      --, "x" * ("y" + "z") := "x" * "y" + "x" * "z" -- :| all_not 0 ["x", "y", "z"] :| all_not 1 ["x"]
      , ("x" * "y") + ("x" * "z") := "x" * ("y" + "z")  -- :| all_not 1 ["x", "y", "z"]
      , "x" - ("y" + "z") := ("x" - "y") - "z" -- :| all_not 0 ["x", "y", "z"]
      , "x" - ("y" - "z") := ("x" - "y") + "z" -- :| all_not 0 ["x", "y", "z"]
      , negate ("x" + "y") := negate "x" - "y" -- :| all_not 0 ["x", "y"]
      --, ("x" - "a") := "x" + negate "a" :| is_const "a" :| is_not_const "x"
      , ("x" - ("a" * "y")) := "x" + (negate "a" * "y") -- :| is_const "a" :| is_not_const "y" :| all_not 0 ["a"] :| all_not 1 ["a"]
      , (1 / "x") * (1 / "y") := 1 / ("x" * "y") -- :| all_not 0 ["x", "y"] :| all_not 1 ["x", "y"]
   ]

-- Rules for nonlinear functions
rewritesFun :: [Rewrite (Maybe Double) SRTree]
rewritesFun = [
        log ("x" * "y") := log "x" + log "y" :| is_not_neg_consts "x" "y" :| all_not 0 ["x", "y"] :| all_not 1 ["x", "y"]
      , log ("x" / "y") := log "x" - log "y" :| is_not_neg_consts "x" "y" :| all_not 0 ["x", "y"] :| all_not 1 ["x", "y"]
      , log ("x" ** "y") := "y" * log "x" :| is_not_neg_const "y" :| all_not 0 ["x", "y"] :| all_not 1 ["x", "y"]
      , log (sqrt "x") := 0.5 * log "x" :| is_not_const "x"
      , log (exp "x") := "x" -- :| is_not_const "x"
      , exp (log "x") := "x" -- :| is_not_const "x"
      , "x" ** (1/2) := sqrt "x"
      , sqrt ("a" * "x") := sqrt "a" * sqrt "x" :| is_not_neg_consts "a" "x"
      , sqrt ("a" * ("x" - "y")) := sqrt (negate "a") * sqrt ("y" - "x") :| is_negative "a"
      , sqrt ("a" * ("b" + "y")) := sqrt (negate "a") * sqrt ("b" - "y") :| is_negative "a" :| is_negative "b"
      , sqrt ("a" / "x") := sqrt "a" / sqrt "x" :| is_not_neg_consts "a" "x"
      , abs ("x" * "y") := abs "x" * abs "y" :| all_not 0 ["x", "y"] :| all_not 1 ["x", "y"] -- :| is_const "x"
    ]

-- Rules that reduces redundant parameters
constReduction :: [Rewrite (Maybe Double) SRTree]
constReduction = [
      -- identities
        0 + "x" := "x" :| is_not_const "x"
      --, "x" + 0 := "x" :| is_not_const "x"
      , "x" - 0 := "x" :| is_not_const "x"
      , 1 * "x" := "x" :| is_not_const "x"
      --, "x" * 1 := "x" :| is_not_const "x" -- this creates a loop, why?
      , 0 * "x" := 0   :| is_not_const "x"
      --, "x" * 0 := 0   :| is_not_const "x"
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
      , "x" - ( (-1) * "y") := "x" + "y" :| is_not_const "y" :| is_not_const "x"
      , "x" + negate "y" := "x" - "y" :| is_not_const "y" :| is_not_const "x"
      , 0 - "x" := negate "x" :| is_not_const "x"
      -- constant fusion
      , "c" * ("a" * "x" + "b" * "y") := ("a" * "c") * "x" + ("b" * "c") * "y" :| is_const "a" :| is_const "b" :| is_const "c" :| is_not_const "x" :| is_not_const "y"
      , ("a" * "x") + ("b" * "y") := "a" * ("x" + ("b"/"a") * "y") :| is_const "a" :| is_const "b" :| is_not_const "x" :| is_not_const "y" :| all_not 0 ["a"]
      , ("a" * "x") * ("b" * "y") := ("a" * "b") * ("x" * "y") :| is_const "a" :| is_const "b" :| is_not_const "x" :| is_not_const "y"
      , "a" / ("b" * "x") := ("a" / "b") / "x" :| is_const "a" :| is_const "b" :| is_not_const "x"
     , ("a" * "x") / "y" := "a" * ("x" / "y") -- :| is_const "a" :| is_not_const "x" :| is_not_const "y"
    ]

-- Rules that moves parameters to the outside and to the left
constFusion :: [Rewrite (Maybe Double) SRTree]
constFusion = [
        "a" * "x" + "b" := "a" * ("x" + ("b" / "a")) :| is_const "a" :| is_const "b" :| is_not_const "x" :| all_not 0 ["a"]
      , "a" * "x" + "b" / "y" := "a" * ("x" + ("b" / "a") / "y") :| is_const "a" :| is_const "b" :| is_not_const "x" :| is_not_const "y" :| all_not 0 ["a"]
      , "a" * "x" - "b" / "y" := "a" * ("x" - ("b" / "a") / "y") :| is_const "a" :| is_const "b" :| is_not_const "x" :| is_not_const "y" :| all_not 0 ["a"]
      , "x" / ("b" * "y") := (1 / "b") * "x" / "y" :| is_const "b" :| is_not_const "x" :| is_not_const "y" :| all_not 0 ["b"]
      , "x" / "a" + "b" := (1 / "a") * ("x" + ("b" * "a")) :| is_const "a" :| is_const "b" :| is_not_const "x" :| all_not 0 ["a"]
      , "x" / "a" - "b" := (1 / "a") * ("x" - ("b" * "a")) :| is_const "a" :| is_const "b" :| is_not_const "x" :| all_not 0 ["a"]
      , "b" - "x" / "a" := (1 / "a") * (("b" * "a") - "x") :| is_const "a" :| is_const "b" :| is_not_const "x" :| all_not 0 ["a"]
      , "x" / "a" + "b" * "y" := (1 / "a") * ("x" + ("b" * "a") * "y") :| is_const "a" :| is_const "b" :| is_not_const "x" :| is_not_const "y" :| all_not 0 ["a"]
      , "x" / "a" + "y" / "b" := (1 / "a") * ("x" + "y" / ("b" * "a")) :| is_const "a" :| is_const "b" :| is_not_const "x" :| is_not_const "y" :| all_not 0 ["a"]
      , "x" / "a" - "b" * "y" := (1 / "a") * ("x" - ("b" * "a") * "y") :| is_const "a" :| is_const "b" :| is_not_const "x" :| is_not_const "y" :| all_not 0 ["a"]
      , "x" / "a" - "b" / "y" := (1 / "a") * ("x" - "y" / ("b" * "a")) :| is_const "a" :| is_const "b" :| is_not_const "x" :| is_not_const "y" :| all_not 0 ["a"]
     , ("a" * "x") / ("b" * "y") := ("a"/"b") * ("x"/"y") :| is_const "a" :| is_const "b" :| is_not_const "x" :| is_not_const "y" :| all_not 0 ["a"]
    ]

gabriel :: [Rewrite (Maybe Double) SRTree]

gabriel =  [-- "a" + 0 := "a"
--  , "a" * 1 := "a"
--  , "a" - "a" := 0
  -- , "a" / 0 := 0/0
--   "a" / "a" := 1
 -- , 1 ** "a" := 1
 -- , "a" ** 1 := "a"
 -- , "a" + "b" * "a" := (1 + "b") * "a"
--  , "a" + ("b" + "c") := ("a" + "b") + "c" -- :| is_not_const "b" :| is_not_const "c"
 -- , "a" * ("b" * "c") := ("a" * "b") * "c" :| has_no_loop "a" "c" -- :| has_no_term "a" :| has_no_term "b" :| has_no_term "c"
  --, "a" * "a" := "a" ** 2
  --, 0 ** "a" := 0
 "a" - "b" := "a" + ((-1) * "b") 
 , "a" + "c" * "a" := "a" * (1 + "c")
 , "a" * 0 := 0
 , "a" / "b" := "a" * ("b"**(-1))
 , "a" + "b" := "b" + "a" :| has_no_loop "a" "b"
 , "a" * "b" := "b" * "a" :| has_no_loop "a" "b" -- :| has_no_term "a" :| has_no_term "b"
 , ("a" + "b") + "c" := "a" + ("b" + "c") :| is_not_const "a" :| is_not_const "b" -- :| is_not_const "a" :| is_not_const "b"
 , ("a" * "b") * "c" := "a" * ("b" * "c") :| is_not_const "a" :| is_not_const "b" -- :| has_no_loop "a" "c"
 , "a" * ("a" ** "b") := "a" ** ("b" + 1)
 , "a" ** 0 := 1
 , "a" * 1 := "a"
  ]

-- (x * x) * x^-1 

rewriteTree :: (Analysis a l, Language l, Ord cost) => [Rewrite a l] -> Int -> Int -> CostFunction l cost -> Fix l -> Fix l
rewriteTree rules n coolOff c t = fst $ equalitySaturation' (BackoffScheduler n coolOff) t rules c

rewriteAll, rewriteConst :: Fix SRTree -> Fix SRTree
rewriteAll   = rewriteTree  (rewritesBasic <> constReduction <>  constFusion <> rewritesFun) 2500 30 cost
rewriteConst = rewriteTree (rewritesBasic <> constReduction) 100 10 cost
--rewriteConst = rewriteTree (gabriel) 1000 0 cost

rewriteUntilNoChange :: [Fix SRTree -> Fix SRTree] -> Int -> Fix SRTree -> Fix SRTree
rewriteUntilNoChange _ 0 t = t
rewriteUntilNoChange rs n t
  | t == t'   = t'
  | otherwise = rewriteUntilNoChange (tail rs <> [head rs]) (n-1) t'
  where t' = head rs t

simplifyEqSat :: R.Fix SRTree -> R.Fix SRTree
-- simplifyEqSat = relabelParams . fromEqFix . rewriteUntilNoChange [rewriteAll] 2 . rewriteConst . toEqFix
simplifyEqSat = relabelParams . fromEqFix . rewriteAll . toEqFix
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
