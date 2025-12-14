{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Módulo      : DSL
-- Descripción : Definición de expresiones algebraicas del DSL y su semántica.
--
-- Este módulo proporciona una sintaxis algebraica declarativa
-- y un evaluador que interpreta las expresiones usando las operaciones seguras.

module DSL
  ( LinExpr(..)
  , eval
  ) where

import GHC.TypeLits
import Tipos
import Operaciones

-- | Expresiones algebraicas del DSL.
data LinExpr (r :: Nat) (c :: Nat) a where
  AddE :: LinExpr r c a -> LinExpr r c a -> LinExpr r c a
  MulE :: LinExpr r m a -> LinExpr m c a -> LinExpr r c a
  TransposeE :: LinExpr r c a -> LinExpr c r a
  DetE :: LinExpr n n a -> LinExpr 1 1 a
  TraceE :: LinExpr n n a -> LinExpr 1 1 a
  DotE :: LinExpr 1 n a -> LinExpr n 1 a -> LinExpr 1 1 a
  NormE :: LinExpr 1 n a -> LinExpr 1 1 a
  Const :: Matrix r c a -> LinExpr r c a


-- | Evaluación de las expresiones algebraicas.
eval :: (Floating a, Num a) => LinExpr r c a -> Matrix r c a
eval (Const m) = m
eval (AddE e1 e2) = add (eval e1) (eval e2)
eval (MulE e1 e2) = mul (eval e1) (eval e2)
eval (TransposeE e) = transposeM (eval e)
eval (DetE e) = Matrix [[det (eval e)]]
eval (TraceE e) = Matrix [[trace (eval e)]]
eval (DotE e1 e2) = Matrix [[dot (eval e1) (eval e2)]]
eval (NormE e) = Matrix [[norm (eval e)]]
