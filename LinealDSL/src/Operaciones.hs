{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Módulo    : Operations
-- Descripción : Operaciones lineales seguras sobre matrices tipadas.
-- 
-- Este módulo implementa operaciones básicas y extendidas:
-- suma, multiplicación, transposición, traza, producto punto y norma.
-- 
-- Todas las firmas aseguran la compatibilidad dimensional en tiempo de compilación.

module Operaciones
  ( add
  , mul
  , transposeM
  , det
  , trace
  , dot
  , norm
  ) where

import GHC.TypeLits
import Tipos

-- | Suma de matrices del mismo tamaño.
add :: Num a => Matrix r c a -> Matrix r c a -> Matrix r c a
add (Matrix a) (Matrix b) = Matrix (zipWith (zipWith (+)) a b)

-- | Multiplicación de matrices (r×m) × (m×c) → (r×c)
mul :: Num a => Matrix r m a -> Matrix m c a -> Matrix r c a
mul (Matrix a) (Matrix b) = Matrix [[ sum $ zipWith (*) row col | col <- transpose b ] | row <- a]
  where
    transpose ([]:_) = []
    transpose x = map head x : transpose (map tail x)

-- | Transposición de matrices.
transposeM :: Matrix r c a -> Matrix c r a
transposeM (Matrix a) = Matrix (transpose a)
  where
    transpose ([]:_) = []
    transpose x = map head x : transpose (map tail x)

-- | Determinante (solo para matrices cuadradas pequeñas, recursivo simple).
det :: Num a => Matrix n n a -> a
det (Matrix [[x]]) = x
det (Matrix m) = sum [(-1) ^ j * head m !! j * det (Matrix (minor j m)) | j <- [0 .. length m - 1]]
  where
    minor j m = [ remove j row | row <- tail m ]
    remove j row = take j row ++ drop (j +1 ) row


-- | Traza de una matriz cuadrada: suma de elementos diagonales.
trace :: Num a => Matrix n n a -> a
trace (Matrix m) = sum [m !! i !! i | i <- [0 .. length m - 1]]

-- | Producto punto (dot product) entre dos vectores compatibles.
--   Requiere que uno sea de tipo 1×n y el otro de tipo n×1.
dot :: Num a => Matrix 1 n a -> Matrix n 1 a -> a
dot (Matrix [xs]) (Matrix ys) = sum $ zipWith (*) xs (concat ys)

-- | Norma euclidiana de un vector fila.
norm :: Floating a => Matrix 1 n a -> a
norm v = sqrt (dot v (transposeM v))
