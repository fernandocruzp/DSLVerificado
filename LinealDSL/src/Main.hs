{-# LANGUAGE DataKinds #-}

-- |
-- Module      : Main
-- Description : Ejemplos del DSL de álgebra lineal con verificación estática.
--
-- Demuestra la ejecución de operaciones tipadas que dependen del tamaño de las matrices:
-- multiplicación, determinante, traza, producto punto y norma.

module Main where

import Tipos
import Operaciones
import DSL


ejemploMul :: Maybe (Matrix 2 2 Int)
ejemploMul = do
  a <- fromList [[1,2,3],[4,5,6]]       :: Maybe (Matrix 2 3 Int)
  b <- fromList [[1,4],[2,5],[3,6]]     :: Maybe (Matrix 3 2 Int)
  pure (mul a b)


ejemploSquare :: Maybe (Matrix 2 2 Int)
ejemploSquare = fromList [[2,1],[1,3]]

ejemploTrace :: Maybe Int
ejemploTrace = fmap trace ejemploSquare

ejemploDet :: Maybe Int
ejemploDet = fmap det ejemploSquare


ejemploDot :: Maybe Double
ejemploDot = do
  v1 <- fromList [[1.0,2.0,3.0]] :: Maybe (Matrix 1 3 Double)
  v2 <- fromList [[1.0],[2.0],[3.0]] :: Maybe (Matrix 3 1 Double)
  pure (dot v1 v2)

ejemploNorm :: Maybe Double
ejemploNorm = do
  v <- fromList [[3.0,4.0]] :: Maybe (Matrix 1 2 Double)
  pure (norm v)

main :: IO ()
main = do
  putStrLn "=== DSL de Álgebra Lineal con Verificación Estática ==="
  print ejemploMul
  print ejemploTrace
  print ejemploDet
  print ejemploDot
  print ejemploNorm
