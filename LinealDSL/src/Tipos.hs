{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Módulo      : Tipos
-- Descripcion : Tipos base del DSL. 
--               Inspirado en Abe & Sumii (2019): verificación estática de tamaños mediante tipos fantasma.
--
-- Este módulo define el tipo 'Matrix' parametrizado por número de filas y columnas.
-- Las dimensiones se codifican a nivel de tipo usando 'DataKinds' y 'Nat', 
-- lo que permite al compilador verificar compatibilidad de tamaños en tiempo de compilación.

module Tipos
  ( Matrix(..)
  , fromList
  , identity
  ) where

import GHC.TypeLits
import Data.Proxy

-- |  Matriz con dimensiones en el tipo.
--   Los parámetros 'r' y 'c' son "phantom types" que representan filas y columnas.
data Matrix (r :: Nat) (c :: Nat) a = Matrix [[a]]
  deriving (Eq, Show)

-- | Constructor seguro: valida las dimensiones en tiempo de ejecución y las fija en el tipo.
fromList :: forall r c a. (KnownNat r, KnownNat c)
         => [[a]] -> Maybe (Matrix r c a)
fromList xs
  | length xs == r && all ((== c) . length) xs = Just (Matrix xs)
  | otherwise = Nothing
  where
    r = fromIntegral $ natVal (Proxy :: Proxy r)
    c = fromIntegral $ natVal (Proxy :: Proxy c)

-- | Matriz identidad
identity :: forall a n. (Num a, KnownNat n) => Matrix n n a
identity =
  let n = fromInteger (natVal (Proxy :: Proxy n))
  in Matrix [[if i == j then 1 else 0 | j <- [1..n]] | i <- [1..n]]

