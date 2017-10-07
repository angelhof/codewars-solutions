{-# LANGUAGE NoImplicitPrelude, GADTs , DataKinds, TypeFamilies, TypeOperators, RankNTypes, DeriveFunctor #-}

module Singletons where

import Prelude hiding (drop, take, head, tail, index, zipWith, replicate, map, (++))

data Vec a n where
  VNil :: Vec a Zero
  VCons :: a -> Vec a n -> Vec a (Succ n)

-- promoted to type level by data kinds
data Nat = Zero | Succ Nat

data SNat a where
  SZero :: SNat Zero
  SSucc :: SNat a -> SNat (Succ a)

type family (a :: Nat) :< (b :: Nat) :: Bool
type instance m :< Zero = False
type instance Zero :< Succ n = True
type instance (Succ m) :< (Succ n) = m :< n

type family (Add (a :: Nat) (b :: Nat)) :: Nat
type instance (Add Zero n) = n
type instance (Add (Succ a) n) = (Succ (Add a n))

type family (Sub (a :: Nat) (b :: Nat)) :: Nat
type instance (Sub n Zero) = n
type instance (Sub Zero n) = Zero
type instance (Sub (Succ a) (Succ b)) = (Sub a b)

type family (Min (a :: Nat) (b :: Nat)) :: Nat
type instance (Min Zero b) = Zero
type instance (Min a Zero) = Zero
type instance (Min (Succ a) (Succ b)) = (Succ (Min a b))


map :: (a -> b) -> Vec a n -> Vec b n
map f VNil = VNil
map f (VCons x xs) = VCons (f x) (map f xs)

index :: ((a :< b) ~ True) => SNat a -> Vec s b -> s
index SZero (VCons x _) = x
index (SSucc i) (VCons x xs) = index i xs


replicate :: s -> SNat a -> Vec s a
replicate x SZero = VNil
replicate x (SSucc n) = VCons x $ replicate x n

-- Both vectors must be of equal length
zipWith :: (s1 -> s2 -> s3) -> Vec s1 a -> Vec s2 a -> Vec s3 a
zipWith _ VNil VNil = VNil
zipWith f (VCons x xs) (VCons y ys) = VCons (f x y) $ zipWith f xs ys

(++) :: Vec v m -> Vec v n -> Vec v (Add m n)
VNil ++ b = b
(VCons x xs) ++ b = VCons x $ xs ++ b

-- The semantics should match that of take for normal lists.
take :: SNat b -> Vec s a -> Vec s (Min a b)
take SZero _ = VNil
take _ VNil = VNil
take (SSucc i) (VCons x xs) = VCons x $ take i xs

-- The semantics should match that of drop for normal lists.
drop :: SNat b -> Vec s a -> Vec s (Sub a b)
drop SZero ls = ls
drop _ VNil = VNil
drop (SSucc i) (VCons x xs) = drop i xs

head :: ((Zero :< a) ~ True) => Vec s a -> s
head (VCons x _) = x

tail :: ((Zero :< a) ~ True) => Vec s a -> Vec s (Sub a (Succ Zero))
tail (VCons _ xs) = xs
