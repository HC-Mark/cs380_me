{-# LANGUAGE GADTs, TypeInType, StandaloneDeriving, EmptyCase,TypeOperators,TypeFamilies, UndecidableInstances,AllowAmbiguousTypes #-}
 {-# OPTIONS_GHC -Wincomplete-patterns #-}

module Assign7 where

import Data.Kind ( Type )
import Prelude hiding ( (++),concat,foldr,concatMap )

data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat

data Vec :: Nat -> Type -> Type where
  Nil  :: Vec Zero a
  (:>) :: a -> Vec n a -> Vec (Succ n) a
infixr 5 :>
deriving instance Show a => Show (Vec n a)

data Fin :: Nat -> Type where
  FZero :: Fin (Succ n)
  FSucc :: Fin n -> Fin (Succ n)

data EVec :: (Nat -> Type) -> Type -> Type where
  EVec :: p n -> Vec n a -> EVec p a

instance (Show a) => Show (EVec c a) where
 show (EVec _ v) = show v

data AlwaysTrue :: Nat -> Type where
  Always :: AlwaysTrue n

type family Sum (ns :: [Nat]) :: Nat where
 Sum '[] = Zero
 Sum (n : ns) = n + Sum ns

data VecList :: [Nat] -> Type -> Type where
   VLNil :: VecList '[] a
   (:>>) :: Vec n a -> VecList ns a -> VecList (n ': ns) a
infixr 5 :>>

deriving instance Show a => Show (VecList ns a)

type family (a :: Nat) + (b :: Nat) :: Nat where
   Zero   + b = b
   Succ a + b = Succ (a + b)
infixl 6 +

type family (a :: Nat) * (b :: Nat) :: Nat where
   Zero   * b = Zero
   Succ a * b = b + (a * b)
infixl 7 *

(++) :: Vec n a -> Vec m a -> Vec (n + m) a
Nil       ++ ys = ys
(x :> xs) ++ ys = x :> (xs ++ ys)
infixr 5 ++

concat :: VecList ns a -> Vec (Sum ns) a
concat VLNil        = Nil
concat (xs :>> xss) = xs ++ concat xss

--examples
stuffs = (1 :> 2 :> Nil) :>> (3 :> Nil) :>> (4 :> 5 :> 6 :> Nil) :>> VLNil
stuff = 3 :> 5:> 8:> Nil
--helper function
foldr :: (a -> b -> b) -> b -> Vec n a -> b
foldr _ x Nil = x
foldr f x ( y :> ys ) = (f y (foldr f x ys))

--1. concatMap
{-
type family NatToList (n:: Nat) (m:: Nat) :: [Nat] where
  NatToList n Zero      = '[]
  NatToList n (Succ m') = n : NatToList n m'

concatMap ::(a -> Vec n b) -> Vec m a -> Vec (n * m) b
concatMap _ Nil = Nil
concatMap f (x:>xs) = concat $ (f x) :>> storeInVecList f xs
     where
       storeInVecList :: (a -> Vec n b) -> Vec m a -> VecList (NatToList n m) b
       storeInVecList = undefined
-}

concatMap :: (a ->EVec AlwaysTrue b) -> Vec n a -> EVec AlwaysTrue b
concatMap _ Nil = EVec Always Nil
concatMap f (x :> xs) = case concatMap f xs of EVec Always xs' -> case f x of EVec Always ys' -> EVec Always (xs'++ys')

testDouble :: a -> EVec AlwaysTrue a
testDouble n = EVec Always (n :> n :> Nil)