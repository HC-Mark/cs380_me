{-# LANGUAGE GADTs, TypeInType, StandaloneDeriving, EmptyCase,TypeOperators,TypeFamilies, UndecidableInstances,AllowAmbiguousTypes, TypeApplications #-}
 {-# OPTIONS_GHC -Wincomplete-patterns #-}

module Assign7 where

import Data.Kind ( Type )
import Prelude hiding ( (++),concat,foldr,concatMap,takeWhile,dropWhile )

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
stuff = 3 :> 5:> 8:> 2 :> Nil
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

concatMap :: (a -> EVec AlwaysTrue b) -> Vec n a -> EVec AlwaysTrue b
concatMap _ Nil = EVec Always Nil
concatMap  f (x :> xs) = case concatMap f xs of EVec Always xs' -> case f x of EVec Always ys' -> EVec Always (xs'++ys')

testDouble :: a -> EVec AlwaysTrue a
testDouble b = EVec  Always ( b :> b :> Nil)
--2.
--base case has some problems
unfoldr :: (b -> Maybe (a,b)) -> b -> EVec AlwaysTrue a
unfoldr f b = case f b of
                          Nothing   -> EVec Always Nil -- and stop here
                          Just(a,b) -> case unfoldr f b of x@(EVec Always xs') -> EVec Always (xs' ++ (a :> Nil))

--3.
data (:>=:) :: Nat -> Nat -> Type where
  GTEZero :: n :>=: Zero
  GTESucc :: n :>=: m -> Succ n :>=: Succ m

gteSuccLeft :: (n :>=: m) -> (Succ n :>=: m)
gteSuccLeft GTEZero       = GTEZero
gteSuccLeft (GTESucc gte) = GTESucc (gteSuccLeft gte)

--work but not constraint enough
takeWhile :: (a -> Bool) -> Vec n a -> EVec AlwaysTrue a
takeWhile _ Nil = EVec Always Nil
takeWhile f (x :> xs) = case f x of True -> case takeWhile f xs of EVec Always xs' -> EVec Always (x :> xs')
                                    otherwise -> EVec Always Nil 
{-
takeWhile :: (a -> Bool) -> Vec n a -> EVec ((:>=:) n) a
takeWhile _ Nil = EVec GTEZero Nil
takeWhile f (x :> xs) = case f x of
                                  | f x -> case takeWhile f xs of EVec gte xs' -> EVec (GTESucc gte) (x :> xs')
                                  | otherwise -> what should we do here, since it need something here to return the result EVec but I don't know how to pass the type check,which need me to use GTESucc or gteSuccLeft. 
-}
{-
takeWhile f (x :> xs) = case takeWhile f xs of
                                             EVec gte xs'
                                               | f x -> EVec (GTESucc gte)  (x :> xs')
                                               | otherwise -> EVec (gteSuccLeft gte) xs' -- how can I directly return this? now it just like filter 
-}
--4.dropWhile
{-
dropWhile :: (a -> Bool) -> Vec n a -> EVec ((:>=:) n) a
dropWhile _ Nil = EVec GTEZero Nil
dropWhile f (x :> xs) = case dropWhile f xs of EVec gte xs'
                                                | f x -> EVec (getSuccLeft gte) xs'
                                                | otherwise -> EVec (GTESucc gte) (x :> xs')
don't know how to use otherwise to stop at the first non-passed point and read in all the rest elements without pattern match f x any more
-}

dropWhile :: (a -> Bool) -> Vec n a -> EVec AlwaysTrue a
dropWhile _ Nil = EVec Always Nil
dropWhile f xs@(x :> xs')
     | f x = dropWhile f xs'
     | otherwise = EVec Always xs 

dropWhileEnd :: (a -> Bool) -> Vec n a -> EVec AlwaysTrue a
dropWhileEnd _ Nil = EVec Always Nil
dropWhileEnd f xs = foldr (\x xs'@(EVec Always ys) -> if f x && (x == Nil)  then (EVec Always Nil) else (EVec Always (x :> ys))) (EVec Always Nil) xs
-- dropwhileENd f xs = EVec Always (foldr (\x xs' -> if f x && (xs' == Nil) then Nil else x :> xs') Nil xs) this doesn't work since the function in foldr is (a -> b -> b) hence, we input Nil in b, so the out put should also be Nil, but we want a Vec (Succ zero) a instead, so not work
