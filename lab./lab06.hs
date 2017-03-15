{-# LANGUAGE GADTs, TypeInType, StandaloneDeriving, TypeFamilies,
             TypeOperators #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Class where

import Data.Kind
import Prelude hiding ( reverse, (++), last, map,and, or, any, take ,drop,unzip,uncons,init )

data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat

data Vec :: Nat -> Type -> Type where
  Nil  :: Vec Zero a
  (:>) :: a -> Vec n a -> Vec (Succ n) a
infixr 5 :>

deriving instance Show a => Show (Vec n a)

stuff = 5 :> 3 :> 8 :> Nil

safeHead :: Vec (Succ n) a -> a
safeHead (x :> _) = x
-- NO!  safeHead Nil      = error "urk"

safeTail :: Vec (Succ n) a -> Vec n a
safeTail (_ :> xs) = xs

snoc :: Vec n a -> a -> Vec (Succ n) a
snoc Nil       x = x :> Nil
snoc (y :> ys) x = y :> snoc ys x

reverse :: Vec n a -> Vec n a
reverse Nil       = Nil
reverse (x :> xs) = snoc (reverse xs) x

reverseList :: [a] -> [a]
reverseList xs = go [] xs
  where
    go acc []     = acc
    go acc (y:ys) = go (y:acc) ys
{-
reverseVec :: Vec n a -> Vec n a
reverseVec xs = go Nil xs
  where
    go :: Vec m a -> Vec p a -> Vec (m + p) a
    go acc Nil     = acc
    go acc (y:>ys) = go (y :> acc) ys
-}

type family Plus (a :: Nat) (b :: Nat) :: Nat where
  Plus Zero     b = b
  Plus (Succ a) b = Succ (Plus a b)

type family a + b where
  Zero   + b = b
  Succ a + b = Succ (a + b)
infixl 6 +
-- (+) is infix left associative and has a precedence of 6

(++) :: Vec n a -> Vec m a -> Vec (n + m) a
Nil       ++ v2 = v2
(x :> xs) ++ v2 = x :> (xs ++ v2)

last :: Vec (Succ n) a -> a
last (x :> Nil) = x
last (_ :> xs@( _ :> _)) = last xs

map :: (a -> b) -> Vec n a -> Vec n b
map f (x :> xs) = (f x) :> (map f xs)
map f Nil = Nil

test = False :> False :> False :> Nil
test1 = True :> True :> True :> Nil

and :: Vec (Succ n) Bool -> Bool
and (x :> Nil) = x
and (x :> xs@(_ :> _))  = x && (and xs)

or :: Vec (Succ n) Bool -> Bool
or (x :> Nil) = x
or (x :> xs@(_ :> _)) = x || (or xs)

any :: (a -> Bool) -> Vec n a -> Bool
any f Nil = False
any f (x :> xs) = (f x) || (any f xs)
-- if we use Succ n here, ghc does not know xs is non-empty

unzip :: Vec n (a,b) -> ( (Vec n a), (Vec n b) )
unzip Nil = (Nil, Nil);
unzip ( x :> xs ) = ( (fst x) :> (fst $ unzip xs), (snd x) :> (snd $ unzip xs))

test2 = (1,2) :> (9,10) :> (12,3) :> Nil