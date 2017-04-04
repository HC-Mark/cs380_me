
{-# LANGUAGE GADTs, TypeInType, StandaloneDeriving, TypeFamilies,
             TypeOperators, ScopedTypeVariables,UndecidableInstances,
             TypeApplications,AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Assign06 where   -- rename as you please
import Data.Kind ( Type )
import Data.Type.Equality
import Prelude hiding (intersperse,inits);
data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat

data Vec :: Nat -> Type -> Type where
  Nil  :: Vec Zero a
  (:>) :: a -> Vec n a -> Vec (Succ n) a
infixr 5 :>

deriving instance Show a => Show (Vec n a)

type family (a :: Nat) + (b :: Nat) :: Nat where
  Zero   + b = b
  Succ a + b = Succ (a + b)
infixl 6 +

type family (a :: Nat) * (b :: Nat) :: Nat where
   Zero   * b = Zero
   Succ a * b = b + (a * b)
infixl 7 *

-- singleton Nat
data SNat :: Nat -> Type where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)

-- singleton Bool
data SBool :: Bool -> Type where
  SFalse :: SBool False
  STrue  :: SBool True
{-
(++) :: Vec n a -> Vec m a -> Vec (n + m) a
Nil       ++ ys = ys
(x :> xs) ++ ys = x :> (xs ++ ys)
infixr 5 ++
-}
plusZero :: SNat m -> (m + Zero) :~: m
plusZero SZero      = Refl
plusZero (SSucc m') = case plusZero m' of Refl -> Refl

plusSucc :: forall n m. SNat m -> (m + Succ n) :~: Succ (m + n)
plusSucc SZero      = Refl
plusSucc (SSucc m') = case plusSucc @n m' of Refl -> Refl
--proof part
--1. proof of (m + n) :~: (n + m)
--helper: new plusSucc

plusSucc' ::forall n m. SNat m -> SNat n -> (m + Succ n) :~: Succ (m +n)
plusSucc' SZero _ = Refl;
plusSucc' m n = case plusSucc @n m of Refl -> Refl;--why it works??
--plusSucc' (SSucc m') (SSucc n') = case plusSucc' m' n' of Refl -> Refl;


plusComm :: SNat m -> SNat n -> (m + n) :~: (n + m)
--plusComm SZero SZero = Refl;
--plusComm (SSucc m') SZero = case plusZero m' of Refl -> Refl;
plusComm SZero n = case plusZero n of Refl -> Refl;
plusComm (SSucc m') n = case plusSucc' n m' of Refl -> case plusComm m' n of Refl -> Refl
{- I gonna write a long analysis to note down what's going on
  1. First, we only need to consider one variable to use induction on it, so it is unnecessary to have both (SSucc m') (SSucc n'). If we decide to use induction on m, then we can just use n without induction for the other.
  2. In the last pattern of plusComm, we actually need to have two case-check here. The first time is when we use the mathematical induction, the second time is when we use plusSucc to help us switch (n + SSucc m') to (Succ (n + m'))
  3. the most important thing. The recursive call of plusSucc is on n instead of m. Since we want to change the right side from (n + m) to (Succ (n + m')), we do induction on n not m.( I spend a whole afternoon on figure out this problem)
  4. Though we can directly use plusSucc in plusComm, we can not figure out what type variable we will use to refer to m. If we use m, then we actually prove (n + Succ (Succ m')) = Succ (n + Succ m'), which is not what we want. ( we want to prove (n + Succ m') = Succ ( n + m')). And m' here is not a valid type variable. Hence, I have to write a helper function plusSucc' to take m' as an argument in plusSucc and sign it as type variable to plusSucc.
  5.we can have nested case of thing. when we want to check multiple cases.
-} 

--the last step asks us to do induction and refer to plusSucc at the same time and how to do that?
--2. proof of (m + n) + l = m + (n +l)
plusAsso :: forall m n l. SNat m -> SNat n -> SNat l -> (m+n) + l :~: m + (n+l)
plusAsso SZero n l = Refl;
plusAsso (SSucc m') n l = case plusAsso m' n l of Refl -> Refl

--3. proof of m * n = n * m

--helper functions
multZero :: SNat m -> Zero :~: (m * Zero)
multZero SZero = Refl;
multZero (SSucc m') = case multZero m' of Refl-> Refl

switch ::forall n m. SNat m -> SNat n -> ( m * Succ n) :~: ( m + (m * n))
switch SZero n = case multZero n of Refl ->Refl
switch (SSucc m') n = undefined

multComm :: forall n m. SNat m-> SNat n -> (m*n) :~: (n*m)
multComm SZero n = case multZero n of Refl -> Refl
multComm m SZero = case multZero m of Refl -> Refl
multComm (SSucc m') (SSucc n') = undefined

--implement list function on Vec
data VecList :: [Nat] -> Type -> Type where
   VLNil :: VecList '[] a
   (:>>) :: Vec n a -> VecList ns a -> VecList (n ': ns) a
infixr 5 :>>


type family a - b where
   a   - Zero           = a
   (Succ a) - (Succ b)  = a - b
   Zero - _ = Zero --a little bit cheating but work for Nat
infixl 6 -
--1.intersperse
{-
type family (a :: Nat) + (b :: Nat) :: Nat where
  Zero   + b = b
  Succ a + b = Succ (a + b)
infixl 6 +
-}
intersperse :: forall n a. a -> SNat n -> Vec n a -> Vec (n + (n - Succ Zero)) a -- I add a parenthese and it works!
intersperse _ SZero Nil = Nil
intersperse sep n@(SSucc n') (x:>xs) = case plusSucc @n n of Refl-> x :> prependToAll sep n' xs

plusBoth :: forall n m. SNat n -> SNat m -> (Succ n + Succ m) :~: Succ(Succ (n + m))
plusBoth SZero SZero =  Refl
plusBoth SZero (SSucc m') = Refl
plusBoth n@(SSucc n') SZero = case plusSucc' n SZero of Refl -> Refl
plusBoth n@(SSucc n') m@(SSucc m') = case plusSucc' n m of Refl -> Refl


prependToAll :: forall n a. a -> SNat n -> Vec n a -> Vec (n + n) a
prependToAll _ SZero Nil = Nil
prependToAll sep n@(SSucc n'::SNat n') (x:>xs) = case plusBoth n' n' of Refl -> sep :> x :> prependToAll sep n' xs

--example for using VecList
stuffs = (1 :> 2 :> Nil) :>> (3 :> Nil) :>> (4 :> 5 :> 6 :> Nil) :>> VLNil

--2.inits

type family (a::[Nat]) ++ (b::[Nat]) where
  ([]::[Nat]) ++ b = b
  a ++ ([]::[Nat]) = a	
  (a:as) ++ b = a : as ++ b 
--take two inputs such that the first one works as the standard, and keep subtracting the second one to note the actual number. So here we need to use the (-) type family and the base case is zero zero = []
type family ListOfNat (n ::Nat) :: [Nat] where
 ListOfNat Zero = '[]
 ListOfNat (Succ n') = ListOfNat n' ++ '[(Succ n')]

sToNat :: SNat n -> Nat
sToNat SZero = Zero
sToNat (SSucc s) = Succ (sToNat s)

inits :: forall n a. SNat n -> Vec n a -> VecList (ListOfNat n) a
inits SZero Nil = VLNil
inits n@(SSucc n') (x:>xs) = undefined