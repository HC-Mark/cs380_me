{-# LANGUAGE GADTs, TypeInType, StandaloneDeriving, TypeFamilies,
             TypeOperators #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Class where

import Data.Kind
import Prelude hiding ( reverse, (++), last, map,and, or, any, take ,drop,unzip,uncons,init,null,length,take,drop,replicate,foldl,foldl1,foldr,foldr1,scanl,scanl1,scanr,scanr1,mapAccumL, mapAccumR,splitAt )

data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat

instance Show Nat where
  show = show . fromNat

fromNat :: Nat -> Integer
fromNat Zero      = 0
fromNat (Succ n) = 1 + (fromNat n) -- print Nat in integer

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
or (x :> xs@(_ :> _)) = x || (or xs) -- since ghc can not identify xs is not Nil
                                     -- we must use y : ys here, to make Succ n be defined

any :: (a -> Bool) -> Vec n a -> Bool
any f Nil = False
any f (x :> xs) = (f x) || (any f xs)
-- if we use Succ n in Vec here, ghc does not know xs is non-empty

unzip :: Vec n (a,b) -> ( (Vec n a), (Vec n b) )
unzip Nil = (Nil, Nil);
unzip ( x :> xs ) = ( (fst x) :> (fst $ unzip xs), (snd x) :> (snd $ unzip xs))

test2 = (1,2) :> (9,10) :> (12,3) :> Nil

uncons :: Vec (Succ n) a -> (a, Vec n a)
uncons (x :> xs) = (x, xs)
--here we don't need a Maybe type here, since if we use Succ n, we can avoid to match the Nil situation

init::Vec (Succ n) a -> Vec n a
init (x :> Nil) = Nil
init (x :> xs@(_ :> _)) = x :> (init xs)

--patterns not matched _ (_ :> _)? but works
insert :: Ord a => a -> Vec n a -> Vec (Succ n) a
insert x Nil = x :> Nil
insert x (y :> ys)
       | x > y = y:> (insert x ys)
       | x <= y = x :> y :> ys

sort :: Ord a => Vec n a -> Vec n a
sort Nil = Nil
sort (x:>Nil) = (x:>Nil)
sort (x :> xs@(_ :> _)) = insert x (sort xs) 

--why I need to use a SBool if Bool versioin works
--null :: Vec n a -> Bool
--null Nil = True
--null (x:>_) = False

-- singleton Nat
data SNat :: Nat -> Type where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)

-- singleton Bool
data SBool :: Bool -> Type where
  SFalse :: SBool False
  STrue  :: SBool True

type family NatToBool (n :: Nat) :: Bool  where
   NatToBool Zero = False
   NatToBool (Succ _) = True


null :: Vec n a -> SBool (NatToBool n)
null Nil = SFalse
null (x :> _) = STrue

void = Nil

length :: Vec n a -> Nat
length Nil = Zero
length ( x :> xs ) = Succ ( length xs )

--need to define - here

type family a - b where
  a   - Zero = a
  (Succ a) - (Succ b)  = a - b
infixl 6 -


stripPrefix :: Eq a => Vec n a -> Vec m a -> Maybe ( Vec (m-n) a )
stripPrefix Nil x = Just x
stripPrefix (_ :> _) Nil = error "can not drop element from an empty vector. "
stripPrefix (x :> xs) (y :> ys)
        | x == y = stripPrefix xs ys --how can I correspond the expression with my type signature
        | x /= y = Nothing



take :: SNat n -> Vec m a -> Vec n a
take SZero _ = Nil
-- take _   Nil = Nil wrong , since we don't specify n
take (SSucc n') Nil = error "can not extract element from an empty vector. " 
take (SSucc n') (x :> xs) = x:> (take n' xs)

drop :: SNat n -> Vec m a -> Vec (m - n) a
drop SZero x = x
drop (SSucc n') Nil = error "can not drop element from an empty vector. "
drop (SSucc n') (x :> xs) = drop n' xs

replicate :: SNat n -> a -> Vec n a
replicate SZero _ = Nil
replicate (SSucc n') x = x :> replicate n' x

foldl :: (b -> a -> b) -> b -> Vec n a -> b
foldl  _ x Nil = x
foldl f x ( y :> ys) = foldl f (f x y) ys

foldl1 :: ( a -> a -> a) -> Vec (Succ n) a -> a
foldl1 f ( x :> Nil) = x
foldl1 f (x :> xs@(_ :> _)) = foldl f x xs

test3 = 3 :> Nil

foldr :: (a -> b -> b) -> b -> Vec n a -> b
foldr _ x Nil = x
foldr f x ( y :> ys ) = (f y (foldr f x ys))

foldr1 :: (a -> a -> a) -> Vec (Succ n) a -> a
foldr1 f ( x :> Nil) = x
foldr1 f ( x :> xs@( _ :> _)) = foldr f x xs

--inspired by standard-prelude.html ( how they implement scanl in list)
scanl :: (b -> a -> b) -> b -> Vec n a -> Vec (Succ n) b
scanl f z xs  = z :> (case xs of
                         Nil -> Nil
                         (x :> xs) -> (scanl f (f z x) xs))

scanl1 :: (a -> a -> a) -> Vec n a -> Vec n a
scanl1 _ Nil = Nil
scanl1 f ( x :> xs ) = scanl f x xs

{-
scanr :: (a -> b -> b) -> b -> Vec n a -> Vec (Succ n) b
scanr f z xs
     | xs == Nil = z :> Nil
     | otherwise  = 
     where
       getLast :: Vec (Succ n) a -> a
       getLast (x :> Nil) = x
       getLast (x :> xs@(_ :> _)) = getLast xs

       removeLast :: Vec (Succ n) a -> Vec n a
       removeLast (x :> Nil) = Nil
       removeLast (x :> xs@(_ :> _)) = removeLast xs

       getHead :: Vec (Succ n) a -> a
       getHead (x :> Nil) = x
       getHead (x :> xs@(_ :> _)) = x
-}

scanr :: (a -> b -> b) -> b ->  Vec n a -> Vec (Succ n) b
scanr f z Nil = z :> Nil
scanr f z (x :> xs)  = (f x (getHead $ scanr f z xs)) :> (scanr f z xs)
      where
        getHead :: Vec (Succ n) a -> a
        getHead (x :> Nil) = x
        getHead (x :> xs@(_ :> _)) = x
{-
scanr1 :: (a -> a -> a) -> Vec (Succ n) a -> Vec (Succ n) a
scanr1 f ( x :> Nil) = x :> Nil
scanr1 f ( x :>xs@(_ :> _)) = scanr f (getLast (x :> xs)) (removeLast (x :> xs))
     where
       getLast :: Vec (Succ n) a -> a
       getLast (x :> Nil) = x
       getLast (x :> xs@(_ :> _)) = getLast xs

       removeLast :: Vec (Succ n) a -> Vec n a
       removeLast (x :> Nil) = Nil
       removeLast (x :> xs@(_ :> _)) = removeLast xs
-}

scanr1 :: (a -> a -> a) -> Vec n a -> Vec n a
scanr1 _ Nil = Nil
scanr1 _ (x :> Nil) = x :> Nil
scanr1 f (x :> xs@(_ :> _))  = (f x (getHead $ scanr1 f xs)) :> (scanr1 f xs)
      where
        getHead :: Vec (Succ n) a -> a
        getHead (x :> Nil) = x
        getHead (x :> xs@(_ :> _)) = x

mapAccumL :: (acc -> x -> (acc,y)) -> acc -> Vec n x -> (acc, (Vec n y))
mapAccumL _ acc Nil = (acc, Nil)
mapAccumL f acc xs = let result = storeInVec f acc xs in
                     (getAcc result , getSnd result)
      where
        storeInVec :: (acc -> x -> (acc,y)) -> acc -> Vec n x -> Vec n (acc,y)
        storeInVec _ acc Nil = Nil
        storeInVec f acc (x :> xs) = (f acc x) :> (storeInVec f (fst (f acc x)) xs)

        getSnd :: Vec n (a,b) -> Vec n b
        getSnd Nil = Nil
        getSnd (x :> xs) = (snd x) :> (getSnd xs)

        getAcc :: Vec n (a,b) -> a
        getAcc Nil = error "it is wrong "
        getAcc (x :> xs) = case xs of
                            Nil -> fst x
                            y :> ys -> getAcc ys
           --   | xs == Nil = fst x
           --   | otherwise = getAcc xs

mapAccumR :: (acc -> x -> (acc, y)) -> acc -> Vec n x -> (acc, Vec n y)
mapAccumR _ acc Nil = (acc, Nil)
mapAccumR f acc (x:>xs) = (acc'', y:>ys)
                         where (acc'', y) = f acc' x
                               (acc', ys) = mapAccumR f acc xs

--easy way to do
splitAt :: SNat n -> Vec m a -> ((Vec n a), (Vec (m-n) a))
splitAt n xs = (take n xs, drop n xs)

splitAt' :: SNat n -> Vec m a -> ((Vec n a), (Vec (m-n) a))
splitAt' SZero xs = (Nil, xs)
--splitAt' _ Nil =(Nil,Nil) the first Nil can not be deduced. how to deal with
splitAt' (SSucc _) Nil = error "can not deduce from an empty Vec"
splitAt' (SSucc SZero) (x:>xs) = (x:>Nil, xs)
splitAt' (SSucc m) (x:>xs) = (x:>xs', xs'') -- how this thing works
        where
           (xs', xs'')= splitAt' m xs
