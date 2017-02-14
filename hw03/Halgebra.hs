{- CS380 Assignment 3
   Name:Tianming Xu
   College email:txu1@haverford.edu
   Resources / collaborators:

   **DUE ON GRADESCOPE BEFORE CLASS ON WEDNESDAY, FEBRUARY 15, 2017.**
-}


{-# LANGUAGE GADTSyntax, StandaloneDeriving #-}

module Halgebra where

import Arith
import Parser
import Data.Ratio

--------------------------------------------------------------------------------
-- The Halgebra computer algebra system
--

{-
   In this assignment, you will write functions that can be used to solve
   linear equations in one variable, x. The final result will be the `solve`
   function, at the end of this file. `solve`'s type is `Equation -> Rational`;
   it takes an `Equation` in the variable x and returns the `Rational` that
   is a solution to the equation. (`Rational`, exported in the `Prelude` but
   originally from `Data.Ratio`, is a numerical type that can store rational
   numbers of arbitrary precision. By "numerical type", I mean that `Rational`
   in an instance of the `Num` class, meaning that `3 :: Rational` is accepted.)

   This assignment is less prescribed than previous ones, with a few function
   signatures given to you, and the rest left for you to figure out.

   Here is the general approach you will take:

   1. Set one side of the input equation to 0. That is, create a `Sum` that
      evaluates to 0 whenever the original equation holds. (This step
      is really simple!)

   2. Simplify your `Sum` into a list of `SimpleTerm`s. Each `SimpleTerm`
      is a `Rational` coefficient perhaps multiplied by x. This step is done
      by writing the three functions `simpleSum`, `simpleTerm`, and
      `simpleFactor`, which will be mutually recursive. (That is, they
      will call one another.) You will likely need to write several helper
      functions.

   3. Separate out the list of `SimpleTerm`s into those that mention x and
      those that don't.

   4. Add together the coefficients of x and the `SimpleTerm`s that do not
      mention x. Call the former `x_coef` and the latter `constant`.

   5. The solution to the equation is `(-constant)/x_coef`.

   Here is an example:

   Start:  1 + 2*x = 3 * (x - 5*(x + 1))
   After step 1: (1 + 2*x) - (3 * (x - 5*(x + 1)))
   After step 2: [1, 2x, -3x, 15x, 15]
   After step 3: ([2x, -3x, 15x], [1, 15])
   After step 4: (14, 16)
   After step 5: -8/7

   This homework assignment requires the Arith.hs and Parser.hs files as
   posted on our syllabus page. It also requires the `parsec` package. You
   might need to

      cabal install parsec

   to install this on your system.

   Hints:
     * The `fromInteger :: Num a => Integer -> a` function can convert from
       `Integer` to any other numerical type (like `Rational`).

     * By default, `Rational`s print out somewhat oddly in GHCi, using a `%`
       to denote division. I've given you `prettyRational` that does a better
       job.

     * There are three ways solving can fail:
       1) You can have a non-linear equation, where you try to multiply x by x.
       2) You can have a non-linear equation, where you try to divide by x.
       3) All the x's can cancel out.
       In any of these scenarios, just call `error :: String -> a` with an
       appropriate error message. Do *not* try to detect (and accept) an equation
       with x/x in it, such that the division by x is OK. Any division by
       an expression with x in it is rejected.

       (This approach toward failure is regrettable. Really, we should return
        a `Maybe Rational`. But that will complicate things too much at this
        stage. We shall return!)

     * Simplifying (a + b + c) * (d + e + f) means you multiply everything
       in the left sum by everything in the right sum, producing *nine* output
       terms. A list comprehension might be useful.

     * Simplifying (a + b + c) / (d + e + f) is harder. Because we reject any
       denominator mentioning x, we can just add the d, e, and f (which must be
       x-less `SimpleTerm`s), and then divide each of a, b, and c by the sum.

     * Write unit tests! You will thank yourself later.
-}

-- a prettier rendering of Rationals
prettyRational :: Rational -> String
prettyRational r
  | denominator r == 1
  = show (numerator r)

  | otherwise
  = "(" ++ show (numerator r) ++ "/" ++ show (denominator r) ++ ")"

-- a SimpleTerm is a coefficient and, perhaps, an x
data SimpleTerm where
  SimpleTerm :: Rational       -- the coefficient
             -> Bool           -- True <=> there is an x; False <=> there isn't
             -> SimpleTerm

-- You may wish to uncomment one of these instances for debugging / unit testing:


-- This Show instance is pretty
instance Show SimpleTerm where
  show (SimpleTerm coef has_x) = show_coef ++ maybe_show_x
    where
      show_coef = prettyRational coef

      maybe_show_x | has_x     = "x"
                   | otherwise = ""


{-
-- This Show instance is ugly
deriving instance Show SimpleTerm
-}

-- Step 1
gatherOnOneSide :: Equation -> Sum
gatherOnOneSide (Equation lhs rhs)= Minus lhs rhs

-- Simplify a Sum to a list of SimpleTerms (Step 2)
simpleSum :: Sum -> [SimpleTerm]
simpleSum (Term val) = simpleTerm val
simpleSum (Plus val1 val2) = simpleSum val1 ++ simpleSum val2
simpleSum (Minus val1 val2) = simpleSum val1 ++ map (reverse) (simpleSum val2) --simpleSum (Term(Mult (Factor (Lit (-1))) (Factor (Sum val2))))
         where
           reverse :: SimpleTerm -> SimpleTerm
           reverse (SimpleTerm coef has_x) = SimpleTerm (coef * (-1)) has_x 
-- Simplify a Term to a list of SimpleTerms (Step 2)
simpleTerm :: Term -> [SimpleTerm]
simpleTerm (Factor val) = simpleFact val
simpleTerm (Mult val1 val2)
      | (val1 == Factor Var) && (val2 == Factor Var) = error "I can not solve non-linear equation"
      | (val1 == Factor Var) = map (have_x) (simpleTerm val2)
      | (val2 == Factor Var) = map (have_x) (simpleTerm val1)
      | otherwise = [x `multiply` y | x <- (simpleTerm val1), y <- (simpleTerm val2)] 
      -- | otherwise = zipWith (multiply) (simpleTerm val1) (simpleTerm val2) -- ** 

      where
      multiply :: SimpleTerm -> SimpleTerm -> SimpleTerm
      multiply (SimpleTerm coef1 has_x1) (SimpleTerm coef2 has_x2) = (SimpleTerm (coef1 * coef2) (has_x1 || has_x2))

      have_x :: SimpleTerm -> SimpleTerm
      have_x (SimpleTerm coef has_x) = (SimpleTerm coef True)

simpleTerm (Div val1 val2) -- not perfect
      | val2 == Factor Var = error "I can not solve non-linear equation"
      | val1 == Factor Var = map (have_x) (reciprocal (simpleTerm val2))
      | otherwise = [x `division` y | x <- (simpleTerm val1), y <- (add_all (head(simpleTerm val2)) (tail(simpleTerm val2)))]

     where
        have_x :: SimpleTerm -> SimpleTerm
        have_x (SimpleTerm coef has_x) = (SimpleTerm coef True)

        add_all :: SimpleTerm -> [SimpleTerm] -> [SimpleTerm]
        add_all (SimpleTerm coef1 has_x1) ((SimpleTerm coef2 has_x2):xs)  = add_all (SimpleTerm (coef1 + coef2) False) xs
        add_all (SimpleTerm coef1 has_x1) [] = (SimpleTerm coef1 False):[]

        reciprocal :: [SimpleTerm] -> [SimpleTerm]
        reciprocal ((SimpleTerm coef has_x):xs) =  (SimpleTerm (1 / coef) has_x):[] ++ reciprocal xs
        reciprocal [] = []

        division :: SimpleTerm -> SimpleTerm -> SimpleTerm
        division (SimpleTerm coef1 has_x1) (SimpleTerm coef2 has_x2) = (SimpleTerm (coef1 / coef2) (has_x1 || has_x2))

-- Simplify a Factor to a list of SimpleTerms (Step 2)
simpleFact :: Factor -> [SimpleTerm]
simpleFact (Lit num) = SimpleTerm (fromInteger num) False : []
simpleFact Var = SimpleTerm (fromInteger 1) True:[]
simpleFact (Sum val) = simpleSum val

-- Step 3
partitionTerms :: [SimpleTerm]
               -> ( [SimpleTerm]   -- these mention x
                  , [SimpleTerm] ) -- these don't
partitionTerms list = (variable list, constant list)

      where
        variable :: [SimpleTerm] -> [SimpleTerm]
        variable [] = []
        variable ((SimpleTerm coef has_x):xs)
             | has_x == True = (SimpleTerm coef has_x):[] ++ variable xs
             | otherwise = variable xs
        constant :: [SimpleTerm] -> [SimpleTerm]
        constant [] = []
        constant ((SimpleTerm coef has_x):xs)
             | has_x == False = (SimpleTerm coef has_x):[] ++ constant xs
             | otherwise = constant xs
        

-- Step 4
sumPartitions :: ( [SimpleTerm]    -- these mention x
                 , [SimpleTerm] )  -- these don't
              -> ( Rational        -- sum of coefficients of x
                 , Rational )      -- sum of constants
sumPartitions (variable, constant) = ((add_all variable), (add_all constant))

    where
       add_all :: [SimpleTerm] -> Rational
       add_all [] = 0;
       add_all ((SimpleTerm coef has_x):xs) = coef + (add_all xs)

-- Step 5
extractSolution :: ( Rational     -- coefficient of x, "a"
                   , Rational )   -- constant, "b"
                -> Rational       -- solution to "a*x + b = 0"
extractSolution (coef_v, constant) = constant / coef_v * (-1)

-- Put them all together
solve :: Equation -> Rational
solve = extractSolution .
        sumPartitions .
        partitionTerms .
        simpleSum .
        gatherOnOneSide


--testing
a = parseEquation "1 + 2*x = 3 * (x - 5*(x + 1))"
b = gatherOnOneSide a
c = simpleSum b
d = partitionTerms c
e = sumPartitions d
f = extractSolution e
