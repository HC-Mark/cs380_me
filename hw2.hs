{- CS380 Assignment 2
   Name:Tianming Xu
   College email:txu1@haverford.edu
   Resources / collaborators:

   **DUE ON GRADESCOPE BEFORE CLASS ON WEDNESDAY, FEBRUARY 8, 2017.**
-}

{-# LANGUAGE GADTSyntax #-}

module Hw02 where

import Test.HUnit
import Data.Char

--------------------------------------------------------------------------------
-- Binary Search Trees

{- In this assignment, you will be writing a binary search tree implementation
   of a finite map. A finite map is a data structure that associates *keys*
   (of one type) with *values* (of, potentially, another type). This is useful for,
   say, storing an address book or counting word frequencies in a file. Java's
   finite map interface is Map (https://docs.oracle.com/javase/8/docs/api/java/util/Map.html);
   Python calls this type a dictionary.

   Note that this is a *different*, unrelated use of the word "map" than the
   higher-order function that we've seen.

   To keep things simple, our map will use Strings as its *key* type.

   The map will be implemented using a binary search tree.
   https://en.wikipedia.org/wiki/Binary_search_tree

   The tree type will store a String and a value (of type `a`) at each interior
   node. It will obey the usual binary search tree properties. Given a interior
   node T with left child L and right child R:

   1. The key stored in every node in the tree rooted at L is less than T's key.
   2. The key stored in every node in the tree rooted at R is greater than T's key.

   By "less" and "greater" here, I mean by using the operators (<) and (>), which
   work on Strings.

   **** UNIT TESTS ****
   Each function must be tested against at least 4 test cases. Add to mine!
-}

-- The tree datatype:
data Tree a where
  Leaf :: Tree a
  Node :: String  -- key
       -> a       -- value
       -> Tree a  -- left child
       -> Tree a  -- right child
       -> Tree a
    deriving (Eq, Show)   -- this allows (==) on trees and (show :: Tree a -> String)

sampleTree1 :: Tree Int
sampleTree1 = Node "pickle" 5
       (Node "avocado" 2
     Leaf           (Node "clementine" 10
                      Leaf    Leaf))           (Node "tomato" 7
                                        (Node "radish" 9
                                          Leaf   Leaf)         (Node "yam" 1
                                                                 Leaf   Leaf))

sampleTree2 :: Tree Char
sampleTree2 = Node "Haskell" 'x'
          (Node "C" 'q'
   (Node "Ada" 'p'
   Leaf   Leaf)    (Node "C++" 'r'
                     Leaf (Node "F#" 'e'
                            Leaf   Leaf)))    (Node "OCaml" 'd'
                                                Leaf     Leaf)
--------------------------------------------------------------------------------
-- Problem 1

{- Write a function that gets the size (number of interior nodes) of a tree. -}

sizeTree :: Tree a -> Int
sizeTree Leaf = 0
sizeTree (Node key value left right) = 1 + sizeTree left + sizeTree right

sizeTree_tests = "sizeTree" ~:
                 TestList [ "sampleTree1" ~: sizeTree sampleTree1 ~?= 6
                          , "sampleTree2" ~: sizeTree sampleTree2 ~?= 6 ]

--------------------------------------------------------------------------------
-- Problem 2

{- Write a function that finds a key in a binary search tree and
   returns the associated value, if there is one. -}

findTree :: Tree a -> String -> Maybe a
findTree Leaf _ = Nothing
findTree (Node key value left right) target
     | target == key = Just value
     | target < key = findTree left target
     | target > key = findTree right target
    
  
findTree_tests = "findTree" ~:
                 TestList [ "pickle" ~: findTree sampleTree1 "pickle" ~?= Just 5
                          , "Java"   ~: findTree sampleTree2 "Java"   ~?= Nothing
                          , "Haskell" ~: findTree sampleTree2 "Haskell" ~?= Just 'x'
                          , "creamcheese" ~: findTree sampleTree1 "creamchesse" ~?= Nothing]

--------------------------------------------------------------------------------
-- Problem 3

{- Write a function that inserts a key into a binary search tree. If the key
   is already in the tree, then update the value associated with the key. -}

insertTree :: Tree a -> String -> a -> Tree a
insertTree Leaf input_key input_value = (Node input_key input_value Leaf Leaf)
insertTree (Node key value left right) input_key input_value
       | input_key == key = (Node key input_value left right)
       | input_key < key = Node key value (insertTree left input_key input_value) right 
       | input_key > key = Node key value left (insertTree right input_key input_value)

insertTree_tests
  = "insertTree" ~:
    TestList [ "insert/find" ~: findTree (insertTree Leaf "hi" "there") "hi" ~?= Just "there"
             , "update"      ~: findTree (insertTree sampleTree1 "clementine" (-5)) "clementine"
                                  ~?= Just (-5)
             , "update" ~: findTree (insertTree sampleTree2 "C++" 'm') "C++" ~?= Just 'm'
             , "insert in a tree" ~: findTree (insertTree sampleTree1 "Pineapple" 380) "Pineapple"
                                      ~?= Just 380]

--------------------------------------------------------------------------------
-- Problem 4

{- Write a function that maps a function over all the *values* in your tree. -}

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree op Leaf = Leaf
mapTree op (Node key value left right)
                          = (Node key (op value) (mapTree op left) (mapTree op right))

mapTree_tests
  = "mapTree" ~:
    TestList [ "isVowel" ~: findTree (mapTree (`elem` "aeiou") sampleTree2) "F#" ~?= Just True
             , "samesize" ~: sizeTree (mapTree (+1) sampleTree1) ~?= 6
             , ">5?" ~: findTree (mapTree (<=5) sampleTree1) "radish" ~?= Just False
             , "allUpper" ~: findTree (mapTree (toUpper) sampleTree2) "Haskell" ~?= Just 'X']

--------------------------------------------------------------------------------
-- Problem 5

{- Write a function that returns all the key/value pairs of a tree in preorder.
   That is, return the key/value pair of a node before recurring into its children. -}

preorder :: Tree a -> [(String, a)]
preorder Leaf = []
preorder (Node key value left right) = (key,value): (preorder left) ++ (preorder right)

preorder_tests
  = "preorder" ~:
    TestList [ "sampleTree1" ~: preorder sampleTree1 ~?= [ ("pickle", 5), ("avocado",2)
                                                         , ("clementine", 10), ("tomato", 7)
                                                         , ("radish", 9), ("yam", 1) ]
             , "empty" ~: preorder (Leaf :: Tree Integer) ~?= []
             , "sampleTree2" ~: preorder sampleTree2 ~?= [("Haskell", 'x'), ("C", 'q'),
                                                          ("Ada", 'p'), ("C++", 'r'),
                                                          ("F#", 'e'), ("OCaml", 'd')]]
                                    -- HUnit struggles if it doesn't know the type
                                    -- of data stored in a polymophic structure

--------------------------------------------------------------------------------
-- Problem 6

{- Write a function that uses your tree structure to efficiently compute the
   frequencies of words in a list of words. The input to your function is a list
   of words (Strings). The output is an association list associating each word
   with the number of times it occurs in the list. -}

frequencies :: [String] -> [(String,Int)]
frequencies list  = preorder ( store list Leaf )
        where
           --store the list of strings to a tree 
             store :: [String] -> Tree Int -> Tree Int
             store [] result = result 
             store (head:rest) Leaf = store rest (insertTree Leaf head 1) 
             store (head:rest) (Node key value left right)
                    |findTree (Node key value left right) head == Nothing   = store rest (insertTree (Node key value left right) head 1)
                    |key == head = store rest (insertTree (Node key value left right) head (value+1))
                    |key < head  = Node key value (store rest left) right
                    |key > head = Node key value left (store rest right)
                                         

frequencies_test
  = "frequencies" ~:
    TestList [ "palindrome a   " ~: lookup "a"    (frequencies words) ~?= Just 3
             , "palindrome plan" ~: lookup "plan" (frequencies words) ~?= Just 1
             , "canal" ~: lookup "canal" (frequencies words) ~?= Just 1
             , "Dota2" ~: lookup "Dota2" (frequencies words) ~?= Nothing ]

  where
    words = ["a", "man", "a", "plan", "a", "canal", "Panama"]

all_tests = TestList [ sizeTree_tests
                     , findTree_tests
                     , insertTree_tests
                     , mapTree_tests
                     , preorder_tests
                     , frequencies_test]

