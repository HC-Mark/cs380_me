{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass, GADTSyntax #-}
{-
   Name : Tianming Xu
   email : txu1@haverford.edu
   collaborators : Eileen, Kevin, Trista

-}
module Hw04 where
import Data.Aeson
import Data.Monoid
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
--import qualified Data.HashMap.Strict as HashMap


--Q1
ynToBool :: Value -> Value
ynToBool (Object value) = Object(fmap (toBool) value) 
     where
       toBool :: Value -> Value
       toBool a
               | a == (String "Y") = Bool True
               | a == (String "N") = Bool False
               | otherwise = a
ynToBool (Array value) = Array(fmap (change) value)
     where
       change :: Value -> Value
       change (Object obj) = Object(fmap (toBool) obj)
        where
          toBool :: Value -> Value
          toBool a
               | a == (String "Y") = Bool True
               | a == (String "N") = Bool False
               | otherwise = a
--Q2
parseData :: B.ByteString -> Maybe Value
parseData file 
     | (fmap ynToBool (decode file)) == Nothing = error "invalid input file"
     | otherwise = fmap ynToBool (decode file)


--Q3
data Market where
     Market :: {marketname :: T.Text
               ,x :: Double
               ,y :: Double
               ,state :: T.Text
               ,cheese :: Bool } -> Market -- cheese should be Bool we will change after we get Q4
     deriving (Show, Generic,FromJSON)


market::Maybe [Market]
market = decode"[{\"fmid\":1002206, \"marketname\":\"Zona Roasa Farmers' Market\", \"x\":-94.7071, \"y\":39.288, \"state\":\"Virginia\", \"cheese\":\"N\", \"updatetime\":2009},{\"fmid\":1002206, \"marketname\":\"Zona Roasa Farmers' Market\", \"x\":-94.7071, \"y\":39.288, \"state\":\"Virginia\", \"cheese\":\"N\", \"updatetime\":2009}]"


{-
data Person where
     Person :: {name :: String
                ,age :: Int
                ,game :: String} -> Person
     deriving(Show, Generic, FromJSON)
p:: Maybe Person
p = decode"{ \"name\" : \"Richard\", \"age\" : 35,\"game\":\"Dota\" }"
-}

--Q4

parseMarkets :: B.ByteString -> Maybe [Market]
parseMarkets file
       | ((pure test) <*> fmap (fromJSON :: Value -> Result [Market] ) (parseData file)) == Just True = (pure getResult) <*> fmap (fromJSON :: Value -> Result [Market]) (parseData file)
       | otherwise = error "invalid input file"
       
       where
          test :: Result a -> Bool
          test (Success a) = True
          test _ = False
          
          getResult :: Result a -> a
          getResult (Success a) = a
          getResult _ = error "fail in parse"

{- 
parseMarkets :: B.ByteString -> Maybe [Market]
parseMarkets file = decode file
-}

--Q5
loadData :: IO [Market]
loadData = B.readFile "markets.json" >>= return . getData . parseMarkets
          where
            getData :: Maybe [Market] -> [Market]
            getData Nothing = []
            getData (Just a) = a
{-
loadData
     = if ((B.readFile "markets.json" >>= parseMarkets) == Nothing) then fail "Invalid parsing"
       else (B.readFile "markets.json" >>= return . getData . parseMarkets)
          where
            getData :: Maybe [Market] -> [Market]
            getData Nothing = []
            getData (Just a) = a
        why it does not work?
-}

--Q6
data OrdList a where
   OrdList :: {getordList :: [a]} -> OrdList a
   deriving (Eq, Show)

instance Ord a => Monoid (OrdList a) where
      mempty = OrdList []
      OrdList a `mappend` OrdList b = OrdList(sort a b)
       where
         sort :: Ord a => [a] -> [a] -> [a] 
         sort [] b = b
         sort a [] = a
         sort a b | (head a) < (head b) || (head a) == (head b) =  [head a] ++ (sort (tail a) b) 
                  | (head a) > (head b) = [head b] ++ (sort a (tail b)) 
         sort _ _ = []
{-
test1 :: OrdList Integer
test1 = OrdList [1,8,3]
test2 :: OrdList Integer
test2 = OrdList [4,5]

combined :: OrdList Integer
combined = test1 <> test2
-}

type Searcher m = T.Text -> [Market] -> m

--Q7
search :: Monoid m => (Market -> m) -> Searcher m
search f name list = let easy = map (marketWithName) list in
              combined f $ extract $ find_mkt name easy

        where
          marketWithName :: Market -> (T.Text, Market)
          marketWithName mkt@(Market {marketname = name}) = (name,mkt)

          find_mkt :: T.Text -> [(T.Text, Market)] -> [(T.Text, Market)]
          find_mkt name (x:xs)
                | (name == fst x) == True = (find_mkt name xs) ++ x:[]
                | otherwise = (find_mkt name xs)

          extract :: [(T.Text, Market)] -> [Market]
          extract (x:xs) = (extract xs) ++ [(snd x)]

-- when write helper function ,should not assume the type is the same as main function
-- here, ghc does not know what m is , so we need to constrain m to Monoid type to let it
-- use mappend and mempty
          combined :: Monoid m => (Market -> m) -> [Market] -> m
          combined f (x:xs) = (combined f xs) <> (f x)
          combined f [] = mempty

--Q8
firstFound :: Searcher (Maybe Market)
firstFound name list = whether $ search store name list

               where                    
                  store :: Market -> OrdList Market
                  store mkt = OrdList [mkt]

                  whether :: OrdList Market -> Maybe Market
                  whether list
                            | (getordList list) == [] = Nothing
                            | otherwise = Just (head (getordList list))
