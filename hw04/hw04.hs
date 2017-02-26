{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass, GADTSyntax #-}
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
             ,cheese :: Bool} -> Market
   deriving (Show, Generic)
instance FromJSON Market


