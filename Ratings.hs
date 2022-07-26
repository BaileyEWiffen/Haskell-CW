module Ratings where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

type Customer = String
type Product = String
type Score = Int

unique:: Ord a => [a] -> [a]
unique xs = Set.toList $ Set.fromList $ xs

sumMapList:: (Foldable f,Ord k, Num a) => f (Map k a) -> [(k, a)]
sumMapList xs = Map.toList $ Map.unionsWith (+) xs

-- a single rating of a product by a customer
type Rating = (Customer, Product, Score)

-- a list of all customers who submitted a rating, without duplicates
customers :: [Rating] -> [Customer]
customers rs = 
    unique [c|(c,p,s)<-rs]

-- a list of all products that have been rated, without duplicates
products :: [Rating] -> [Product]
products rs = 
    unique [p|(c,p,s)<-rs]

-- customers and the number of products they have rated
numScores :: [Rating] -> [(Customer, Int)]
numScores rs =  
    sumMapList [Map.singleton c 1|(c,p,s)<-rs]

-- a list of the customers who give the same score in all their ratings
consistent :: [Rating] -> [Customer]
consistent rs = 
    [c|(c,n)<-sumMapList [Map.singleton c 1|(c,s)<-unique [(c,s)|(c,p,s)<-rs]],n==1]

-- the average score of each product
averageScore :: [Rating] -> [(Product, Double)]
averageScore rs = 
    [(p, (fromIntegral (s) / fromIntegral (n))) | (p, (s,n)) <- Map.toList $ Map.unionsWith (addPair) [Map.singleton p (s,1) |(c,p,s)<-rs]]
    
addPair :: Num a => (a,a)->(a,a)->(a,a)
addPair (a,b) (x,y) = (a+x,b+y)

-- the products that have each been rated by every customer
popular :: [Rating] -> [Product]
popular rs = 
    [p|(p,f)<-sumMapList [Map.singleton p 1|(c,p,s)<-rs], f==totalCus]
        where
        totalCus = length $ customers rs

