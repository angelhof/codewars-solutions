module FunctionEvaluator where

import qualified Data.Map.Strict as M
import Debug.Trace
import Data.List

evaluateFunction :: (Show a, Show b, Ord a) => (a -> Either b ([a], [b] -> b)) -> a -> b
evaluateFunction f n = fst $ evaluate M.empty f n
  
evaluate :: (Show a, Show b, Ord a) => M.Map a b -> (a -> Either b ([a], [b] -> b)) -> a -> (b, M.Map a b)  
evaluate hash f n =
  case M.lookup n hash of
   Just result -> (result, hash)
   Nothing ->
     case f n of
      Left res -> (res, M.insert n res hash)
      Right (newArgs, reduce) -> (finalResult, M.insert n finalResult newHash)
        where
          reduceFun (acc, hash) arg = ((res:acc), M.union hash hash'')
            where
              (res, hash') = evaluate hash f arg
              hash'' = hash'
          (results, newHash) = foldl reduceFun ([], hash) newArgs
          finalResult = reduce results
        
factorial i | i == 0    = Left 1
            | otherwise = Right ([i-1], (*i).head)

fibonacci i | i < 2     = Left i
            | otherwise = Right ([i-1, i-2], sum)
       
coinchange (a, i) | a == 0          = Left 1
                  | a < 0 || i == 0 = Left 0
                  | otherwise       = Right ([(a, i-1), (a-coinlist!!(i-1), i)], sum)
coinlist = [1, 3, 5, 10]

heigth (n, m) | m <= 0 || n <= 0 = Left 0
              | otherwise        = Right ([(n, m-1), (n-1, m-1)], (+1).sum)

foo  i | i <= 2    = Left 1
       | odd i     = Right ([6*i`div`7, 2*i`div`3], sum)
       | otherwise = Right ([i-1, i-3], sum)
