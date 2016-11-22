module Tech where

import Control.Arrow
import Data.Monoid
import Data.List
import qualified Data.Map as M

techProbs :: [Int] -> Double -> [Int] -> [(Int, Double)]
techProbs _ p [] = [(0, p)]
techProbs (a:advances) prob  techs = 
        let start = step . map (subtract a) $ techs
            len = length start    
            p = prob / fromIntegral len
            tec = [step $ take n start <> ((start !! n) - 5: drop (n+1) start)| n <- [0 .. len-1]]  
        in if len == 0 then [(1, prob)] else M.toList . M.fromListWith (+) . map (first (+1)) . concatMap (techProbs advances p) $ tec  

step :: [Int] -> [Int]
step ls = let (finished, remaining) = partition (<=0) ls
          in fillUp remaining $ (-1) * sum finished

fillUp :: [Int] -> Int -> [Int] 
fillUp [] _ = []
fillUp ls v | v <= 0 = ls
            |otherwise = let (maximal, other) = partition (== maximum ls) ls                                        
                         in if length maximal >= v then map (subtract 1) (take v maximal) <> drop v maximal <> other
                                                   else step $ (length maximal -v) : map (subtract 1) maximal <> other