module Tech where

import Control.Arrow
import Data.Monoid
import Data.List
import qualified Data.Map as M

techProbs :: [Int] -> Double -> [Int] -> [(Int, Double)]
techProbs = techProbsBoosts . (flip zip) (repeat False)

boostProbs :: [Int] -> Double -> [Int] -> [(Int, Double)]
boostProbs = techProbsBoosts . (flip zip) (repeat True)
        
techProbsBoosts :: [(Int, Bool)] -> Double -> [Int] -> [(Int, Double)]
techProbsBoosts _ p [] = [(0, p)]
techProbsBoosts ((a, boost):advances) prob  techs 
        | start == [] = [(1, prob)]
        | otherwise = M.toList . M.fromListWith (+) . map (first (+1)) . concatMap (uncurry $ techProbsBoosts advances) $ tec                               
    where start = step . map (subtract a) $ techs
          tec   = if boost then concatMap (uncurry picks) $ picks prob start else picks prob start

        
        
step :: [Int] -> [Int]
step ls = let (finished, remaining) = partition (<=0) ls
          in fillUp remaining $ (-1) * sum finished

picks :: Double -> [Int] -> [(Double, [Int])]
picks p [] = [(p, [])]        
picks p techs = let len = length techs
                    p' = p / fromIntegral len
                in [(p', step $ subAt n techs 5)| n <- [0 .. len-1]]

  where subAt :: Int -> [Int] -> Int -> [Int]
        subAt 0 (i:is) s = i - s : is
        subAt n (i:is) s | n > 0 = i : subAt (n-1) is s
        subAt _ _ _ = error "subtracting at invalid index"

                
fillUp :: [Int] -> Int -> [Int] 
fillUp [] _ = []
fillUp ls v | v <= 0 = ls
            |otherwise = let (maximal, other) = partition (== maximum ls) ls                                        
                         in if length maximal >= v then map (subtract 1) (take v maximal) <> drop v maximal <> other
                                                   else step $ (length maximal -v) : map (subtract 1) maximal <> other