
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ViewPatterns #-}

module Stats 
    (StatFn
    ,StatWSFn
    ,statFnMap
    ,docText
    ,defaultStat
    ) where

import qualified Data.Map.Strict as Map (Map)
import Data.List (sortBy,minimumBy,maximumBy,genericLength)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)

-- Normal statistics function on pairs (precision,value)
type StatFn = [(Int,Double)] -> Maybe (Int,Double)
-- Function for whitespace on integers (# of spaces)
type StatWSFn = [Int] -> Int

safeMin,safeMax :: StatFn
safeMin [] = Nothing
safeMin xs = Just $ minimumBy (comparing snd) xs
safeMax [] = Nothing
safeMax xs = Just $ maximumBy (comparing snd) xs

minWS,maxWS :: [Int] -> Int
minWS [] = 0
minWS xs = maximum xs -- Most whitespace to pad the least digits
maxWS [] = 0
maxWS xs = minimum xs -- Least whitespace to pad the most digits

medianListBy :: (a -> a -> Ordering) -> (a -> a -> a) -> [a] -> Maybe a
medianListBy cmp evenFn (sortBy cmp -> xs) =
    case length xs of
        0                               -> Nothing
        len | odd len,   h <- len`div`2 -> Just $ xs !! h
        len | otherwise, h <- len`div`2 -> Just $ evenFn (xs !! (h-1)) (xs !! h)

median :: StatFn
median = medianListBy (comparing snd) (\(a,x) (b,y) -> ((a + b + 1)`div`2, (x + y)/2))

medianWS :: StatWSFn
medianWS = fromMaybe 0 . medianListBy compare (\a b -> (a + b)`div`2)

mean :: StatFn
mean []                 = Nothing
mean (unzip -> (ns,vs)) = Just (round $ sum (fromIntegral <$> ns) / genericLength ns, sum vs / genericLength vs)

meanWS :: StatWSFn
meanWS [] = 0
meanWS xs = round $ sum (fromIntegral <$> xs) / genericLength xs

statFnMap :: Map.Map String (StatFn,StatWSFn)
statFnMap =
    [("min",(safeMin,minWS)),("max",(safeMax,maxWS))
    ,("median",(median,medianWS))
    ,("mean",(mean,meanWS)),("avg",(mean,meanWS))]

docText :: String -> String
docText progName = unlines
    ["Usage: "++progName++" [period=1] [method...]"
    ,"    where period is a positive integer and method is one or more of the following:"
    ,"      min     the minimum value"
    ,"      max     the maximum value"
    ,"      median  the median value"
    ,"      mean    the (arithmetic) mean"
    ,"      avg     alias for mean"
    ,"If no method is supplied, it defaults to mean."]

defaultStat :: [(StatFn,StatWSFn)]
defaultStat = [(mean,meanWS)]

