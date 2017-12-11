
module Text 
    (Token(..)
    ,Line
    ,tokenize
    ,niceFormat
    ,parseNum
    ) where

import Data.Char (isDigit)
import Text.Printf (printf)
import Control.Arrow (first)

data Token = Number Int Double
            | Character Char
            | Whitespace Int
            deriving (Show,Eq,Ord)

type Line = [Token]

-- Returns (number of digits after decimal, result, the rest of the string)
parseNum :: String -> Maybe (Int,Double,String)
parseNum [] = Nothing
parseNum (x:xs) | isDigit x = Just $ parseNumH (fromDigit x,xs)
               | otherwise = Nothing
  where fromDigit :: Num a => Char -> a -- Assumes it's a valid digit
        fromDigit c = fromIntegral $ fromEnum c - 48
        parseDecimal :: String -> (Int,Double,String) -- Handles after the decimal point
        parseDecimal (c:cs) | isDigit c = let (n,v,r) = parseDecimal cs in (succ n, (fromDigit c + v)/10, r)
                            | otherwise = (0,0,c:cs)
        parseDecimal [] = (0,0,[])
        parseNumH :: (Double,String) -> (Int,Double,String)
        parseNumH (v,'.':cs) = let (n,v',r) = parseDecimal cs in (n,v+v',r)
        parseNumH (v,c:cs) | isDigit c = parseNumH (v*10 + fromDigit c, cs)
                           | otherwise = (0,v,c:cs)
        parseNumH (_,[]) = (0,0,[])

tokenize :: String -> [Line]
tokenize = uncurry (:) . go
  where go :: String -> (Line,[Line])
        go [] = ([],[])
        go ('\n':cs) = ([],uncurry (:) $ go cs)
        go (' ':cs) = let f :: String -> (Int,String)
                          f (' ':l) = first succ $ f l
                          f l = (1,l)
                          (n,r) = f cs
                      in first (Whitespace n:) $ go r
        go (c:cs) = case parseNum $ c:cs of
                        Just (n,v,r) -> first (Number n v:) $ go r
                        Nothing -> first (Character c:) $ go cs

niceFormat :: Int -> Double -> String
niceFormat n = printf $ "%."++show n++"f"
