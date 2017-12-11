
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Arrow ((***),first,second)
import Data.Foldable (foldr')
import Data.List
import Control.Monad ((=<<),(>=>))
import System.Environment (getArgs,getProgName)
import qualified Data.Set as Set (toList,fromList)
import qualified Data.Map.Strict as Map (lookup)
import Text.Read (readMaybe)

import Stats
import Text

nubOrd :: Ord a => [a] -> [a]
nubOrd = Set.toList . Set.fromList

statLines :: StatFn -> StatWSFn -> [Line] -> String
statLines statFn whitespaceFn = transpose >=> avg
  where avg :: [Token] -> String
        avg xs = case nubOrd *** statFn *** whitespaceFn $ foldl' (flip $ \case Character c -> first (c:)
                                                                                Number n x -> second $ first ((n,x):)
                                                                                Whitespace n -> second $ second (n:))
                                                            mempty xs of
                    ([c],(Nothing,0)) -> [c]
                    ([],(Just x,0))   -> uncurry niceFormat x
                    (cs,(Nothing,0))  -> "[" ++ cs ++ "]"
                    ([],(Nothing,n))  -> replicate n ' '
                    (cs,(Nothing,_))  -> "[ " ++ cs ++ "]"
                    (cs,(Just x,0))   -> "[" ++ cs ++ "|" ++ uncurry niceFormat x ++ "]"
                    (cs,(Just x,_))   -> "[ " ++ cs ++ "|" ++ uncurry niceFormat x ++ "]"

collapseIn :: Int -> [a] -> [[a]]
collapseIn n | n <= 0 = error "collapseIn: argument must be positive"
             | otherwise = uncurry (++) . second reverse . foldr' ff ([],replicate n [])
  where ff :: a -> ([[a]],[[a]]) -> ([[a]],[[a]])
        ff a (done,l:ls) = ((a:l):done,ls)
        ff a (done,[]) = ff a ([],reverse done)

parseArgs :: [String] -> Either String (Int,[(StatFn,StatWSFn)])
parseArgs [] = Right (1,defaultStat)
parseArgs (x:xs) = case readMaybe x of
    Just n  -> (n,) . (\case [] -> defaultStat; fns -> fns) <$> traverse findFn xs
    Nothing -> (1,) <$> traverse findFn (x:xs)
  where findFn :: String -> Either String (StatFn,StatWSFn)
        findFn k = maybe (Left k) Right $ Map.lookup k statFnMap

main :: IO ()
main = fmap parseArgs getArgs
        >>= \case Left wrong -> do putStrLn $ "Unknown method: " ++ wrong
                                   putStr . docText =<< getProgName
                  Right (n,fns) -> putStr . unlines . concatMap (\l -> nubOrd $ map (($l) . uncurry statLines) fns) . collapseIn n . tokenize =<< getContents

