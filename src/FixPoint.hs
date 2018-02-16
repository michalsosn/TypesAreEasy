module FixPoint where

import qualified Data.Time.Clock as T
import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import Debug.Trace

fix :: (a -> a) -> a
fix f = let x = f x in x

memoizeList :: (Int -> a) -> Int -> a
memoizeList f = (fmap f [0..] !!)

mlfix :: ((Int -> a) -> (Int -> a)) -> Int -> a
mlfix f = fix (memoizeList . f)

memoizeMap :: (Ord k) => S.Set k -> (k -> a) -> k -> a
memoizeMap ks f k = M.findWithDefault (f k) k memo
    where
        memo = M.fromSet f ks

mmfix :: (Ord k) => S.Set k -> ((k -> a) -> (k -> a)) -> k -> a
mmfix ks f = fix (memoizeMap ks . f)

mkLength :: ([a] -> Int) -> [a] -> Int
mkLength rec xs = case xs of [] -> 0; (_:xs') -> 1 + rec xs'

length' :: [a] -> Int
length' = fix mkLength

mkFib :: (Int -> Integer) -> Int -> Integer
mkFib _ 0 = 1
mkFib _ 1 = 1
mkFib rec n = rec (n - 1) + rec (n - 2)

fib' :: Int -> Integer
fib' = fix mkFib

fib'' :: Int -> Integer
fib'' = mlfix logMkFib

fib''' :: Int -> Integer
fib''' = mmfix (S.fromList [0..5]) (logMkFib)

logMkFib :: (Int -> Integer) -> Int -> Integer
logMkFib rec n = trace (show n) $ mkFib rec n

main :: IO ()
main = do
    timed $ print $ fib' 37
    timed $ print $ fib'' 5
    timed $ print $ fib''' 5


timed :: IO a -> IO a
timed action = do
    before <- T.getCurrentTime
    value  <- action
    after  <- T.getCurrentTime
    putStrLn $ "Took: " ++ show (T.diffUTCTime after before)
    return value


