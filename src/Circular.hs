module Circular where

circular :: [Int] -> ([Int], [Int])
circular [] = ([], [])
circular xs@(hd:_) =
    let (less, more, mean) = aux mean xs [] [] hd hd
    in  (less, more)
    where
        aux :: Int -> [Int] -> [Int] -> [Int] -> Int -> Int -> ([Int], [Int], Int)
        aux mid [] less more minx maxx = (less, more, (minx + maxx) `div` 2)
        aux mid (x:xs) less more minx maxx =
            let less' = if x <= mid then x:less else less
                more' = if x <= mid then more else x:more
                minx' = min x minx
                maxx' = max x maxx
            in  aux mid xs less' more' minx' maxx'
-- WRONG! NOT | x <= mid  = aux mid xs (x:less) more (min x minx) maxx
--  CIRCULAR  | otherwise = aux mid xs less (x:more) minx (max x maxx)

demo :: IO ()
demo = do
    print $ circular [1, 2, 4, 6, 3, 9, 0, 8]
