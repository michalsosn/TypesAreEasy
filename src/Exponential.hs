module Exponential where

main :: IO ()
main = do
    let f0 x = (x, x)
        f1 y = f0 (f0 y) -- 4
        f2 y = f1 (f1 y) -- 16
        f3 y = f2 (f2 y) -- 256
        f4 y = f3 (f3 y) -- 65536  -- 4294967296
    print $ f4 0
