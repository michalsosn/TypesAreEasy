module Combinators where

myReverse :: [a] -> [a]
myReverse xs = (foldr (\a z -> z . (a:)) id xs) []

i :: a -> a
i = s k k

k :: a -> b -> a
k = const

s :: (a -> b -> c) -> (a -> b) -> a -> c
s x y z = x z (y z)

myReverse2 :: [a] -> [a]
myReverse2 = s (foldr (s (k (s (.))) (s (k k) (:))) id) (k [])

--help :: a -> ([a] -> [a]) -> [a] -> [a]
--help a z = z . (a:)
--help a z = (.) z ((:) a)
--help a = s (.) (k ((:) a))
--help = s (k (s (.))) (s (k k) (:))

demo :: IO ()
demo = do
    print $ myReverse ([] :: [()])
    print $ myReverse2 [1]
    print $ myReverse "blergh"
    print $ myReverse2 [1..5]
