import Data.List (nub)
import Data.Maybe (catMaybes)

isPrime n = n > 1 
            && foldr (\p r -> p*p > n || ((n `rem` p) /= 0 && r)) True primes
primes = 2 : filter isPrime [3,5..]

factorize :: Int -> [Int]
factorize n = go n primes
  where 
    go n ds@(d:t)
       | d*d > n   = [n]
       | r == 0    = d : go q ds
       | otherwise = go n t
         where (q, r) = quotRem n d

factors fac = nub.go 1 $ fac
  where
    go :: Int -> [Int] -> [Int]
    go n []         = [n]
    go n all@(x:xs) = (go n xs ++) $ go (n*x) xs

d n = sum.filter(/= n).factors.factorize$n

friend x | d(me) /= x = Nothing
         | x >= me    = Nothing
         | otherwise  = Just (x, me)
         where me = d(x)

add (a, b) = a + b
allPairs n = foldl (\a v -> a + add v) 0 . catMaybes . map friend $ [1..n]

main = putStrLn.show.allPairs$10000
