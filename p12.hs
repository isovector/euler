import Data.List (nub)

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

numFactors = length.factors.factorize

triangle 1 = 1
triangle n = n + triangle (n - 1)

main = putStrLn.show.triangle.head.take 1.filter ((>500).numFactors.triangle) $ [10..]
