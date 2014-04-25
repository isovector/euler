import Data.List (nub)

isPrime n = n > 1 
            && foldr (\p r -> p*p > n || ((n `rem` p) /= 0 && r)) True primes
primes = 2 : filter isPrime [3,5..]

candidates n = map (floor.(/2).fromIntegral).map (n-).filter (<n).drop 1.take n $ primes
conjecture n = any isSquare . candidates $ n

isSquare n = root * root == n
  where
    root = floor.sqrt.fromIntegral$n

main = putStrLn.show.take 1.filter (not.conjecture).filter (not.isPrime) $ [3,5..]
