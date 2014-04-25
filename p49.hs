import Data.Maybe(isJust)

import Data.Function (on)
import Data.List (nub, sort, group, groupBy, sortBy, find, (\\) )
import Data.Ord (comparing)

isPrime n = n > 1 
            && foldr (\p r -> p*p > n || ((n `rem` p) /= 0 && r)) True primes
primes = 2 : filter isPrime [3,5..]

oklen = (==4).length.show
lexorder = sort.show

thousandprimes = filter oklen.take 10000$primes
groups = groupBy ((==) `on` lexorder).sortBy (comparing lexorder)$thousandprimes
candidates = map sort.filter ((>=3).length) $ groups

powerset [] = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)

arithmetic [a,b,c] = b - a == c - b
arithmetic xs = True

solns=filter arithmetic.filter ((==3).length).concat.map powerset$candidates
--answer=map (foldl1 (++)).show$solns
answer=map (foldl1 (++).map show)$solns

main=putStrLn.show$answer
--main=putStrLn.show.filter ((/=0).length).map (nub.repeats.diffs.ziplist)$candidates
--main=putStrLn.show.fmap (3+) $ (5,6)
