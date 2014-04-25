import Data.List (sort, all)

digits = sort.show
multiples n = map (*n) [1,2,3,4,5,6]
results n = map digits$multiples n
isgood n = all (==xs!!0) xs
  where xs = results n

main=putStrLn.show.take 1.filter isgood$[1..]

