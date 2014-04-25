fact 0 = 1
fact n = (*) n.fact $ n-1

nCr n r = quot (fact n) bottom
  where 
    bottom = (*fact r) $ fact (n-r)

main=putStrLn.show.length.filter(>1000000)$[nCr n r| n <- [1..100], r <- [1..n]]

