fib :: Int -> Integer
fib 1 = 1
fib 2 = 1
fib n = a + b
  where
    a = f !! 0
    b = f !! 1
    f = reverse . take (n-1) $ fibs

fibs = map fib [1..]

main=putStrLn.show.take 1.filter((>=1000).length.show.fib)$[1..]
