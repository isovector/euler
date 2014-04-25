tri x = floor $ n * (n+1) / 2
  where n = fromIntegral x
pen x = floor $ n*(3*n-1) / 2
  where n = fromIntegral x
hex n = n * (2*n-1)

soln = go 285 166 1
  where
    go a b c
      | t==p&&p==h = (a,b,c,t)
      | p<t = go a (b+1) c
      | h<t = go a b (c+1)
      | otherwise = go (a+1) b c
      where
        t = tri a
        p = pen b
        h = hex c

main = putStrLn.show$soln
