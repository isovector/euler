import Data.List (maximumBy)
import Data.Ord (comparing)

triangles p = concat$map (\b -> map (\a -> (a,b,p-a-b)) [1..b]) [1..p]
right = filter (\(a,b,c) -> a^2+b^2==c^2)

results n = length.right.triangles$n


main=putStrLn.show.maximumBy (comparing results)$[1..1000]
