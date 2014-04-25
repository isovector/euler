import Data.List (permutations, sort)



main=putStrLn.show.(!!999999).sort.permutations$"1234567890"
