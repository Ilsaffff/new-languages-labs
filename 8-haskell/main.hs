permutations :: [a] -> [[a]]
permutations [] = [[]]  
permutations (x:xs) = [y | ys <- permutations xs, y <- insertAll x ys]
  where
    insertAll :: a -> [a] -> [[a]]
    insertAll x [] = [[x]]  
    insertAll x (y:ys) = (x:y:ys) : map (y:) (insertAll x ys)


main :: IO ()
main = do
  let result = permutations [1, 2, 3, 6]
  print result