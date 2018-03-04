
-- Autor: Erick Fernandes da Cruz

main = do
  print ("Collatz 4")
  print (collatz 4)
  print ("Collatz 5")
  print (collatz 5)

collatz :: Integer -> Integer
collatz x
  | (x `rem` 2 == 0) = x `div` 2
  | otherwise = ((3 * x) + 1)