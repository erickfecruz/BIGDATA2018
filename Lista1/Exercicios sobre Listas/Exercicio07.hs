
-- Autor: Erick Fernandes da Cruz

main = do
  print ("collatzLen 5")
  print (collatzLen 5)
  print ("collatzLen 4")
  print (collatzLen 4)

collatzLen :: Integer -> Integer
collatzLen x
  | (x == 1) = 1
  | otherwise = 1 + collatzLen(collatz x)

collatz :: Integer -> Integer
collatz x
  | (x `rem` 2 == 0) = x `div` 2
  | otherwise = ((3 * x) + 1)