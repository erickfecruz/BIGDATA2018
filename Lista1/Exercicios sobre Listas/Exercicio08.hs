
-- Autor: Erick Fernandes da Cruz

main = do
  print ("Numero com maior sequencia de Collatz")
  print (maiorIndexCollatz)

collatzLen :: Integer -> Integer
collatzLen x
  | (x == 1) = 1
  | otherwise = 1 + collatzLen(collatz x)

collatz :: Integer -> Integer
collatz x
  | (x `rem` 2 == 0) = x `div` 2
  | otherwise = ((3 * x) + 1)

maiorCollatz :: Integer
maiorCollatz = maximum (map collatzLen [1..1000000])

findIndexMaiorCollatz :: [Integer] -> Integer -> Integer
findIndexMaiorCollatz lista maior
  | ((head lista) == maior) = 1
  | otherwise = 1 + findIndexMaiorCollatz (tail lista) maior

maiorIndexCollatz :: Integer 
maiorIndexCollatz = findIndexMaiorCollatz (map collatzLen [1..1000000]) maiorCollatz