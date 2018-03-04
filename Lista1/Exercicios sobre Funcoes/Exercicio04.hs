
-- Autor: Erick Fernandes da Cruz

main = do
  print ("isPrime aplicado em 1")
  print (isPrime 1)
  print ("isPrime aplicado em 2")
  print (isPrime 2)
  print ("isPrime aplicado em 16")
  print (isPrime 16)
  print ("isPrime aplicado em 7")
  print (isPrime 7)

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime x = isPrime' x (x-1)

isPrime' :: Integer -> Integer -> Bool
isPrime' x 2 = divisivel x 2
isPrime' x y = (divisivel x y) || (isPrime' (x) (y-1))

divisivel :: Integer -> Integer -> Bool
divisivel x y = (x `rem` y == 0)