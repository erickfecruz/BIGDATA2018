
-- Autor: Erick Fernandes da Cruz

main = do
  print ("binomial aplicado em 5 2")
  print (binomial 5 2)
  print ("binomial aplicado em 10 6")
  print (binomial 10 6)
  
binomial :: Integer -> Integer -> Integer
binomial 0 y = 1
binomial x 0 = 1
binomial x y = (fatorial x) `div` ((fatorial y) * (fatorial (x - y)))

fatorial :: Integer -> Integer
fatorial 0 = 1
fatorial 1 = 1
fatorial x = x * (fatorial (x-1))