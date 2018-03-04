
-- Autor: Erick Fernandes da Cruz

main = do
  print ("pascal aplicado em (1,1)")
  print (pascal (1,1))
  print ("pascal aplicado em (2,1)")
  print (pascal (2,1))

pascal :: (Integer, Integer) -> Integer
pascal (x,y) = binomial x y

binomial :: Integer -> Integer -> Integer
binomial 0 y = 1
binomial x 0 = 1
binomial x y = (fatorial x) `div` ((fatorial y) * (fatorial (x - y)))

fatorial :: Integer -> Integer
fatorial 0 = 1
fatorial 1 = 1
fatorial x = x * (fatorial (x-1))