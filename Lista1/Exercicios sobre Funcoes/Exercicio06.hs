
-- Autor: Erick Fernandes da Cruz

main = do
  print ("persistencia aplicado em 2718")
  print (persistencia 2718)
  print ("persistencia aplicado em 55")
  print (persistencia 55)
  print ("persistencia aplicado em 999999")
  print (persistencia 999999)
  
persistencia :: Integer -> Integer
persistencia x
  | (x `div` 10 == 0) = x
  | otherwise = persistencia $ somaDig x

somaDig :: Integer -> Integer
somaDig 0 = 0
somaDig x = (x `mod` 10) + somaDig(x `div` 10)