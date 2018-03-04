
-- Autor: Erick Fernandes da Cruz

main = do
  print ("somaDig aplicado em 8")
  print (somaDig 8)
  print ("somaDig aplicado em 261")
  print (somaDig 261)
  print ("somaDig aplicado em 50")
  print (somaDig 50)

somaDig :: Integer -> Integer
somaDig 0 = 0
somaDig x = (x `mod` 10) + somaDig(x `div` 10)