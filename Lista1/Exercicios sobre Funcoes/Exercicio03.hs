
-- Autor: Erick Fernandes da Cruz

main = do
  print ("multEtiope aplicado em 2 4")
  print (multEtiope 2 4)
  print ("multEtiope aplicado em 1 5")
  print (multEtiope 1 5)
  print ("multEtiope aplicado em 6 3")
  print (multEtiope 6 3)

multEtiope :: Integer -> Integer -> Integer
multEtiope 1 n = n
multEtiope m n
    | par m = multEtiope (m `div` 2) (n*2)
    | otherwise = multEtiope (m `div` 2) ((n*2) + n)

par :: Integer -> Bool
par x = (x `rem` 2 == 0)