
-- Autor: Erick Fernandes da Cruz

main = do
  print ("Produto Escalar entre (3,4) (-2,5)")
  print (prodEscalar (3,4) (-2,5))

prodEscalar :: (Integer, Integer) -> (Integer, Integer) -> Integer
prodEscalar (a1, a2) (b1, b2) = (a1*b1) + (a2*b2)