
-- Autor: Erick Fernandes da Cruz

main = do
  print ("typeTriangle aplicado em 1 1 1")
  print (typeTriangle 1 1 1)
  print ("typeTriangle aplicado em 5 10 9")
  print (typeTriangle 5 10 9)
  print ("typeTriangle aplicado em 2 2 1")
  print (typeTriangle 2 2 1)
  print ("typeTriangle aplicado em 5 1 1")
  print (typeTriangle 5 1 1)
  
typeTriangle :: Float -> Float -> Float -> String
typeTriangle x y z 
  | (abs (x-y) >= z) || (z >= (x+y)) = "Nao e triangulo"
  | ((x == y) && (y == z)) = "Triangulo equilatero"
  | ((x /= y) && (y /= z)) = "Triangulo escaleno"
  | otherwise = "Triangulo isosceles" 