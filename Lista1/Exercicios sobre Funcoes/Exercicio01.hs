
-- Autor: Erick Fernandes da Cruz

main = do
  print ("IsTriangle aplicado em 1 1 1")
  print (isTriangle 1 1 1)
  print ("IsTriangle aplicado em 5 10 9")
  print (isTriangle 5 10 9)
  print ("IsTriangle aplicado em 5 1 1")
  print (isTriangle 5 1 1)
  
isTriangle :: Float -> Float -> Float -> Bool
isTriangle x y z 
  | (abs (x-y) < z) && (z < (x+y)) = True
  | otherwise = False 