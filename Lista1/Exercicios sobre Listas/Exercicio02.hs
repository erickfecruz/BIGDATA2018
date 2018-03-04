
-- Autor: Erick Fernandes da Cruz

main = do
  print ("Primeiro numero divisivel pela lista de numeros")
  print (projectEuler5)
  
divisivel :: [Integer] -> Integer -> Bool
divisivel [] _ = True
divisivel l x = (x `rem` (head l) == 0) && divisivel (tail l) x
 
divisivel20 :: Integer -> Bool
divisivel20 x = divisivel [1..20] x 

projectEuler5 :: Integer
projectEuler5 = projectEuler5' 20

projectEuler5' :: Integer -> Integer
projectEuler5' x
  | (divisivel20 x) = x
  | otherwise = projectEuler5' (x+20)
