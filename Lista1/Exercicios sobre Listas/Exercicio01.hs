
-- Autor: Erick Fernandes da Cruz

main = do
  print ("divisivel20 aplicado em 20")
  print (divisivel20 20)
  print ("divisivel20 aplicado em numero divisivel por todos os numeros de 1 a 20")
  let num = 2*3*4*5*6*7*8*9*10*11*12*13*14*15*16*17*18*19
  print (divisivel20 num)
  
divisivel :: [Integer] -> Integer -> Bool
divisivel [] _ = True
divisivel l x = (x `rem` (head l) == 0) && divisivel (tail l) x
 
divisivel20 :: Integer -> Bool
divisivel20 x = divisivel [1..20] x 