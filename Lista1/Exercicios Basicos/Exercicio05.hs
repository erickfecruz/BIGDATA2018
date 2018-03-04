
-- Autor: Erick Fernandes da Cruz

module Main where

main :: IO ()
main = do
  print ("exe5 aplicado em -2")
  print (exe5 (-2))
  print ("exe5 aplicado em -1")
  print (exe5 (-1))
  print ("exe5 aplicado em 3")
  print (exe5 3)
  print ("exe5 aplicado em 4")
  print (exe5 4)

exe5 :: Int -> Bool
exe5 x
  | (x < (-1)) = True
  | (x `rem` 2 == 0) && (x > 1) = True
  | otherwise = False 