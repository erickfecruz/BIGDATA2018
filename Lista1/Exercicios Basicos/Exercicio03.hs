
-- Autor: Erick Fernandes da Cruz

module Main where

main :: IO ()
main = do
  print ("num3 aplicado em 3")
  print (num5 3)
  print ("num3 aplicado em 5")
  print (num5 5)
  print ("num3 aplicado em 9")
  print (num5 9)

num5 :: Int -> Bool
num5 x
  | (x `rem` 5 == 0) = True
  | otherwise = False 