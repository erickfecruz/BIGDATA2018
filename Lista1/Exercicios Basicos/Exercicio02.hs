
-- Autor: Erick Fernandes da Cruz

module Main where

main :: IO ()
main = do
  print ("num3 aplicado em 3")
  print (num3 3)
  print ("num3 aplicado em 5")
  print (num3 5)
  print ("num3 aplicado em 9")
  print (num3 9)

num3 :: Int -> Bool
num3 x
  | (x `rem` 3 == 0) = True
  | otherwise = False 