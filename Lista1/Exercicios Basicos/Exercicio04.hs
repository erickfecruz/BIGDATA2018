
-- Autor: Erick Fernandes da Cruz

module Main where

main :: IO ()
main = do
  print ("num3 aplicado em 3")
  print (num35 3)
  print ("num3 aplicado em 5")
  print (num35 5)
  print ("num3 aplicado em 15")
  print (num35 15)
  print ("num3 aplicado em 30")
  print (num35 30)

num5 :: Int -> Bool
num5 x
  | (x `rem` 5 == 0) = True
  | otherwise = False 

num3 :: Int -> Bool
num3 x
  | (x `rem` 3 == 0) = True
  | otherwise = False 

num35 :: Int -> Bool
num35 x
  | ((num3 x) && (num5 x)) = True
  | otherwise = False 