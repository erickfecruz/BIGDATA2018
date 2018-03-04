
-- Autor: Erick Fernandes da Cruz

module Main where

main :: IO ()
main = do
  print ("div2 aplicado em 3")
  print (div2 3)
  print ("div2 aplicado em 4")
  print (div2 4)

div2 :: Int -> Double
div2 x = fromIntegral x / 2 
  