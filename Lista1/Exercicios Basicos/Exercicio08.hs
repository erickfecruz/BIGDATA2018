
-- Autor: Erick Fernandes da Cruz

module Main where

main :: IO ()
main = do
  print ("Lista de Anos Bissextos")
  let bissextos = [x | x <- [1..2017], bissexto x]
  print bissextos
  
bissexto :: Int -> Bool
bissexto ano = (ano `rem` 400 == 0) || ((ano `rem` 4 == 0) && (ano `rem` 100 /= 0))