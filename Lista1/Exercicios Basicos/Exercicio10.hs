
-- Autor: Erick Fernandes da Cruz

module Main where

main :: IO ()
main = do
  print ("Tupla com Anos Bissextos")
  let bissextos = [x | x <- [1..2017], bissexto x]
  let tuplaBissexto = ((take ((length bissextos) `div` 2) bissextos),(drop ((length bissextos) `div` 2) bissextos))
  print tuplaBissexto
  
bissexto :: Int -> Bool
bissexto ano = (ano `rem` 400 == 0) || ((ano `rem` 4 == 0) && (ano `rem` 100 /= 0))

