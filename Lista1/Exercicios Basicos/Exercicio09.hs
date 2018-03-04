
-- Autor: Erick Fernandes da Cruz

module Main where

main :: IO ()
main = do
  print ("10 Primeiros Anos Bissextos")
  let bissextos = [x | x <- [1..2017], bissexto x]
  print (take 10 bissextos)
  print ("10 Ultimos Anos Bissextos")
  print (drop (length bissextos - 10) bissextos)
  
bissexto :: Int -> Bool
bissexto ano = (ano `rem` 400 == 0) || ((ano `rem` 4 == 0) && (ano `rem` 100 /= 0))