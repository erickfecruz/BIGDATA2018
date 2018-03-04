
-- Autor: Erick Fernandes da Cruz

module Main where

main :: IO ()
main = do
  let numeros = "0123456789"
  print (separaString numeros)
  
separaString :: String -> [Int]
separaString num
  | ((length num) == 1) = [charParaInt (head num)]
  | otherwise = charParaInt(head num):separaString (tail num)

charParaInt :: Char -> Int
charParaInt x
  | (x == '1') = 1
  | (x == '2') = 2
  | (x == '3') = 3
  | (x == '4') = 4
  | (x == '5') = 5
  | (x == '6') = 6
  | (x == '7') = 7
  | (x == '8') = 8
  | (x == '9') = 9
  | (x == '0') = 0
