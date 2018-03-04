
-- Autor: Erick Fernandes da Cruz

module Main where

main :: IO ()
main = do
  print ("Palavras Separadas:")
  let s1 = "Palavra1"
  let s2 = "Palavra2"
  print s1
  print s2
  print ("Palavras Concatenadas:")
  print (concatena s1 s2)
  
concatena :: String -> String -> String
concatena string1 string2 = string1 ++ " " ++ string2
