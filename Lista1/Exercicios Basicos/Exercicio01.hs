
module Main where

-- |'main' executa programa principal
main :: IO ()
main = do
  print ("PRIMEIRA OPERACAO")
  print (2*3+5)
  print ("SEGUNDA OPERACAO")
  print (2+2*3+1)
  print ("TERCEIRA OPERACAO")
  print (3^4+5*2^5+1)