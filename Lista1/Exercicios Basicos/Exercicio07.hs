
-- Autor: Erick Fernandes da Cruz

module Main where

main :: IO ()
main = do
  print ("exe7 aplicado em 0")
  print (exe7 0)
  print ("exe7 aplicado em pi")
  print (exe7 pi)

exe7 :: Float -> (Float,Float)
exe7 x = (y,-y)
  where
  	y = sqrt((1-cos(x))/2)

