
-- Autor: Erick Fernandes da Cruz

main = do
  print ("Soma numeros pares da sequencia de Fibonacci menores que 4000000")
  print (euler2)

lista = 1 : 2 : prox lista
  where
    prox (x : t@(y:_)) = (x+y) : prox t

euler2 :: Integer
euler2 = somaListaPares listaEuler

listaEuler :: [Integer]
listaEuler = filter (\x -> x < 4000000) (take 50 lista)

somaListaPares :: [Integer] -> Integer
somaListaPares [x]
  | par x = x
  | otherwise = 0
somaListaPares (x:xs)
  | par x = x + somaListaPares xs
  | otherwise = somaListaPares xs

par :: Integer -> Bool
par x 
  | (x `rem` 2 == 0) = True
  | otherwise = False