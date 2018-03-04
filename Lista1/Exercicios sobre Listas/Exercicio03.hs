
-- Autor: Erick Fernandes da Cruz

main = do
  print ("Lista com primeiros 5 elementos da sequencia de Fibonacci")
  print (take 5 lista)

lista = 1 : 2 : prox lista
  where
    prox (x : t@(y:_)) = (x+y) : prox t

