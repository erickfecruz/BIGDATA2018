
-- Autor: Erick Fernandes da Cruz

main = do
  print ("Matriz Identidade 1")
  print (identidade 1)
  print ("Matriz Identidade 3")
  print (identidade 3)

identidade :: Int -> [[Int]]
identidade 0 = [[]]
identidade n = matriz n n

matriz :: Int -> Int -> [[Int]]
matriz n 1 = [valores n 1]
matriz n x = valores n x : matriz n (x-1)

valores :: Int -> Int -> [Int]
valores 0 _ = []
valores n x
  | (n /= x) = 0:(valores (n-1) x)
  | otherwise = 1:(valores (n-1) x)