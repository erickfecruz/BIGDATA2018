
-- Autor: Erick Fernandes da Cruz

main = do
  print("Diagonal Matriz [[]]")
  let mat1 = [[]]
  print(diagonal mat1)
  print("Diagonal Matriz [[1,2,3], [3,4,5], [6,7,8]]")
  let mat2 = [[1,2,3], [3,4,5], [6,7,8]]
  print(diagonal mat2)
  print("Diagonal Matriz [[1,7], [3,2]")
  let mat3 = [[1,7], [3,2]]
  print(diagonal mat3)

diagonalSoma :: [[Int]] -> Int -> Int
diagonalSoma _ (-1) = 0
diagonalSoma x y = (x!!y!!y) + (diagonalSoma x (y-1))

diagonal :: [[Int]] -> Int
diagonal [[]] = 0
diagonal x = diagonalSoma x ((length x) - 1)