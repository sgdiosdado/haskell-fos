-- rest_col
resto_col :: [[Int]] -> [[Int]]
resto_col [] = []
resto_col a = map (\(x:xs) -> xs) a

-- sumaMatriz 
sumaMatriz :: [[Int]] -> Int
sumaMatriz [] = 0
sumaMatriz a = foldl (+) 0 (map (foldl (+) 0) a)

-- trans
trans :: [[a]] -> [[a]]
trans ([]:_) = []
trans a = (map head a) : trans (map tail a)

-- aplicaN
aplicaN :: (a -> a) -> Int -> a -> a
aplicaN f n x 
  | n == 0 = x
  | otherwise = f (aplicaN f (n-1) x)

-- secuencia
-- Mientras la longitud de la lista sea menor que el argumento,
-- agrega un elemento, el cuál será el resultado del último elemento actual + 1
secuencia :: Int -> [Int]
secuencia 0 = []
secuencia n = until (\l -> (length l) == n) (\x -> x ++ [((last x) + 1)]) [1]

main = do
  print("Suma matriz: ")
  print(sumaMatriz [])
  print(sumaMatriz [[1, 2, 3], [4, 5, 6]])
  print(sumaMatriz [[1, 2], [5, 6]])
  print("Resto col: ")
  print(resto_col [])
  print(resto_col [[1, 2, 3], [4, 5, 6]])
  print("Trans: ")
  print(trans ["ab","cd"])
  print(trans [[1, 2, 3], [4, 5, 6]])
  print("AplicaN: ")
  print((aplicaN (+ 1) 7) 5)
  print((aplicaN (\x->x*x) 2) 3)
  print((aplicaN tail 3) [1,2,3,4])
  print("Secuencia: ")
  print(secuencia 3)
  print(secuencia 5)
