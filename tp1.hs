class Punto p 
  where
    dimension :: p -> Int -- devuelve el n´umero de coordenadas de un punto
    coord :: Int -> p -> Double -- devuelve la coordenada k-´esima de un punto (comenzando de 0)
    dist :: p -> p -> Double -- calcula la distancia entre dos puntos
    dist p1 p2 = sqrt (sum [((coord i p1) - (coord i p2)) | i <- [0..(dimension p1) - 1]])
    

newType Punto2d = P2d (Double, Double)
newType Punto3d = P3d (Double, Double, Double)

instance Punto Punto2d where
    dimension _ = 2

    coord 0 (P2d (x, _)) = x
    coord 1 (P2d (_, y)) = y

    dist (P2d (x1, y1)) (P2d (x2, y2)) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

instance Punto Punto3d where
    dimension _ = 3

    coord 0 (P3d (x, y, z)) = x
    coord 1 (P3d (x, y, z)) = y
    coord 2 (P3d (x, y, z)) = z
    
    dist (P3d (x1, y1, z1)) (P3d (x2, y2, z2)) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2 + (z1 - z2) ^ 2)

-- ej2

auxFromList :: Punto p => [p] -> Int -> NdTree p
auxFromList [] = Empty
auxFromList lp = t1

listMedian :: Punto p => [p] -> Int -> (p, [p], [p])
listMedian 

qsortList :: Punto p => [p] -> Int -> [p]
qsortList [] _          = Empty
qsortList l@(x : []) _  = x
qsortList (x : xs) k    = (qsortList izq) ++ [x] ++ (qsortList der) 
    where
        izq = filter (\a -> (coord k x) >= (coord k a)) xs
        der = filter (\a -> (coord k x) <  (coord k a)) xs


fromList :: Punto p => [p] -> NdTree p
fromList [] = Empty
fromList lp = auxFromList lp 0

