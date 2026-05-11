class Punto p 
  where
    dimension :: p -> Int -- devuelve el n´umero de coordenadas de un punto
    coord :: Int -> p -> Double -- devuelve la coordenada k-´esima de un punto (comenzando de 0)
    dist :: p -> p -> Double -- calcula la distancia entre dos puntos
    dist p1 p2 = sqrt $ sum [((coord i p1) - (coord i p2)) | i <- [0..(dimension p1) - 1]]



