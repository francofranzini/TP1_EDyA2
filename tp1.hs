data NdTree p = 
  Node (NdTree p) -- sub´arbol izquierdo
  p -- punto
  (NdTree p) -- sub´arbol derecho
  Int -- eje
  | Empty
deriving (Eq, Ord, Show)

class Punto p 
  where
    dimension :: p → Int -- devuelve el n´umero de coordenadas de un punto
    coord :: Int → p → Double -- devuelve la coordenada k-´esima de un punto (comenzando de 0)
    dist :: p → p → Double -- calcula la distancia entre dos puntos

