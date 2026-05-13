
import Data.List (sortBy)
import Data.Ord (comparing)


data NdTree p = 
  Node 
  (NdTree p) -- sub´arbol izquierdo
  p -- punto
  (NdTree p) -- sub´arbol derecho
  Int -- eje
  | Empty
  deriving (Eq, Ord, Show)

class Punto p 
  where
    dimension :: p -> Int -- devuelve el n´umero de coordenadas de un punto
    coord :: Int -> p -> Double -- devuelve la coordenada k-´esima de un punto (comenzando de 0)
    dist :: p -> p -> Double -- calcula la distancia entre dos puntos
    dist p1 p2 = sqrt (sum [((coord i p1) - (coord i p2))^2 | i <- [0..(dimension p1) - 1]])


newtype Punto2d = P2d (Double, Double) deriving Show
newtype Punto3d = P3d (Double, Double, Double) deriving Show

instance Punto Punto2d where
    dimension _ = 2
    coord 0 (P2d (x, _)) = x
    coord 1 (P2d (_, y)) = y

instance Punto Punto3d where
    dimension _ = 3
    coord 0 (P3d (x, y, z)) = x
    coord 1 (P3d (x, y, z)) = y
    coord 2 (P3d (x, y, z)) = z 

---------------------------- EJERCICIO 2 ---------------------------------

auxFromList :: Punto p => [p] -> Int ->Int -> NdTree p
auxFromList [] _ _ = Empty
auxFromList lp k n= let (menores, mediana, mayores) = partir (ordenarPorCoord k lp); lvl_sig = ((k+1) `mod` n) 
                    in (Node 
                    (auxFromList menores lvl_sig n) 
                    mediana 
                    (auxFromList mayores lvl_sig n)
                    k)

ordenarPorCoord :: Punto p => Int -> [p] -> [p]
ordenarPorCoord k ps = sortBy (comparing (coord k)) ps

partir :: Punto p => [p] -> ([p], p, [p])
partir ps = (menores, mediana, mayores)
  where
    n                = length ps
    mitad            = n `div` 2
    (menores, resto) = splitAt mitad ps
    mediana          = head resto
    mayores          = tail resto



fromList :: Punto p => [p] -> NdTree p
fromList [] = Empty
fromList lp = auxFromList lp 0 (dimension (head lp))


---------------------------- EJERCICIO 3 ---------------------------------

insertar :: Punto p => p -> NdTree p -> NdTree p
insertar x Empty = Node Empty x Empty 0 
insertar x (Node izq p der k)
  | coord k x < coord k p = Node (insertar' x izq ((k+1) `mod` dimension p)) p der k
  | otherwise              = Node izq p (insertar' x der ((k+1) `mod` dimension p)) k

insertar' :: Punto p => p -> NdTree p -> Int -> NdTree p
insertar' x Empty k      = Node Empty x Empty k
insertar' x (Node izq p der k) _ 
  | coord k x < coord k p = Node (insertar' x izq ((k+1) `mod` dimension p)) p der k
  | otherwise              = Node izq p (insertar' x der ((k+1) `mod` dimension p)) k

---------------------------- EJERCICIO 4 ---------------------------------


buscarKMenor :: Punto p => Int -> NdTree p -> Maybe p
buscarKMenor _ Empty = Nothing
buscarKMenor k (Node izq x der i) = if k == i then aux (buscarKMenor k izq) x Nothing else aux minIzq x minDer
    where
        minIzq = buscarKMenor k izq
        minDer = buscarKMenor k der
        aux Nothing x Nothing   = Just x
        aux Nothing x (Just y)  = Just (minCoord k x y)
        aux (Just x) y Nothing  = Just (minCoord k x y)
        aux (Just x) y (Just z) = Just (minCoord k x (minCoord k y z))
        minCoord k x y = if (coord k x) < (coord k y) then x else y

buscarKMayor :: Punto p => Int -> NdTree p -> Maybe p
buscarKMayor _ Empty = Nothing
buscarKMayor k (Node izq x der i) = if k == i then aux Nothing x (buscarKMayor k der) else aux maxIzq x maxDer
    where
        maxIzq = buscarKMayor k izq
        maxDer = buscarKMayor k der
        aux Nothing x Nothing   = Just x
        aux Nothing x (Just y)  = Just (maxCoord k x y)
        aux (Just x) y Nothing  = Just (maxCoord k x y)
        aux (Just x) y (Just z) = Just (maxCoord k x (maxCoord k y z))
        maxCoord k x y = if (coord k x) > (coord k y) then x else y

compP :: Punto p => p -> p -> Int -> Bool
compP x y k = (coord k x) < (coord k y)

eliminar :: (Eq p, Punto p) => p -> NdTree p -> NdTree p
eliminar p Empty = Empty
eliminar p t@(Node Empty x Empty k) = if p == x then Empty else t

eliminar p t@(Node Empty x der   k) | p == x      = (Node Empty minimo_der (eliminar minimo_der der) k) 
                                    | compP p x k = t
                                    | otherwise   = (Node Empty x (eliminar p der) k)
                                    where Just minimo_der = buscarKMenor k der

eliminar p t@(Node izq x Empty   k) | p == x      = (Node (eliminar maximo_izq izq) maximo_izq Empty k)
                                    | compP p x k = (Node (eliminar p izq) x Empty k)
                                    | otherwise   = t
                                    where Just maximo_izq = buscarKMayor k izq

eliminar p t@(Node izq x der k)     | p == x      = (Node izq minimo_der (eliminar minimo_der der) k)
                                    | compP p x k = (Node (eliminar p izq) x der k) 
                                    | otherwise   = (Node izq x (eliminar p der) k)
                                    where Just minimo_der = buscarKMenor k der

---------------------------- EJERCICIO 5 ---------------------------------
type Rect = (Punto2d, Punto2d)
inRegion :: Punto2d -> Rect -> Bool
inRegion (P2d (x1, x2)) (p1, p2) = 
  let min_x = (min (coord 0 p1) (coord 0 p2))
      max_x = (max (coord 0 p1) (coord 0 p2))
      min_y = (min (coord 1 p1) (coord 1 p2))
      max_y = (max (coord 1 p1) (coord 1 p2))
  in (x1 >= min_x) && (x1 <= max_x) && (x2 >= min_y) && (x2 <= max_y)

ortogonalSearch :: NdTree Punto2d -> Rect -> [Punto2d]
ortogonalSearch tree rect = aux tree []
  where
    (min_x, max_x, min_y, max_y) = limites rect

    limites (P2d(x1,y1), P2d(x2,y2)) = ((min x1 x2), (max x1 x2), (min y1 y2), (max y1 y2))

    aux Empty xs = xs
    aux (Node l x r k) xs = 
      if inRegion x rect 
      then (aux l (aux r (x:xs)))
      else if (k == 0) && ((coord 0 x) > max_x) then (aux l xs) --Se va por derecha
      else if (k == 0) && ((coord 0 x) < min_x) then (aux r xs) --Se va por izquierda
      else if (k == 1) && ((coord 1 x) > max_y) then (aux l xs) --Se va por arriba
      else if (k == 1) && ((coord 1 x) < min_y) then (aux r xs) -- Se va por abajo
      else (aux l (aux r xs))

                            
