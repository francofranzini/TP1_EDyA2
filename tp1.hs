
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


newtype Punto2d = P2d (Double, Double)
newtype Punto3d = P3d (Double, Double, Double)

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
buscarKMenor k (Node izq x der i) = if k == i then buscarKMenor k izq else aux minIzq x minDer
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
buscarKMayor k (Node izq x der i) = if k == i then buscarKMayor k der else aux maxIzq x maxDer
    where
        maxIzq = buscarKMayor k izq
        maxDer = buscarKMayor k der
        aux Nothing x Nothing   = Just x
        aux Nothing x (Just x)  = Just (maxCoord k x y)
        aux (Just x) y Nothing  = Just (maxCoord k x y)
        aux (Just x) y (Just z) = Just (maxcoord k x (maxcoord k z))
        maxCoord k x y = if (coord k x) > (coord k y) then x else y

compP :: Punto p => p -> p -> Int -> Bool
compP x y k = (coord k x) < (coord k y)

eliminar :: Punto p => p -> NdTree p -> NdTree p
eliminar p Empty = Empty
eliminar p t@(Node Empty x Empty k) = if p == x then Empty else t

eliminar p t@(Node Empty x der   k) | p == x      = (Node Empty val (eliminar val der) k) 
                                        where Just val = buscarKMenor k der
                                    | compP p x k = t
                                    | otherwise   = (Node Empty x (eliminar p der) k)

eliminar p t@(Node izq x Empty   k) | p == x      = (Node (eliminar val izq) val Empty k)
                                        where Just val = buscarKMayor k izq
                                    | compP p x k = (Node (eliminar p izq) x Empty k)
                                    | otherwise   = t

eliminar p t@(Node izq x der k)     | p == x      = (Node izq val (eliminar val der) k)
                                        where Just val = buscarKMenor k der
                                    | compP p x k = (Node (eliminar p izq) x der k) 
                                    | otherwise   = (Node izq x (eliminar p der) k)
