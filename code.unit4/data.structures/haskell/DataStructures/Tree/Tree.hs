import Control.Applicative (Alternative(empty))
data Tree a = Empty | Node a [Tree a] deriving Show

tree1 :: Tree Int
tree1 = Node  1 [Node 2[Node 4[]
                        , Node 5[]
                        , Node 6 []
                        ]
                , Node 3[Node 7[]]
                ]

sizeT :: Tree a -> Int
sizeT Empty = 0
--sizeT (Nodo raiz listaDeArboles ) = 1(del x) + sumatorio [sizeT t | t es elemento de ts]
sizeT (Node x ts) = 1  + sum [ sizeT  t | t <- ts]  

sumT :: (Num a) => Tree a -> a
sumT Empty = 0
sumT (Node x ts) = x + sum [sumT t | t<-ts]

heighT :: Tree a -> Int
heighT Empty = 0
heighT (Node x []) = 1 -- Definimos esto pq maximun de listaVacia da error
--heighT (Nodo raiz listaDeArboles ) = 1(del x) + maximo [heighT t | t es elemento de ts]
heighT (Node x ts) = 1 + maximum [heighT t | t<-ts]--Estas lista de compresion que estamos definiendo son de enteros
--Se podría hacer con | pero esto implicaría que 'a'  tendría que ser der tipo Eq para poder comparar las listas vacías

containsT :: (Eq a) => a->Tree a -> Bool
containsT x Empty = False
containsT x (Node y ts) = x == y || or [containsT x t | t<-ts]
--containsT x (Node y ts) = (( x == y): or [containsT x t | t<-ts]) Esto es otra opcion usando compresión de Booleanos

atLevelT :: Int -> Tree a -> [a]
atLevelT n Empty = []
atLevelT n (Node x ts) 
    |n<0 = error "Nivel negativo"
    |n==0 = [x]--devolvemos una lista con la raiz del arbol
    |otherwise = concat [atLevelT (n-1) t | t<-ts] 
    -- Hay que hacer concat pq tenemos una lista de listas y queremos una unica lista de solucion

leafsT ::Tree a -> [a]
leafsT Empty = []
leafsT (Node x []) = [x]
leafsT (Node x ts) = concat [leafsT t | t<-ts]


data TreeB a = EmptyB | NodeB a (TreeB a) (TreeB a) deriving Show

treeB1 :: TreeB Int
treeB1 = NodeB 1 (NodeB 2 --Hijo izquierdo
                          (NodeB 4 EmptyB EmptyB)
                          (NodeB 5 EmptyB EmptyB)
                 )
                 (NodeB 3 --Hijo derecho
                          (NodeB 6 EmptyB EmptyB) --EmptyB EmptyB representa que no tiene hijos ni izquierdo ni derecho
                           EmptyB
                 )

sizeB :: TreeB a -> Int
sizeB EmptyB = 0
sizeB (NodeB x lt rt) = 1 + sizeB lt + sizeB rt

sumB :: (Num a) => TreeB a -> a
sumB EmptyB = 0
sumB (NodeB x lt rt) = x + sumB lt + sumB rt

heighB :: TreeB a -> Int
heighB EmptyB = 0
heighB (NodeB x lt rt) = 1 + max (heighB lt) (heighB rt)

