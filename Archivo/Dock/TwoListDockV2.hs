-- =====================================================
-- ============ Puntuación máxima 1.5 =================
-- =====================================================
-- Un Dock es una secuencia de elementos en el que hay uno destacado (sign)
-- Las operaciones disponibles para un dock son:
-- Borrar el elemento destacado, añadir un nuevo elemento delante o detrás del destacado,
-- cambiar el elemento destacado al elemento anterior o al siguiente,
-- saber si la secuencia es vacia o si el destacado es el primero o el último
-- y crear un dock a partir de una lista

module DataStructures.Dock.TwoListDock  (
    empty,           -- :: Dock a
    isEmpty,         -- :: Dock a -> Bool
    sign,            -- :: Dock a ->  a
    isFirst,         -- :: Dock a -> Bool
    isLast,          -- :: Dock a -> Bool
    left,            -- :: Dock a ->  Dock a
    right,           -- :: Dock a -> Dock a
    delete,          -- :: Dock a ->  Dock a
    insertl,         -- :: a  ->  Dock a ->  Dock a
    dockToList,      -- :: Dock a -> [a]
    listToDock       -- :: [a] -> Dock a
    
) where

import           Data.List                        (intercalate)
import           Test.QuickCheck

-- INVARIANTES:
-- Siempre hay un elemento destacado (salvo en el dock vacío)
-- Siempre mantendremos el elemento destacado en la primera posición de la primera lista.

data Dock a =  D [a] [a]
-- Ejemplo. Si tenemos el dock: 1 2 3 <4> 5 donde el elemento destacado es el 4, se presentará por el dato
-- D [4,3,2,1] [5]
-- Su show será TwoListDock(1,2,3,<4>,5)

sample1 = D [4,3,2,1] [5]


-- ===========================================================
-- Ejercicio 1 (0.05 ptos.)
-- Crea una dock vacío 
-- O(1)
empty :: Dock a
empty = D [] []

-- ===========================================================
-- Ejercicio 2 (0.05 ptos.)
-- Determina si un dock está vacío 
-- O(1)
isEmpty :: Dock a -> Bool
isEmpty (D [] []) = True
isEmpty _ = False

-- ===========================================================
-- Ejercicio 3 (0.10 ptos.)
-- Devuelve el elemento destacado. Error si está vacío
-- O(1)
sign :: Dock a ->  a
sign (D [] _ ) = error "Empty Dock"
sign (D (x:xs) _ ) = x

{-
Prelude (Dock.hs)> sign sample1
4
-}

-- ===========================================================
-- Ejercicio 4 (0.10 ptos.)
-- Devuelve cierto si el destacado es el primero (o está vacía)
-- O(1)
isFirst :: Dock a -> Bool
isFirst (D [] []) = True
isFirst (D xs _ )
    | length xs == 1 = True
    | otherwise = False

{-
Prelude (Dock.hs)> isFirst sample1
False
-}
-- ===========================================================
-- Ejercicio 5 (0.10 ptos.)
-- Devuelve cierto si el destacado es el último (o está vacía)
-- O(1)
isLast :: Dock a -> Bool
isLast (D [] []) = True
isLast (D xs ys)
    | (length xs >= 1) && (length ys == 0) = True
    | otherwise = False

{-
Prelude (Dock.hs)> isLast sample1
False
-}

-- ===========================================================
-- Ejercicio 6 (0.20 ptos.)
-- Cambia el elemento destacado que pasa a ser el de la izquierda.
-- Si está vacío o el destacado es el primero lo deja igual
-- O(1)
left :: Dock a ->  Dock a
left (D [] []) = D [] []
left (D (x:xs) ys)
    | isFirst (D (x:xs) ys) = (D (x:xs) ys)
    | otherwise = (D xs (x:ys))
{-
Prelude (Dock.hs)> left sample1
TwoListDock(1,2,<3>,4,5)
Prelude (Dock.hs)> left $ left sample1
TwoListDock(1,<2>,3,4,5)
Prelude (Dock.hs)> left $ left $ left sample1
TwoListDock(<1>,2,3,4,5)
Prelude (Dock.hs)> left $ left $ left $ left sample1
TwoListDock(<1>,2,3,4,5)
Prelude (Dock.hs)> isFirst $ left $ left $ left sample1
True
-}
-- ===========================================================
-- Ejercicio 7 (0.20 ptos.)
-- El elemento destacado pasa a ser el de la derecha.
-- Si está vacío o el destacado es el último  lo deja igual
-- O(1)
right :: Dock a ->  Dock a
right (D [] []) = D [] []
right (D xs ys)
    | isLast (D xs ys) = (D xs ys)
    | otherwise = (D ((head ys):xs) (tail ys))

{-
Prelude (Dock.hs)> right sample1
TwoListDock(1,2,3,4,<5>)
Prelude (Dock.hs)> right $ right sample1
TwoListDock(1,2,3,4,<5>)
Prelude (Dock.hs)> isLast $ right sample1
True
-}
-- ===========================================================
-- Ejercicio 8 (0.20 ptos.)
-- Elimina el objeto destacado. El destacado pasa a ser el anterior.
-- Si no hay anterior pasa a ser el siguiente.
-- Si queda vacía no hay destacado.
-- Error si está vacía
-- O(1)
delete :: Dock a ->  Dock a
delete (D [] []) = error "Empty Dock"
delete (D (x:xs) (y:ys))
    | length xs > 0 = (D xs (y:ys))
    | length (y:ys) > 0 = (D [y] ys)
    | otherwise = D [] []
-- FIX

{-
Prelude (Dock.hs)> delete sample1
TwoListDock(1,2,<3>,5)
Prelude (Dock.hs)> delete $ delete sample1
TwoListDock(1,<2>,5)
Prelude (Dock.hs)> delete $ delete $ delete sample1
TwoListDock(<1>,5)
Prelude (Dock.hs)> delete $ delete $ delete $ delete sample1
TwoListDock(<5>)
Prelude (Dock.hs)> delete $ delete $ delete $ delete $ delete sample1
TwoListDock()
-}
-- ===========================================================
-- Ejercicio 9 (0.20 ptos.)
-- inserta el elemento a la izquierda del destacado y este elemento pasa a ser el destacado
-- Si el Dock esta vacía el elemento se convierte en el destacado
-- O(1)
insertl :: a  ->  Dock a ->  Dock a
insertl e (D [] [])= (D [e] [])
insertl e (D (x:xs) ys) = (D (e:xs) (x:ys))


-- ===========================================================
-- Ejercicio 10  (0.10-0.20)
-- devuelve una lista con los elementos del Dock en el orden en el que están (del primero al último)
-- Implementar usando plegado de listas (0.20) otro metodo (0.10)

dockToList :: Dock a -> [a]
dockToList (D xs ys) = reverse xs ++ ys

-- ===========================================================
-- Ejercicio 11 (0.10 ptos.)
-- genera un dock con los elementos de la lista. El destacado será el primero de la lista
-- O(n)
listToDock :: [a] -> Dock a
listToDock xs = foldr insertl empty xs

{-
Prelude (Dock.hs)> listToDock [1..5]
TwoListDock(<1>,2,3,4,5)
-}
-- =============================================================================
-- ========================= NO TOCAR DE AQUÍ PARA ABAJO =======================
instance (Show a) => Show (Dock a) where
    show d@(D si sd) | isEmpty d = "TwoListDock()"
    show d@(D si sd) = "TwoListDock("++(intercalate "," (reverse zs)) ++
                        (if (isFirst d) then "<" else ",<") ++ z ++(if (null sd) then ">" else ">,") ++ (intercalate "," ys)++ ")"
        where
          
          aux s
            | null s = []
            | otherwise = show x : aux s'
            where
                (x:s') = s          
          ys = aux sd
          (z:zs) = aux si
            
instance Arbitrary a => Arbitrary (Dock a) where
    arbitrary = do
            xs <- listOf arbitrary
            return (foldr insertl empty xs)
