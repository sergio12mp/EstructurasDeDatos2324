-- Un Dock es una secuencia de elementos en el que hay uno destacado (sign)
-- Las operaciones disponibles para un dock son: Borrar el elemento destacado,
-- añadir un nuevo elemento delante o detrás del destacado, cambiar el elemento
-- destacado al elemento anterior o al siguiente, saber si la secuencia es vacía
-- o si el destacado es el primero o el último y crear un dock a partir de una lista

module DataStructures.Dock.TwoListsDock (
    empty,      -- :: Dock a
    isEmpty,    -- :: Dock a -> Bool
    sign,       -- :: Dock a -> a
    isFirst,    -- :: Dock a -> Bool
    isLast,     -- :: Dock a -> Bool
    left,       -- :: Dock a -> Dock a
    right,      -- :: Dock a -> Dock a
    delete,     -- :: Dock a -> Dock a
    insertl,    -- :: a -> Dock a -> Dock a
    insertr,    -- :: a -> Dock a -> Dock a
    listToDock  -- :: [a] -> Dock a
) where

import Data.List(intercalate)
import qualified LinearQueue as S
import Test.QuickCheck

-- Vamos a implementar el dock con dos listas:
-- En la primera lista estarán los elementos anteriores al destacado siendo el más cercano
-- al destacado el que está en la cima.
-- En la segunda lista, el destacado será la cima y le seguirán el resto siendo el de la
-- derecha del destacado el que está el siguiente en la lista.
-- INVARIANTES:
-- Siempre hay un elemento destacado (salvo en el dock vacío).
-- Siempre mantendremos el elemento destacado en la cima de la segunda lista.

data Dock a = D (S.Queue a) (S.Queue a) deriving Eq
-- Ejemplo. Si tenemos el dock: 1 2 3 <4> 5 donde el elemento destacado es el 4, se presentará
-- por el dato
-- D S.enqueue 1 $ S.enqueue 2 $ S.enqueue 3 S.empty S.enqueue 5 $ S.enqueue 4 S.empty
-- Su show será TwoListsDock(1,2,3,<4>,5)
-- y las filas se verán así:
-- 3
-- 2 4 <- El destacado siempre en la cima de la segunda fila
-- 1 5

-- NOTA: Todas las operaciones con filas están cualificadas con S: S.Queue, S.enqueue, S.dequeue, etc

sample1 = D (S.enqueue 1 $ S.enqueue 2 $ S.enqueue 3 S.empty) (S.enqueue 5 $ S.enqueue 4 S.empty)
sampleV = D (S.empty) (S.empty)
sampleP = D (S.empty) (S.enqueue 3 $ S.enqueue 2 $ S.enqueue 1 S.empty)
sampleU = D (S.enqueue 1 $ S.enqueue 2 $ S.enqueue 3 S.empty) (S.enqueue 4 S.empty)
sampleD = D (S.empty) (S.enqueue 1 S.empty)

-- =================================================================================================
-- Crea un dock vacío
empty :: Dock a
empty = D S.empty S.empty

{-
Prelude (Dock.hs)> let dockVacio = empty
Prelude (Dock.hs)> show dockVacio
"TwoStackDock()"
-}

-- =================================================================================================
-- Determina si un dock está vacío
isEmpty :: Dock a -> Bool
isEmpty (D xs ys) = (S.isEmpty xs) && (S.isEmpty ys)

{-
Prelude (Dock.hs)> isEmpty sample1
False
Prelude (Dock.hs)> isEmpty sampleV
True
Prelude (Dock.hs)> isEmpty sampleP
False
-}

-- =================================================================================================
-- Devuelve el elemento destacado. Error si está vacío
sign :: Dock a -> a
sign (D xs ys)
    | (S.isEmpty xs) && (S.isEmpty ys) = error "Dock vacío"
    | otherwise                        = S.first ys

{-
Prelude (Dock.hs)> sign sample1
4
Prelude (Dock.hs)> sign sampleV
*** Exception: Dock vacío
Prelude (Dock.hs)> sign sampleP
1
-}

-- =================================================================================================
-- Devuelve cierto si el destacado es el primero (o está vacío)
isFirst :: Dock a -> Bool
isFirst (D xs ys)
    | (S.isEmpty xs) && (S.isEmpty ys)    = True
    | (S.isEmpty xs) && not(S.isEmpty ys) = True
    | otherwise                           = False

{-
Prelude (Dock.hs)> isFirst sample1
False
Prelude (Dock.hs)> isFirst sampleV
True
Prelude (Dock.hs)> isFirst sampleP
True
-}

-- =================================================================================================
-- Devuelve cierto si el destacado es el último (o está vacío)
isLast :: Dock a -> Bool
isLast (D xs ys)
    | (S.isEmpty xs) && (S.isEmpty ys)              = True
    | not(S.isEmpty xs) && S.isEmpty (S.dequeue ys) = True      -- Queda vacío al hacer dequeue, por tanto había un elemeto
    | otherwise                                     = False


-- 3
-- 2
-- 1 4 <- Es el último

{-
Prelude (Dock.hs)> isFirst sample1
False
Prelude (Dock.hs)> isFirst sampleV
True
Prelude (Dock.hs)> isFirst sampleU
True
-}

-- =================================================================================================
-- Cambia el elemento destacado que pasa a ser el de la izquierda.
-- Si está vacío o el destacado es el primero lo deja igual
left :: Dock a -> Dock a
left d@(D xs ys)
    | isFirst d || isEmpty d = d
    | otherwise = D (S.dequeue xs) (S.enqueueFront (S.first xs) ys)

-- 3 <- Nuevo destacado    3
-- 2 4                   2 4
-- 1 5                   1 5

{-
Prelude (Dock.hs)> left sampleV
TwoStackDock()

Prelude (Dock.hs)> show sampleP
"TwoStackDock(<1>,2,3)"
Prelude (Dock.hs)> left sampleP
TwoStackDock(<1>,2,3)

Prelude (Dock.hs)> show sample1
"TwoStackDock(1,2,3,<4>,5)"
Prelude (Dock.hs)> left sample1
TwoStackDock(1,2,<3>,4,5)
Prelude (Dock.hs)> left $ left sample1
TwoStackDock(1,<2>,3,4,5)
Prelude (Dock.hs)> left $ left $ left sample1
TwoStackDock(<1>,2,3,4,5)
Prelude (Dock.hs)> left $ left $ left $ left sample1
TwoStackDock(<1>,2,3,4,5)
Prelude (Dock.hs)> isFirst $ left $ left $ left $ left sample1
True
-}

-- =================================================================================================
-- El elemento destacado pasa a ser el de la derecha.
-- Si está vacío o el destacado es el último lo deja igual
right :: Dock a -> Dock a
right d@(D xs ys)
    | isLast d || isEmpty d = d
    | otherwise = D (S.enqueueFront (S.first ys) xs) (S.dequeue ys)

--                           4
-- 3                         3
-- 2 4                       2
-- 1 5 <- Nuevo destacado    1 5

{-
Prelude (Dock.hs)> right sampleV
TwoStackDock()

Prelude (Dock.hs)> show sampleU
"TwoStackDock(1,2,3,<4>)"
Prelude (Dock.hs)> right sampleU
TwoStackDock(1,2,3,<4>)

Prelude (Dock.hs)> show sample1
"TwoStackDock(1,2,3,<4>,5)"
Prelude (Dock.hs)> right sample1
TwoStackDock(1,2,3,4,<5>)
Prelude (Dock.hs)> right $ right sample1
TwoStackDock(1,2,3,4,<5>)
Prelude (Dock.hs)> isLast $ right sample1
True
-}

-- =================================================================================================
-- Elimina el objeto destacado. El destacado pasa a ser el siguiente.
-- Si no hay soguiente pasa a ser el anterior.
-- Si queda vacío no hay destacado.
-- Error si está vacío
delete :: Dock a -> Dock a
delete d@(D xs ys)
    | isEmpty d = error "Dock vacío"
    | not(S.isEmpty xs) && (S.isEmpty (S.dequeue ys)) = D (S.dequeue xs) (S.enqueue (S.first xs) (S.dequeue ys))
    | otherwise = D xs (S.dequeue ys)

-- 3                     3
-- 2 4 <- Borramos       2 
-- 1 5                   1 5 <- Nuevo destacado

{-
Prelude (Dock.hs)> delete sample1
TwoStackDock (1,2,3, <5>)
Prelude (Dock.hs)> delete $ delete sample1
TwoStackDock (1,2, <3>)
Prelude (Dock.hs)> delete $ delete $ delete sample1
TwoStackDock (1, <2>)
Prelude (Dock.hs)> delete $ delete $ delete $ delete sample1
TwoStackDock(<1>)
Prelude (Dock.hs)> delete $ delete $ delete $ delete $ delete sample1
TwoStackDock()
-}

-- =================================================================================================
-- Inserta el elemento a la izquierda del destacado y este elemento pasa a ser el destacado
insertl :: a -> Dock a -> Dock a
insertl e (D xs ys) = D xs ((S.enqueueFront e) ys)

-- (1,2,3,<4>,5) > (1,2,3,<e>,4,5)
-- 3 e <- Nuevo destacado
-- 2 4
-- 1 5

{-
Prelude (Dock.hs)> insertl 7 sample1
TwoStackDock(1,2,3,<7>,4,5)
-}

-- =================================================================================================
-- Inserta el elemento a la derecha del destacado y este elemento pasa a ser el destacado
insertr :: a -> Dock a -> Dock a
insertr e (D xs ys) = D (S.enqueueFront (S.first ys) xs) ((S.enqueueFront e) (S.dequeue ys))

-- (1,2,3,<4>,5) > (1,2,3,4,<e>,5)
-- 4
-- 3 
-- 2 e <- Nuevo destacado
-- 1 5

{-
Prelude (Dock.hs)> insertl 7 sample1
TwoStackDock(1,2,3,4,<7>,5)
-}

-- =================================================================================================
-- Genera un dock con los elementos de la lista. El destacado será el primero de la lista
listToDock :: [a] -> Dock a
listToDock = foldr insertl empty

-- [1,2,3,4,5]
--   1 <- Destacado
--   2
--   3
--   4
-- _ 5

{-
Prelude (Dock.hs)> listToDock [1..5]
TwoStackDock(<1>,2,3,4,5)
-}

-- *************************************************************************************************
instance (Show a) => Show (Dock a) where
    show d@(D si sd) | isEmpty d = "TwoStackDock()"
    show (D si sd) = "TwoStackDock("++(intercalate "," (reverse (aux si))) ++ (if ei then "<" else ",<") ++ y ++ (if ed then ">" else ">,") ++ (intercalate "," ys) ++ ")"
        where
            aux s
                    | S.isEmpty s = []
                    | otherwise = show x : aux s'
                where
                    x = S.first s
                    s' = S.dequeue s
            (y:ys) = aux sd
            ei = S.isEmpty si
            ed = S.isEmpty (S.dequeue sd)

instance Arbitrary a => Arbitrary (Dock a) where
    arbitrary = do
        xs <- listOf arbitrary
        return (foldr insertl empty xs)