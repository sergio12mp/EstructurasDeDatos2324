--  ghci -iC:\Users\Sergio\Desktop\ED\Haskell
-- :load haskell\DataStructures\Set\SortedLinearSet.hs 

--Ejercicio 1
module WellBalanced where
import DataStructures.Stack.LinearStack
wellBalanced :: String -> Bool
wellBalanced xs = wellBalanced' xs S.empty
wellBalanced' :: String -> Stack Char -> Bool
wellBalanced' [] s = isEmpty s
wellBalanced' (x:xs) s ...