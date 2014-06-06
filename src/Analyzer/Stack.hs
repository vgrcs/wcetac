module Analyzer.Stack where

--(Stack, emptyStack, push, pop, top, size,
	      --listToStack, isElement, mapStack, reverseStack,
	      --partitionStack, stackToList) where

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Binary

data Stack a = St [a] deriving (Show, Eq)


listToStack  = St
emptyStack   = St []

push :: Stack a -> a -> Stack a
push (St s) = St . (flip (:)) s

updateSt (St s) = St (nubBy (\a b -> fst a == fst b) s)

mapStack f   = St . map f . stackToList
reverseStack = St . reverse . stackToList
isElement s  = isJust . (flip find) (stackToList s) . (==)
isEmpty      = null   . stackToList
top          = head   . stackToList
size         = length . stackToList

pop :: Stack t -> (t, Stack t)
pop (St (h:t)) = (h, St t)
pop _          = error "Attempt to pop an empty stack."

partitionStack :: (a -> Bool) -> Stack a -> (Stack a, Stack a)

partitionStack f (St s)
    = (\(a1, a2) -> (St a1, St a2)) $ partition f s

appendStack (St st1) (St st2) = St (st1 ++ st2)

-- Helper function:
stackToList :: Stack a -> [a]
stackToList (St s) = s

stackToMap :: (Ord a, Eq b) => Stack (a,b) -> Map.Map a b
stackToMap (St s) = Map.fromList (nubBy (\a b -> fst a == fst b) s)


