{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances, TypeSynonymInstances #-}
module AMPL.AMPL
            (Set, Param, Param_, LPVal (..), Solution, Solution_, RowSolution, RowSolution_,
             Expr (..),   -- ^ types
             Program(..), Model, Direction,
             (%<=), (%==), (%>=), (@<=),       -- ^ constraint relations
             AbsOrd (..),
             var, var_, var__, param, param_, solution_, row_, -- ^ initializers
             maximize, minimize, subject_to, -- ^ primitives
             runModel, toGLPK, objectiveToGLPK
            ) where
import Data.Char(isAlpha)
import Data.Ratio
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State

import Data.LinearProgram hiding (direction, objective, constraints, Var, Direction, var)

infixr 4 %<=, %>=, %==, @<=

-- sets are represented by lists
type Set a = [a]

-- symbols for variables
type Sym = String

data LPVal = Val  Int
           | Sol  Double
           | NonComp
           | Pair (LPVal, LPVal) deriving (Eq)

instance Show LPVal where
  show (Val r) = show r
  show (Sol r) = "(s=" ++ show r ++")"
  show (Pair r) = "(p=" ++ show r ++")"

--instance Fractional LPVal where
--  (Val a)/(Val b) = Val (a/b)
--  fromRational x = Val x

instance Ord LPVal where
  compare (Val a) (Val b) = compare a b

instance Num LPVal where
  (Val x) + (Val y) = Val (x+y)
  (Val x) + (Sol y) = Sol (fromIntegral x+y)
  (Sol x) + (Sol y) = Sol (x+y)
  (Val x) - (Val y) = Val (x-y)
  (Val x) * (Val y) = Val (x*y)
  (Sol x) * (Sol y) = Sol (x*y)
  abs (Val x) = Val (abs x)
  signum (Val x) = Val (signum x)
  fromInteger x = Val (fromInteger x)

class AbsOrd a where
  (%<<) :: a -> a -> Bool
  (%>>) :: a -> a -> Bool

instance AbsOrd LPVal where
  (Val x) %<< (Pair (y,z)) =  (Val x) == z
  _ %<< _ = False
  (Val x) %>> (Pair (y,z)) =  (Val x) == y
  _ %>> _ = False


 -- scalar parameters
--type Param = Expr Rational
type Param = Expr LPVal

type Solution = Double
type RowSolution = [(Int, Solution)]


type GLPKParam = Expr Double

objectiveToGLPK
  :: [(Int, String)]
  -> Expr LPVal
  -> [(Int, String)]

objectiveToGLPK accum (App "*" [(Con (Val x)), (Var y)])
  =  accum ++ (x, y):[]

objectiveToGLPK accum (App "*" [x, y])
  =  objectiveToGLPK (objectiveToGLPK accum x) y

objectiveToGLPK accum (App "+" [x, y])
  =  objectiveToGLPK (objectiveToGLPK accum x) y


equationToGLPK
  :: [(Int, String)]
  -> Expr LPVal
  -> [(Int, String)]


equationToGLPK accum (Var e)
  = accum ++ (1, e):[]

equationToGLPK [] (App "==" [Var n, Con (Val 0)])
  = []

equationToGLPK [] (App "==" [Var n, es])
  = let edges = equationToGLPK [] es
    in edges ++ (-1, n):[]

equationToGLPK accum (App "+" [x, y])
  = equationToGLPK (equationToGLPK accum x) y


boundsToGLPK
  :: [(String, Int)]
  -> Expr LPVal
  -> [(String, Int)]

boundsToGLPK accum (App "<=" [Var x, Con (Val y)])
  = (x,y):[]


toGLPK
  :: (Expr LPVal)
  -> [Expr LPVal]
  -> [Expr LPVal]
  -> LPM String Int ()

toGLPK objective equations bounds
  = do
    let wcet = foldl objectiveToGLPK [] (objective:[])

        eqCombinations = filter (\es -> es /=[]) $ map (equationToGLPK []) equations
        eqGLPK lc = equalTo (linCombination lc) 0
        (e:eqConstrainsts) = map eqGLPK eqCombinations

        bsCombinations = concat $ map (boundsToGLPK []) bounds
        bsGLPK (var, bound) = leqTo (varSum [var]) bound
        (b:bsConstrainsts) = map bsGLPK bsCombinations

    --error (show wcet)
    --error (constraints_eqs2 "" eqCombinations)

    setDirection Max
    setObjective $ linCombination wcet
    equalTo (linCombination [(1,"d0")]) 1
    foldl (>>) e eqConstrainsts
    foldl (>>) b bsConstrainsts

    --error (show eqCombinations)
    --(leqTo (varSum ["dx"]) 3)


constraints_eqs2 s a
  = foldl (\cstr lc -> cstr ++ (" equalTo " ++ (show lc) ++ " " ++ show 0 ++"\n")) s a

--toGLPK (App "+" [x,y]) = error $ "X= " ++ show x ++ " Y= " ++ show y
--toGLPK other = error $ "Expr= " ++ show other

instance AbsOrd Param where
  (Con x) %<< (Con y) = x %<< y
  _ %<< _ = False
  (Con x) %>> (Con y) = x %>> y
  _ %>> _ = False

 -- indexed parameters
type Param_ i = i -> Param

 -- indexed solutions
type Solution_ i = i -> Solution
type RowSolution_ i = i -> RowSolution

 -- symbolic expression, parameterized by type of constants
data Expr t = Con t
            | Var Sym
            | App PrimOp [Expr t]
            -- deriving (Eq)

instance (Eq t) => Eq (Expr t) where
   Con a == Con b = a == b
   Var a == Var b = a == b
   App a b == App c d = a == c && b == d

type PrimOp = String  -- ^ primive ops represented by strings

instance (Eq t) => Ord (Expr t) where
     compare (Var a) (Var b) = compare a b

instance Functor Expr where
     fmap f (Con x) = Con (f x)
     fmap f (Var v) = Var v
     fmap f (App v es) = App v (map (fmap f) es)


--instance Num t => Num (Expr t) where
instance (Num t, Eq t) => Num (Expr t) where
     x+y = binOp (+) x "+" y
     x-y = binOp (-) x "-" y
     x*y = binOp (*) x "*" y
     negate = unOp  negate "negate"
     signum = unOp  signum "signum"
     abs    = unOp  abs "abs"
     fromInteger n = Con (fromInteger n)

instance (Fractional t, Eq t) => Fractional (Expr t) where
     x/y            = binOp (/) x "/" y
     fromRational x = Con  (fromRational x)

instance Show t => Show (Expr t) where
     --showsPrec p (Con c) = (("con=" ++ showsPrec p c "" ++ ")")++)
     showsPrec p (Con c) = showsPrec p c
     showsPrec _ (Var x) = showString x
     showsPrec p (App op@(c:_) [x, y]) | not (isAlpha c) =
         showParen (p>q) (showsPrec ql x . showString op . showsPrec qr y)
         where (ql, q, qr) = Map.findWithDefault (9,9,9) op precs
               precs = Map.fromList [("**", (9,8,8)),
                                        ("/",  (7,7,8)),
                                        ("*",  (7,7,8)),
                                        ("+",  (6,6,7)),
                                        ("-",  (6,6,7)),
                                        ("<=", (4,4,5)),
                                        ("==", (4,4,5))]
     showsPrec p (App "negate" [x]) =
         showParen (p>=6) (showString "-" . showsPrec 7 x)
     showsPrec p (App f xs) =
         showParen (p>10) (foldl (.) (showString f) (map (\ x -> showChar ' ' . showsPrec 11 x) xs))

-- Assume the numbers are a field and simplify a little
binOp :: (Eq a, Num a) => (a->a->a) -> Expr a -> String -> Expr a -> Expr a
--binOp f (Con x) _ (Con y) = Con (f x y)
binOp _ x "+" (Con 0) = x
binOp _ (Con 0) "+" x = x
binOp _ x "+" (App "+" [y, z]) = (x + y) + z
binOp _ x "+" y | isCon y && not (isCon x) = y + x
binOp _ x "+" (App "negate" [y]) = x - y
binOp _ x "-" 0 = x
binOp _ x "-" (Con y) | not (isCon x) = Con (-y) + x
binOp _ x "-" x' | x == x' = 0
binOp _ _ "*" 0 = 0
binOp _ x "*" 1 = x
binOp _ x "*" (-1) = -x
binOp _ 0 "*" _ = 0
binOp _ 1 "*" x = x
binOp _ (-1) "*" x = -x
binOp _ x "*" (App "*"  [y, z]) = (x * y) * z
binOp _ x "*" y | isCon y && not (isCon x) = y * x
binOp _ x "*" (App "/" [y, z]) = App "/" [x*y, z]
binOp _ x "/" 1 = x
binOp _ x "/" (-1) = -x
binOp _ x "/" x' | x == x' = 1
binOp _ x "/" (App "/" [y, z]) = App "/"  [x*z, y]
binOp _ x op y = App op [x, y]
binOp f (Con x) _ (Con y) = Con (f x y)

unOp :: (Num a) => (a->a) -> String -> Expr a -> Expr a
unOp f _ (Con c) = Con (f c)
unOp _ "negate" (App "negate" [x]) = x
unOp _ "abs" e@(App "abs"  _) = e
unOp _ "signum" e@(App "signum"  _) = e

unOp f op x = App op [x]

isCon :: Expr a -> Bool
isCon (Con _) = True
isCon _ = False


(%==) :: (Eq t, Num t) => Expr t -> Expr t -> Expr t
x %== y = binOp undefined x "==" y
x %<= y = binOp undefined x  "<=" y
(%>=)   = flip (%<=)
x @<= y = binOp undefined x  "@<=" y

var :: Sym -> Model t (Expr t)
var x = do modify $ \p -> p {variables = variables p ++ [(x,"")]}
           return (Var x)

var_ :: (Show k, Ord k) => Sym -> Set k -> Model t (k -> Expr t)
var_ v ixs
    = do modify $ \p -> p {variables = variables p ++ zip vs (map show ixs)}
         return (\i -> Map.findWithDefault err i tvs)
  where tvs = Map.fromList $ zip ixs (map Var vs)
        vs = [v++show i | i<-[0..]]
        err = error $ "AMPL.var "++v++": index out of bounds"

var__ :: (Show k, Ord k) => Sym -> Set k -> Model t (k -> Expr t)
var__ v ixs
    = do modify $ \p -> p {variables = variables p ++ zip vs (map show ixs)}
         return (\i -> Map.findWithDefault err i tvs)
  where tvs = Map.fromList $ zip ixs (map Var vs)
        vs = [v++show i | i<-[-1..]]
        err = error $ "AMPL.var "++v++": index out of bounds"

--param :: Rational -> Param
param :: LPVal -> Param
param = Con

--param_ :: (Ord k) => [(k,Rational)] -> Param_ k
param_ :: (Ord k, Show k) => [(k,LPVal)] -> Param_ k
param_ assocs k
    = Con (Map.findWithDefault err k m)
    where m = Map.fromList assocs
          err = error $ "AMPL.param: index out of bounds " ++ show k

solution_ :: (Ord k) => [(k, Double)] -> Solution_ k
solution_ assocs k
    = Map.findWithDefault err k m
    where m = Map.fromList assocs
          err = error "AMPL.param: index out of bounds"

row_ :: (Ord k) => [(k, RowSolution)] -> RowSolution_ k
row_ assocs k
    = Map.findWithDefault err k m
    where m = Map.fromList assocs
          err = error "AMPL.param: index out of bounds"

 -- a monad for modeling mathematical programs
type Model t = State (Program t)

 -- a linear/non-linear program
data Program t
     = Program { direction :: Direction
               , objective :: (Expr t, String)
               , variables :: [(Sym, String)]
               , constraints :: [(Expr t, String)]
               } deriving (Eq,Show)

data Direction = Maximize | Minimize deriving (Eq,Show,Read)


instance Functor Program where
     fmap f p = Program { direction = direction p
                        , objective = (fmap f (fst $ objective p), snd $ objective p)
                        , variables = variables p
                        , constraints =
                            [(fmap f c, n) | (c,n)<-constraints p]
                        }


-- initial empty program
--initialProg :: Program Rational
initialProg :: Program LPVal
initialProg = Program Maximize (Con (Val 0),"") [] []

maximize, minimize :: String -> Expr a -> Model a ()
maximize comm f = modify $ \p -> p {direction=Maximize,objective=(f,comm)}
minimize comm f = modify $ \p -> p {direction=Minimize,objective=(f,comm)}

-- type class to overload single or multiple constraints
class HasConstraints t c where
    getConstraints :: c -> [Expr t]

instance HasConstraints t (Expr t) where
    getConstraints e = [e]

instance HasConstraints t c => HasConstraints t [c] where
    getConstraints = concatMap getConstraints


subject_to :: HasConstraints t c => String -> c -> Model t ()
subject_to tag constr
  = modify $
    \p -> let cs = [(c, tag) | c<-getConstraints constr]
          in  p {constraints=constraints p ++ cs}

--runModel :: Model Rational a -> Program Rational
runModel :: Model LPVal a -> Program LPVal
runModel m = snd $ runState m initialProg
