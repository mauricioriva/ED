module Prop where

import Data.List

--DEFINICIONES
-- Tipo de dato para representar las expresiones de la lógica proposicional
data Prop = Verdadero
          | Falso
          | Var String
          | Neg Prop
          | Conj Prop Prop
          | Disy Prop Prop
          | Impl Prop Prop
          | Syss Prop Prop
          deriving (Eq,Ord)

-- Sinónimo para representar el estado
type Estado = [(String, Prop)]


--Instancia de Show para el tipo Prop, para que sea legible lo que se imprime en consola
-- NO DEFINE COMPORTAMIENTO
instance Show Prop where
 show Verdadero = "Verdadero"
 show Falso = "Falso"
 show (Var x) = x
 show (Neg p) = "¬ " ++ show p
 show (Conj p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
 show (Disy p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
 show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
 show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

--EJERCICIOS

--EJERCICIO 1
variables :: Prop -> [String]
variables Verdadero = []
variables Falso = []
variables (Var p) = [p]
variables (Neg p) = variables p
variables (Syss p q) = union (variables p) (variables q)
variables (Disy p q) = union (variables p) (variables q)
variables (Conj p q) = union (variables p) (variables q)
variables (Impl p q) = union (variables p) (variables q)

--EJERCICIO 2
quitaRepetidosPar ::(Eq a) => [(a,b)] -> [(a,b)]
quitaRepetidosPar [] = []
quitaRepetidosPar (x:xs) = x : quitaRepetidosPar (filter (\y -> fst x /= fst y) xs)
