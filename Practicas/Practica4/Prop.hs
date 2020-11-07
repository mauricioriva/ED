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
  show Verdadero = "V" -- V
  show Falso = "F" -- F
  show (Var x) = x -- P 
  show (Neg p) = "¬ " ++ show p -- ¬ P
  show (Conj p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")" -- (P ∧ Q)
  show (Disy p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")" -- (P ∨ Q)
  show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")" -- (P → Q)
  show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")" -- (P ↔ Q)


-- EQUIVALENCIAS LÓGICAS 

 -- Ejercicio 1.1
 eliminacion :: Prop -> Prop
 eliminacion (Var p) = (Var p)
 eliminacion Verdadero = Verdadero
 eliminacion Falso = Falso
 eliminacion (Neg (Neg p)) = eliminacion p
 eliminacion (Neg p) = (Neg (eliminacion p))
 eliminacion (Conj p q) = Conj (eliminacion p) (eliminacion q)
 eliminacion (Disy p q) = Disy (eliminacion p) (eliminacion q)
 eliminacion (Impl p q) = Disy (Neg (eliminacion p)) (eliminacion q)
 eliminacion (Syss p q) = Conj (eliminacion(Impl p q)) (eliminacion(Impl q p))

 -- Ejercicio 1.2
 deMorgan :: Prop -> Prop
 deMorgan (Var p) = (Var p)
 deMorgan (Neg (Conj p q)) = (Disy (Neg p) (Neg q))
 deMorgan (Neg (Disy p q)) = (Conj (Neg p) (Neg q))
 deMorgan (Conj p q) = (Conj p q)
 deMorgan (Disy p q) = (Disy p q)
 deMorgan (Impl p q) = (Impl (deMorgan p) (deMorgan q))
 deMorgan (Syss p q) = (Syss (deMorgan p) (deMorgan q))

-- EVALUACIÓN Y ANÁLISIS SINTÁCTICO DE EXPRESIONES

 -- Ejercicio 2.1 
 fvar :: Prop -> Estado -> Prop
 fvar (Var p) [] = error "Ya tengo sueño"
 fvar (Var s) (x:xs)
  | s == fst(x) = snd(x)
  | otherwise = fvar (Var s) xs



 interp :: Prop -> Estado -> Bool
 interp (Neg p) ys = not(interp p ys)
 interp (Verdadero) ys = True
 interp (Falso) ys = False
 interp (Var p) ys = interp (fvar (Var p) ys) ys
 interp (Impl p q) ys
  | (interp p ys) == False = True
  | otherwise = (interp (p) (ys)) && (interp (q) (ys))
 interp (Conj p q) ys = (interp p ys) && (interp q ys)
 interp (Disy p q) ys = (interp p ys) || (interp q ys)
 interp (Syss p q) ys
  | ((not(interp p ys)) && (not(interp q ys))) || ((interp p ys) && (interp q ys)) = True
  | otherwise = False

 -- Ejercicio 2.2

 variables :: Prop -> [String]
 variables (Var x) = [x]
 variables (Neg p) = variables p
 variables (Conj p q) =  variables p `union` variables q
 variables (Disy p q) =  variables p `union` variables q
 variables (Impl p q) =  variables p `union` variables q
 variables (Syss p q) =  variables p `union` variables q
 variables p = []



 quitaRepetidosPar ::(Eq a) => [(a,b)] -> [(a,b)]
 quitaRepetidosPar [] = []
 quitaRepetidosPar (x:xs) = x:(quitaRepetidosPar $ filter (\y -> fst x /= fst y) xs)


 estados :: Prop -> [Estado]
 estados p = nub -- Elminamos los estados que pudieran estar repetidos.
            -- Ordenamos cada uno de los estados.
            $ map sort
              -- Dejamos una única aparicion de las variables en cada estado.
              $ map quitaRepetidosPar
              -- Calculamos todas las permutaciondes de las
              --combinaciones de las Variables con las constantes lógicas
              $ permutations [(x,y) | x <- (variables p), y <- [Verdadero,Falso]]


 truthTable :: Prop -> String
 truthTable p
  | and(map (\x -> interp p x) (estados p)) = "Tautologia"
  | not(or(map (\y -> interp p y) (estados p))) = "Contradiccion"
  | otherwise = "Contingencia"
  

 -- Ejercicio 2.3  
 unir :: [Prop] -> Prop
 unir [] = Verdadero
 unir (x:xs) = Conj x (unir xs)


 correcto :: [Prop] -> Prop -> Bool 
 correcto p q
  | truthTable( Impl (unir p) q ) == "Tautologia" = True
  | truthTable( Impl (unir p) q ) == "Contradiccion" = False
  | otherwise = False
