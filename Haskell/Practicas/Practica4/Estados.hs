-- Función que construye una lista con todos los nombres de variable
-- que aparecen en una Prop
variables :: Prop -> [String]
variables (Var x) = [x]
variables (Neg p) = variables p
variables (Conj p q) =  variables p `union` variables q
variables (Disy p q) =  variables p `union` variables q
variables (Impl p q) =  variables p `union` variables q
variables (Syss p q) =  variables p `union` variables q
variables p = []

-- Función que quita las tuplas que compartan el primer elemento,
-- dejando únicamente la primera aparición de esta.
quitaRepetidosPar ::(Eq a) => [(a,b)] -> [(a,b)]
quitaRepetidosPar [] = []
quitaRepetidosPar (x:xs) = x:(quitaRepetidosPar $ filter (\y -> fst x /= fst y) xs)

-- Función que calcula todos los posibles estados de una Prop.
estados :: Prop -> [Estado]
estados p = nub -- Elminamos los estados que pudieran estar repetidos.
            -- Ordenamos cada uno de los estados.
            $ map sort
              -- Dejamos una única aparicion de las variables en cada estado.
              $ map quitaRepetidosPar
              -- Calculamos todas las permutaciondes de las
              --combinaciones de las Variables con las constantes lógicas
              $ permutations [(x,y) | x <- (variables p), y <- [Verdadero,Falso]]
