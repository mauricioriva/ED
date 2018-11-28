--Hecho por :
-- Mauricio Riva Palacio
-- Emiliano Sánchez Pérez


data Figura = Circulo Float | Cuadrado Float | Rectangulo Float Float | Triangulo Float Float  deriving (Show,Eq)

area :: Figura -> Float
area (Circulo n) = pi * n ** 2
area (Triangulo n m) = (n * m)/2

loki :: Int -> Bool -> String
loki t b
 | b && (t < 25 && t > 15) = "Sale a jugar"
 | not b && (t < 30 && t > 20) = "Sale a jugar"
 | otherwise = "No sale a jugar"

suma1 :: [Int] -> [Int]
suma1 m = map(+ 1) m

areaHeron :: Float -> Float -> Float -> Float
areaHeron a b c =
  let s = (a + b + c)/2
  in sqrt(s*(s - a)*(s - b)*(s - c))
