compara :: Int -> Int -> String
compara x y
 | x < y = "LT"
 | x > y = "GT"
 | x == y = "EQ"

--iniciales :: String -> String -> String
--iniciales nom ap = n ++ ". " ++ a
--                where n = head nom
--                      a = head ap

palindromo :: String -> Bool
palindromo str =
    let rts = reverse str
    in str == rts

data Figura = Circulo Float
 | Cuadrado Float
 | Rectangulo Float Float
 | Triangulo Float Float deriving (Show)

data Lista a = Empty | Cons
