
data AE = Var String | Const Int | Op Operacion AE AE deriving (Show)

data Operacion = Suma | Resta | Mult | Div deriving (Show)

type Estado = [(String,AE)]

opera :: Operacion -> (Int -> Int -> Int)
------------------------------ Explicita ---------------------------------------
opera Suma = (\m -> (\n -> m + n))
opera Resta = (\m -> (\n -> m - n))
opera Mult = (\m -> (\n -> m * n))
opera Div = (\m -> (\n -> div m n))
{----------------------------- Implicita ---------------------------------------
  opera Suma = (\a b -> a + b)
  opera Resta = (\a b -> a - b)
  opera Mult = (\a b -> a * b)
  opera Div = (\a b -> div a b)
-}
