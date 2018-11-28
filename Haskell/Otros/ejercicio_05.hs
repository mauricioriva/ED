
--Ejercicio 5

--Mauricio Riva Palacio Orozco
--Derek Almanza Infante
--José Eliseo Ortiz Montaño

--Ejercicio 1
data NIL = Suma | Resta | Mult | Div  deriving (Show)

data AE = Var String | Const Int | Op NIL AE AE deriving (Show)

--Ejercicio 2
eval :: AE -> Int
eval (Const n) = n
eval (Op Suma a b) = eval a + eval b
eval (Op Resta a b) = eval a - eval b
eval (Op Mult a b) = eval a * eval b
eval (Op Div a b) = div (eval a) (eval b)

eval (Var m) = error "Tiene que ser constante"

--Ejercicio 3
posNeg :: [Int] -> [String]
posNeg ls = map (\ x -> if x < 0 then "Negativo" else "Positivo") ls
