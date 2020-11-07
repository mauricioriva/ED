--ESTRUCTURAS DISCRETAS 2019-1
--Práctica 1
--Mauricio Riva Palacio Orozco

--Ejercicio 1.1:
areaCirculo :: Float -> Float
areaCirculo r = (pi*r*r)

--Ejercicio 1.2:
distancia :: (Float , Float ) -> (Float , Float ) -> Float
distancia (m,n) (p,q) = sqrt (((p - m)** 2) + ((q - n)**2))

--Ejercicio 1.3:
imp :: Bool -> Bool -> Bool
imp a b = a && b

--Ejercicio 1.4:
xor :: Bool -> Bool -> Bool
xor e r = e || r

--Ejercicio 1.5:
mes :: Int -> String
mes 1 = "Enero"
mes 2 = "Febrero"
mes 3 = "Marzo"
mes 4 = "Abril"
mes 5 = "Mayo"
mes 6 = "Junio"
mes 7 = "Julio"
mes 8 = "Agosto"
mes 9 = "Septiembre"
mes 10 = "Octubre"
mes 11 = "Noviembre"
mes 12 = "Diciembre"

--Ejercicio 1.6:
calculadora :: String -> (Int,Int) -> Int
calculadora "First" (a,b) = a
calculadora "last" (a,b) = b
calculadora "sum" (a,b) = a + b
calculadora "rest" (a,b) = a - b
calculadora "mul" (a,b) = a * b
calculadora "div" (a,b) = div a b
calculadora "pow" (a,b) = a ^ b

--Ejercicio 1.7:
loki :: Int -> Bool -> String
loki n b = if (b==False) && (n>15) && (n<25) then "Sale a jugar" else
  if (b==True) && (n>20) && (n<30) then "Sale a jugar" else "No sale a jugar"

--Ejercicio 1.8:
monos :: Bool -> Bool -> String
monos s t = if not(s && t)==True then "No hay problemas" else "Hay problemas"

--Ejercicio 1.9:
suma :: Int -> Int -> Int
suma 0 m = m
suma n m = suma(n - 1)(m + 1)

multiplica :: Int -> Int -> Int
multiplica a 0 = 0
multiplica a 1 = a

multiplica a b = suma a (multiplica a (b-1))

--Ejercicio 1.10:
potencia :: Int -> Int -> Int
potencia a 0 = 1
potencia 0 a = 0
potencia a 1 = a

potencia a b = multiplica a (potencia (a) (b-1))
