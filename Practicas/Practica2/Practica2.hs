--ESTRUCTURAS DISCRETAS 2019-1
--Práctica 2
--Mauricio Riva Palacio Orozco

module Practica2 where

--DEFINICIÓN DE LISTAS

--1.1: Naturales.
-- En este ejercicio se imprimen todos los números naturales hasta detener
-- la ejecución, usando listas de Haskell.
nat = [0..]
-------------------------------------------------------------------------
--1.2: Multiplos de diez.
-- En este ejercicio se imprimen todos los números de 10 en 10
-- hasta detener la ejecución(está comentado también por comprensión).
multiplosDiez = [10,20..]
-- multiplosDiez = [x|x <- nat, mod x 10 == 0]
-------------------------------------------------------------------------
--1.3: potencias de 2.
-- En este ejercicio se imprimen todas las potencias de 2
-- hasta detener su ejecución en una lista por comprensión.
potenciasDos = [2 ^ x|x <- nat]
-------------------------------------------------------------------------
--1.4: Números pares.
-- En este ejercicio se imprimen todos los números pares usando listas
-- predefinidas de Haskell.
pares = [0,2..]
-------------------------------------------------------------------------
--1.5: Años desde el año de tu nacimiento.
-- En este ejercicio se imprimen todos los años desde mi año de nacimiento
-- que es el año 1999, hasta la fecha del 2018.
anosVividos = [1999,2000..2018]

-------------------------------------------------------------------------
-------------------------------------------------------------------------

--DEFINICIÓN DE FUNCIONES

--Ejercicio 2.1:
-- En este ejercicio se calcula cualquier número de la sucesión de fibonacci
-- (El tiempo en calcular el número es proporcional al tamaño del número).
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n-1) + fibonacci(n-2)
-------------------------------------------------------------------------
--Ejercicio 2.2:
-- En este ejercicio el usuario define una lista de elementos y afuera de
-- la lista un elemento y el programa verifica si el elemento que está
-- afuera de la lista pertenece a la lista definida por el usuario.
elemento :: (Eq a) => [a] -> a -> Bool
elemento [] y = False
elemento (x:xs) y = x == y || (elemento xs y)
-------------------------------------------------------------------------
--Ejercicio 2.3:
-- En este ejercicio el ususario ingresa una lista y te devuelve la suma
-- de los elementos de la lista.
sumaLista ::(Num a) => [a] -> a
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista(xs)
-------------------------------------------------------------------------
--Ejercicio 2.4:
-- En este ejercicio el ususario ingresa una lista de números (del 1 al 12)
-- y te devuelve el mes equivalente a ese número.
-- Previamente se define un número con un mes.
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

meses :: [Int] -> [String]
meses [] = []
meses (x:xs) = mes x : meses(xs)
-------------------------------------------------------------------------
--Ejercicio 2.5:
-- Este ejercicio imprime los divisores propios del número, menores
-- que el número (también está la opción por comprensión).
divisoresPropios :: Int -> [Int]
divisoresPropios 0 = [0]
divisoresPropios 1 = [1]
divisoresPropios n = [x | x <- [1..n], x < n, mod n x == 0]
--divisoresPropios n = [x | x <- [1..n-1], mod n x == 0]
-------------------------------------------------------------------------
--Ejercicio 2.6:
-- Este ejercicio te dice si la suma de los divisores propios
-- exceptuando el número, es igual al número.
esPerfecto :: Int -> Bool
esPerfecto n = sumaLista (divisoresPropios n) == n
-------------------------------------------------------------------------
--Ejercicio 2.7:
-- Este ejericio te dice la suma de los divisores propios de
-- uno es igual al otro número y viceversa.
sonAmigos :: Int -> Int -> Bool
sonAmigos a b = (sumaLista (divisoresPropios a) == b) && (sumaLista (divisoresPropios b) == a)
-------------------------------------------------------------------------
--Ejercicio 2.8:
-- Este ejericio te imprime la suma de los digitos de cualquier número.
supersuma :: Int -> Int
supersuma 0 = 0
supersuma n = supersuma (div n 10) + mod n 10
-------------------------------------------------------------------------
--Ejercicio 2.9:
-- Este ejercicio te imprime la escritura de un número en japones (del 0 al 99).
-- Previamente se definieron como se escriben los números en japonés del 0 al 10.
conc :: [a] -> [a] -> [a]
conc [] ys = ys
conc (x:xs) ys = x:(conc xs ys)

japones :: Int -> String
japones 0 = "rei"
japones 1 = "ichi"
japones 2 = "ni"
japones 3 = "san"
japones 4 = "yon"
japones 5 = "go"
japones 6 = "roku"
japones 7 = "nana"
japones 8 = "haci"
japones 9 = "kyu"
japones 10 = "ju"

japones n = if(mod n 10 == 0) then japones(div n 10) `conc` " ju"
            else japones(div n 10) `conc` " ju " `conc` japones(mod n 10)
