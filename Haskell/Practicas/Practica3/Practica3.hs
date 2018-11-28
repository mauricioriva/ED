module Practica3 where 

 import Binario

--Ejercicio 2.1
 binarios :: [Int] -> [Binario]
 --Tu código va aquí
 binarios l = map natToBin

--Ejercicio 2.2
 par :: Binario -> Bool
 par (Cero _) = True
 par _ = False


 pares :: [Binario] -> [Binario]
 --Tu código va aquí
 pares l = filter par l 

--Ejercicio 2.3
 tooLong :: [String] -> [String]
 --Tu código va aquí
 tooLong l n =  

--Ejercicio 2.4
 sFibonacci :: Int -> [Int]
 --Tu código va aquí
 sFibonacci = error "Borrar esta linea"

--Ejercicio 2.5
 quitaElemento :: (Eq a) => [a] -> a -> [a]
 --Tu código va aquí
 quitaElemeto = error "Borrar esta linea"
