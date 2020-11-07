module Binario where

 --Ejercicio 1.1
 data Binario = BaseUno | Cero Int | Uno Int deriving (Eq)

 instance Show Binario where
  show BaseUno = "1"
  show (Cero b) = show b ++ "0"
  show (Uno b) = show b ++ "1"

--Ejercicio 1.2
 readB :: String -> Binario
 read 

 natToBin :: Int -> Binario
 natToBin 1 = BaseUno 
 natToBin n 

--Ejercicio 1.3
 binToNat :: Binario -> Int
 --Tu código va aquí
 binToNat BaseUno = 1
 binToNat (Cero b) = 2 * (binToNat b)
 binToNat (Uno b) = (2 * (binToNat b)) + 1 

--Ejercicio 1.4
 sucesor :: Binario -> Binario
 --Tu código va aquí
 sucesor BaseUno = Cero BaseUno
 sucesor (Cero b) = Uno b
 sucesor (Uno b) = Cero (sucesor b)

--Ejercicio 1.5
 bitsEncendidos :: Binario -> Int
 --Tu código va aquí
 bitsEncendidos BaseUno = 1
 bitsEncendidos (Cero b) = bitsEncendidos b
 bitsEncendidos (Uno b) = succ (bitsEncendidos b)
 