
--Ejercicio 9

--Mauricio Riva Palacio Orozco
--Jose Eliseo Ortiz Montaño 
--Derek Almanza Infante

module Nat where

  data Nat = Cero | S Nat deriving (Eq, Ord)

  instance Show Nat where
    show Cero = "0"
    show (S n) = "S (" ++ show n ++ ")"


  suma :: Nat -> Nat -> Nat
  suma (S n) Cero = (S n)
  suma a (S n) = suma (S a) n

  multiplica :: Nat -> Nat -> Nat
  multiplica (S n) Cero = Cero
  multiplica a (S n) = suma a (multiplica a n)


  spar :: Nat -> Nat
  spar Cero = Cero
  spar (S n) = (suma (multiplica (S (S Cero)) (S n)) (spar n)) 


  simpar :: Nat -> Nat
  simpar Cero = (S Cero)
  simpar (S n) = (suma (S (multiplica (S (S Cero)) (S n))) (simpar n))
