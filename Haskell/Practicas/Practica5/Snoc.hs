module Snoc where

 --Tipo de dato algebraico para definir listas Snoc
 data SnocList a = Mt
                 | Snoc (SnocList a) a
                 deriving (Eq, Ord, Show)

 --Ejercicio1.1
 addSnoc :: SnocList a -> a -> SnocList a
 addSnoc = error "Falta Implementar"

 --Ejercicio1.2
 ultimo :: SnocList a -> a
 ultimo = error "Falta Implementar"

 --Ejercicio1.3
 resto :: SnocList a -> SnocList a
 resto = error "Falta Implementar"

 --Ejercicio1.4
 cabeza :: SnocList a -> a
 cabeza = error "Falta Implementar"

 --Ejercicio1.5
 cola :: SnocList a -> SnocList a
 cola = error "Falta Implementar"

 --Ejercicio1.6
 longitud :: SnocList a -> Int
 longitud = error "Falta Implementar"
