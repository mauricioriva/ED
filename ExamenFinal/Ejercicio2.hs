
--Mauricio Riva Palacio Orozco

type Persona = String
type Libro = String

type DB = [(Persona,Libro)]

--[("Alicia","Universo"),("Marta","El viaje"),("Susana","La revolucion"),("Marta", "El infinito"),("Daniela","Universo")]

libros :: DB -> Persona -> [Libro]
libros [] _ = []
libros ((p,l):xs) r
 |(r == p) = l:(libros xs r)
 |otherwise = libros xs r

usuariosL :: DB -> Libro -> [Persona]
usuariosL [] _ = []
usuariosL ((p,l):xs) r
 |(r == l) = p:(usuariosL xs r)
 |otherwise = usuariosL xs r

prestado :: DB -> Libro -> Bool
prestado [] _ = False
prestado ((p,l):xs) t
 |(t == l) = True 
 |otherwise = prestado xs t

agregarPrestamo :: DB -> Persona -> Libro -> DB
agregarPrestamo xs p l = xs ++ [(p,l)]

eliminarPrestamo :: DB -> Persona -> Libro -> DB
eliminarPrestamo [] _ _ = []
eliminarPrestamo ((p,l):xs) r t
 |((p == r) && (l == t)) = eliminarPrestamo xs r t
 |otherwise = [(p,l)] ++ eliminarPrestamo xs r t