--Funciones Recursivas

--Factorial
factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial(n-1)

--Par
par :: Int -> Bool
par 0 = True
par 1 = False
par n = par(n-2)

--Impar
impar :: Int -> Bool
impar 0 = False
impar 1 = True
impar n = impar(n-2)

--Par(impar)/ par = pim / impar = mip
pim :: Int -> Bool
pim 0 = True
pim n = mip(pred n)

mip :: Int -> Bool
mip 0 = False
mip n = pim(pred n)

--Car (cabeza de lista)
car :: [a] -> a
car [e] = e
car (x:xs) = x
car [] = error "Lista no valida"

--Cdr (cola de la lista)
cdr :: [a] -> [a]
cdr [e] = []
cdr (x:xs) = xs
cdr [] = error "Lista no valida"

--Ultimo

ultimo :: [a] -> a
ultimo [e] = e
ultimo (x:xs) = ultimo(xs)
ultimo [] = error "Lista no valida"

--Toma
toma :: Int -> [a] -> [a]
toma _ [] = []
toma 0 _ = []
toma n(x:xs) = x:(toma(pred n) xs)

--Deja
deja:: Int -> [a] -> [a]
deja 0 k = k
deja n [] = []
deja n (x:xs) = deja(pred n)xs

--Concatenar
conc :: [a] -> [a] -> [a]
conc [] m = m
conc (x:xs) m = x:(conc xs m)

--Merge(mezcla)
merge :: [a] -> [a] -> [a]
merge [] u = u
merge u [] = u
merge (x:xs) (y:ys) = x:y:(merge xs ys)

--Suma Recursiva
suma :: Int -> Int -> Int
suma 0 m = m
suma n m = suma(pred n)(succ m)
