
--Mauricio Riva Palacio Orozco

and1 :: [Bool] -> Bool
and1 [True] = True
and1 (x:xs)
 |(x == False) = False 
 | otherwise = and1 xs

pertenece :: (Eq a) => a -> [a] -> Bool
pertenece n [] = False
pertenece n (x:xs)
 |(n == x) = True
 |otherwise = pertenece n xs

elimDuplicados :: (Eq a) => [a] -> [a]
elimDuplicados [] = []
elimDuplicados (x:xs)
 |(pertenece x xs) = elimDuplicados xs 
 |otherwise = x:elimDuplicados xs

numOcc :: (Eq a) => a -> [a] -> Int
numOcc n [] = 0
numOcc n (x:xs)
 |(n == x) = (1 + numOcc n xs)
 |otherwise = numOcc n xs

n_esimo :: Int -> [a] -> a
n_esimo _ [] = error ""
n_esimo 0 (x:xs) = x
n_esimo n (x:xs) = n_esimo (n-1) xs

