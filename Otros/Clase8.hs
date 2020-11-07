
import Data.Char

ultimo :: [a] -> a
ultimo [n] = n
ultimo (x:xs) = ultimo xs

kesimo :: [a] -> Int -> a
kesimo [] _ = error ""
kesimo (x:xs) 0 = x
kesimo (x:xs) k = kesimo xs (k-1)

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa (xs) ++ [x]

uppercase :: String -> String
uppercase a = map toUpper a

multilength :: [[a]] -> [Int]
multilength xs = map(length)xs

sumasqrt :: Int -> Int
sumasqrt n = sum (map (\ x -> x ^ 2 ) ([0..n]))

replace :: String -> Char -> Char -> String
replace a b c = map (\ x -> if(x == b) then c else x) a

wel :: [[a]] -> [[a]]
wel l = filter (not.null) l

palin :: String -> Bool
palin s = s == reversa s

palindromo :: [String] -> [String]
palindromo a = filter palin a
