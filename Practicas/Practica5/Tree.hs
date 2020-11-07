module Tree where

 -- Tipo de dato Algebraico para definir Árboles Binarios
 data BinaryTree a = Void
                   | Node (BinaryTree a) a (BinaryTree a)
                   deriving (Eq,Ord,Show)

 --Ejercicio 2.1
 addTree :: (Ord a) => BinaryTree a -> a -> BinaryTree a
 addTree = error "Falta Implementar"

 --Ejercicio 2.2
 inorder :: BinaryTree a -> [a]
 inorder = error "Falta Implementar"

 --Ejercicio 2.3
 preorder :: BinaryTree a -> [a]
 preorder = error "Falta Implementar"

 --Ejercicio 2.4
 postorder :: BinaryTree a -> [a]
 postorder = error "Falta Implementar"

 --Ejercicio 2.5
 maximo :: (Ord a) => BinaryTree a -> a
 maximo = error "Falta Implementar"

 --Ejercicio 2.6
 minimo :: (Ord a) => BinaryTree a -> a
 minimo = error "Falta Implementar"

 --Ejercicio 2.7
 busca :: (Ord a) => a -> BinaryTree a -> Bool
 busca = error "Falta Implementar"
