
-------------------------------Calculadora-------------------------------

calculadora :: String -> (Int,Int) -> Int
calculadora "First" (a,b) = a
calculadora "last" (a,b) = b
calculadora "sum" (a,b) = a + b
calculadora "rest" (a,b) = a - b
calculadora "mul" (a,b) = a * b
calculadora "div" (a,b) = div a b
calculadora "pow" (a,b) = a ^ b
-------------------------------------------------------------------------
suma :: Int -> Int -> Int
suma 0 m = m
suma n 0 = n
suma n m = if (n < 0 && m > 0) then suma(succ n)(pred m)
  else suma(pred n)(succ m)
-------------------------------------------------------------------------
multiplica :: Int -> Int -> Int
multiplica a 0 = 0
multiplica 0 b = b
multiplica a 1 = a
multiplica 1 b = b
multiplica a (-1) = (-a)
multiplica (-1) b = (-b)

multiplica a b = if(a > 0 && b < 0) then suma (multiplica a (-1)) (multiplica a (succ b))
  else if(a < 0 && b < 0) then suma (multiplica a (-1)) (multiplica a (succ b))
    else suma a (multiplica a (b-1))
-------------------------------------------------------------------------
potencia :: Int -> Int -> Int
potencia a 0 = 1
potencia 0 a = 0
potencia a 1 = a

potencia a b = multiplica a (potencia (a) (b-1))
-------------------------------------------------------------------------
resta :: Int -> Int -> Int
resta a 0 = a
resta 0 a = -a
resta a b = resta(pred a)(pred b)
-------------------------------------------------------------------------
divide :: Int -> Int -> Int
divide a 1 = a
divide a 0 = 0
divide 0 a = 0
divide a b = divide(resta a b)(b) + 1
-------------------------------------------------------------------------
raiz :: Int -> Int
raiz 1 = 1
raiz 4 = 2
raiz 9 = 3
raiz 16 = 4
raiz 25 = 5
raiz 36 = 6
raiz 49 = 7
raiz 64 = 8
raiz 81 = 9
raiz 100 = 10
