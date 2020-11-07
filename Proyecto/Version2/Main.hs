module Main(main) where

  import Data.Emoji
  import System.Random
  import System.IO.Unsafe
  import Peliculas
  import Data.Char

  comparaN :: String -> IO ()
  comparaN n
   | n == "s" = jugar (unsafePerformIO (randomRIO (1,120)))
   | n == "n" = main
   | otherwise = do
      putStrLn "\nElige una opción válida.\n"
      jugarDeNuevo

  jugarDeNuevo :: IO ()
  jugarDeNuevo = do
   putStrLn "¿Quieres jugar otra vez? (s/n) \129300"
   n <- getLine
   comparaN n

  comparaJugar :: String -> String -> String -> IO ()
  comparaJugar n m k
   | n == m = do
     putStrLn "\n\10004\65039\n"
     jugarDeNuevo
   | otherwise = do
     putStrLn "\n\10006\65039\n"
     putStr "Respuesta: "
     putStr k
     putStrLn "\n"
     jugarDeNuevo

  jugar :: Int -> IO ()
  jugar m = do
    let peli = lista_de_peliculas!!m
    putStrLn "\n¿Cuál es el nombre de la película?"
    putStrLn ("\n" ++ (snd peli) ++ "\n")
    let keep = (fst peli)
    let lowerString lqsea = [ toLower loweredString | loweredString <- lqsea]
    respuesta <- getLine
    if respuesta == "" then comparaJugar "hola" "javi" keep else comparaJugar (lowerString(cutWhitespace(fst peli))) (lowerString(cutWhitespace respuesta)) keep

  cutWhitespace :: String -> String
  cutWhitespace str = filter (\y -> (y /=' ' && y /= '.')) str

  imprimir :: String -> IO ()
  imprimir "men" = do
    putStrLn "\nBienvenid@, selecciona una opción:\n[1] Jugar \127922\n[2] Instrucciones \128220\n[3] Salir \9760\n"
  imprimir "ins" = do
    putStrLn "\nInstrucciones\nEste es un juego programado en Haskell, el cual consiste en que te aparecen varios emojis, y tienes que adivinar la pelicula."
    putStrLn "Los titulos de las peliculas están en inglés."
    putStrLn "Hecho por:\n\tJosé Eliseo Ortíz Montaño\n\tMauricio Riva Palacio Orozco\n\tDerek Almanza Infante"
    putStrLn "Presiona intro para continuar"
    esp <- getLine
    main

  compara :: String -> Int -> IO ()
  compara n m
   | n == "1" = jugar m
   | n == "2" = imprimir "ins"
   | n == "3" = putStrLn "\nAdiós \128075\n"
   | otherwise = do
      putStrLn "\nElige una opción válida."
      main

  main :: IO ()
  main = do
    imprimir "men"
    op <- getLine
    compara op (unsafePerformIO (randomRIO (1,120)))
