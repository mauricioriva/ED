module Main(main) where

  main :: IO ()
  main = do
    putStrLn "\127829" --Imprimimos un Emoji
    let lst = [0..] -- Declaramos una lista de Naturales
    putStrLn "¿Cuántos naturales quieres que imprima?" -- Imprimimos la cadena
    count <- getLine -- Esperamos la respuesta del usuario
    let num = read count :: Int -- Hacemos una conversión de tipos,
    -- la entrada es de tipo String y queremos manejarla como un Int (String -> Int)
    putStrLn $ show $ take num lst -- Imprimimos de nuevo en consola, pero como
    -- putStrLn espera una cadena,, convertimos con la función show la lista a cadena
    putStrLn "¿Otra vez? (s/n)"
    answer <- getLine
    if (answer == "s") then main else putStrLn "Ciao" --Si el usuario dice que quiere
    -- seguir en el programa, llamamos recursivamente a la función main para que se ejecute de nuevo
