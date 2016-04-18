module Menu(
    menu
    )where

menu = do
        putStrLn "-------------------------------- Jogo da Velha --------------------------------"
        putStrLn "Digite 1 para jogar"
        putStrLn "Digite 0 para sair"
        putStr "Opção: "
        op <- getChar
        getChar