module ExecutarOpcao (
    executarOpcao
    )where

import PrepararJogo

executarOpcao :: Jogadores -> Char -> IO Jogadores
executarOpcao dados '1' = prepararJogo dados
executarOpcao dados '0' = do
                putStrLn ("\nBye!\n")
                return dados
executarOpcao dados _ = do
                putStrLn ("\nOpção inválida! Tente novamente...")
                putStr "\nPressione <Enter> para voltar ao menu..."
                getChar
                menu dados