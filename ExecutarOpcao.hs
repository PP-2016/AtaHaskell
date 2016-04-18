module ExecutarOpcao (
    executarOpcao
    )where

--import PrepararJogo
executarOpcao :: Char -> Char
executarOpcao dados '1' = do
                --prepararJogo dados
                putStrLn("Olá")
executarOpcao dados '0' = do
                putStrLn ("\nBye!\n")
                return dados
executarOpcao dados _ = do
                putStrLn ("\nOpção inválida! Tente novamente...")
                putStr "\nPressione <Enter> para voltar ao menu..."
                getChar
                menu 