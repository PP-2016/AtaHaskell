import RodarJogo

module NovoJogo (
    novoJogo
    )where

novoJogo :: Jogadores -> Nome -> Nome -> IO Jogadores
novoJogo dados jogador1 jogador2 = do
                    putStrLn ("\nIniciando o jogo \"" ++
                            jogador1 ++ " vs " ++ jogador2 ++ "\" ... ")
                    putStrLn ("\nOs quadrados que possuem números NÃO estão marcados.")
                    putStrLn ("\n" ++ jogador1 ++ " será o \'X\' e " ++ jogador2 ++ " será o \'O\'. Vamos lá!!")
                    {-
                        A configuração inicial do tabuleiro é
                        ['1', '2', '3', '4', '5', '6', '7', '8', '9']
                        Numeração da esquerda para direita e de cima para baixo
                        Exemplo:
                                1 | 2 | 3
                               -----------
                                4 | 5 | 6
                               -----------
                                7 | 8 | 9
                    -}
                    -- passa os dados, a configuração inicial, os jogadores e uma flag que indica de quem é
                    -- a vez: 0 quer dizer que a vez é do jogador1 e 1 quer dizer que a vez é do jogador2
                    rodarJogo dados ['1', '2', '3', '4', '5', '6', '7', '8', '9'] jogador1 jogador2 0
