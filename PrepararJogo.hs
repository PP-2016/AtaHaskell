module PrepararJogo(
    prepararJogo
    )where

import NovoJogo

prepararJogo :: Jogadores -> IO Jogadores
prepararJogo dados = do
            jogador1 <- getString "\nDigite o nome do primeiro jogador: "
            jogador2 <- getString "\nDigite o nome do segundo jogador: "
            novoJogo dados jogador1 jogador2