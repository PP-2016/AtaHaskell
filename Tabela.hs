--module Tabela(
--    obterNovoTabuleiro
--    )where



--type Tabela tabela = [Char]
--type Vez = Int

--obterNovoTabuleiro :: Tabela -> Vez -> Char -> Tabela
--obterNovoTabuleiro (x:xs) vez e
--            | ((x == e) && (vez == 0)) = (['X'] ++ xs)
--            | ((x == e) && (vez == 1)) = (['O'] ++ xs)
--            | otherwise = x:(obterNovoTabuleiro xs vez e)