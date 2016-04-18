module Module (obterNovoTabuleiro, venceuJogador1, venceuJogador2) where

type Nome = String
type Vez = Int
type Tabela = [Char]

obterNovoTabuleiro :: Tabela -> Vez -> Char -> Tabela
obterNovoTabuleiro (x:xs) vez e
						| ((x == e) && (vez == 0)) = (['X'] ++ xs)
						| ((x == e) && (vez == 1)) = (['O'] ++ xs)
						| otherwise = x:(obterNovoTabuleiro xs vez e)


venceuJogador1 :: Tabela -> Bool
venceuJogador1 tabela
				| (((tabela !! 0) == 'X') && ((tabela !! 1) == 'X') && ((tabela !! 2) == 'X')) = True
				| (((tabela !! 3) == 'X') && ((tabela !! 4) == 'X') && ((tabela !! 5) == 'X')) = True
				| (((tabela !! 6) == 'X') && ((tabela !! 7) == 'X') && ((tabela !! 8) == 'X')) = True
				-- verifica nas colunas
				| (((tabela !! 0) == 'X') && ((tabela !! 3) == 'X') && ((tabela !! 6) == 'X')) = True
				| (((tabela !! 1) == 'X') && ((tabela !! 4) == 'X') && ((tabela !! 7) == 'X')) = True
				| (((tabela !! 2) == 'X') && ((tabela !! 5) == 'X') && ((tabela !! 8) == 'X')) = True
				-- verifica nas diagonais
				| (((tabela !! 0) == 'X') && ((tabela !! 4) == 'X') && ((tabela !! 8) == 'X')) = True
				| (((tabela !! 2) == 'X') && ((tabela !! 4) == 'X') && ((tabela !! 6) == 'X')) = True
				| otherwise = False


venceuJogador2 :: Tabela -> Bool
venceuJogador2 tabela 
				| (((tabela !! 0) == 'O') && ((tabela !! 1) == 'O') && ((tabela !! 2) == 'O')) = True
				| (((tabela !! 3) == 'O') && ((tabela !! 4) == 'O') && ((tabela !! 5) == 'O')) = True
				| (((tabela !! 6) == 'O') && ((tabela !! 7) == 'O') && ((tabela !! 8) == 'O')) = True
				-- verifica nas colunas
				| (((tabela !! 0) == 'O') && ((tabela !! 3) == 'O') && ((tabela !! 6) == 'O')) = True
				| (((tabela !! 1) == 'O') && ((tabela !! 4) == 'O') && ((tabela !! 7) == 'O')) = True
				| (((tabela !! 2) == 'O') && ((tabela !! 5) == 'O') && ((tabela !! 8) == 'O')) = True
				-- verifica nas diagonais
				| (((tabela !! 0) == 'O') && ((tabela !! 4) == 'O') && ((tabela !! 8) == 'O')) = True
				| (((tabela !! 2) == 'O') && ((tabela !! 4) == 'O') && ((tabela !! 6) == 'O')) = True
				| otherwise = False