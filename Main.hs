
import System.IO
import System.IO.Error
import System.Process
import Data.List
import Data.Function

type Nome = String
type Vez = Int
type Tabela = [Char]

getString :: String -> IO String
getString str = do
	putStr str
	res <- getLine
	return res

menu = do
	putStrLn "-----Bem-Vindo ao jogo da Velha!-----"
	putStrLn "1- Novo Jogo"
	putStrLn "2- Sair"
	putStrLn "Opcao"
	op <- getChar
	getChar
	trataOp(op)

trataOp '2' = do
	putStrLn ("Volte logo! :D")

trataOp '1' = do
	novoJogo

trataOp _ = do
	putStrLn ("A opcao digitava e invalida")
	menu


novoJogo = do
	jogador1 <- getString "\nDigite o nome do primeiro jogador: "
	jogador2 <- getString "\nDigite o nome do segundo jogador: "
	putStrLn ("\nIniciando o jogo ")
	putStrLn ("\nOs quadrados que possuem números NÃO estão marcados.")
	rodarJogo ['1', '2', '3', '4', '5', '6', '7', '8', '9'] jogador1 jogador2 0




rodarJogo :: Tabela -> Nome -> Nome -> Vez -> IO()
rodarJogo tabela jogador1 jogador2 vez = do
	-- imprime o tabuleiro
	putStrLn ("\n" ++ " " ++ (show (tabela !! 0)) ++ " | " ++ (show (tabela !! 1)) ++ " | " ++ (show (tabela !! 2)) ++ "\n ---------------\n" ++ " " ++ (show (tabela !! 3)) ++ " | " ++ (show (tabela !! 4)) ++ " | " ++ (show (tabela !! 5)) ++ "\n ---------------\n" ++ " " ++ (show (tabela !! 6)) ++ " | " ++ (show (tabela !! 7)) ++ " | " ++ (show (tabela !! 8)) ++ "\n")
	if (venceuJogador1 tabela) then do
		putStrLn ("O Jogador " ++ jogador1 ++ "! venceu!!")
		putStr "\nPressione <Enter> para voltar ao menu..."
		getChar
		menu
	else do
-- verifica se o jogador2 venceu
		if (venceuJogador2 tabela) then do
			putStrLn ("O Jogador " ++ jogador2 ++ "! venceu!!")
			putStr "\nPressione <Enter> para voltar ao menu..."
			getChar
			menu
		else do
			-- verifica se houve empate
			-- se o tamanho da intersecção entre "123456789" e "tabela" for 0, então deu empate
			if ((length (intersect "123456789" tabela)) == 0) then do
				putStrLn ("Empate!")
				putStr "\nPressione <Enter> para voltar ao menu..."
				getChar
				menu
			else do
				-- verifica se a vez é do jogador1
				if (vez == 0) then do
					putStr (jogador1 ++ ", é a sua vez! Onde você quer marcar? ")
					op <- getChar
					getChar -- descarta o Enter
					-- testa se a opção é válida
					if not (elem op "123456789") then do
						putStrLn "\nEssa opção NÃO é válida, tente novamente..."
						-- como foi opção inválida, então ainda é a vez do jogador1
						rodarJogo tabela jogador1 jogador2 0
					else
						-- se caiu aqui, então é uma opção válida
						-- testa se a opção já foi marcada
						-- se ela não existir na tabela, é porque já foi marcada
						if not (elem op tabela) then do
							putStrLn "\nEssa opção já foi marcada, escolha outra opção..."
							rodarJogo tabela jogador1 jogador2 0
						else
							-- se caiu aqui é porque a opção é válida e ainda NÃO foi marcada
							-- passa 1 para indicar que a vez é do jogador2
							-- a nova tabela será o retorno da função obterNovoTabuleiro
							rodarJogo  (obterNovoTabuleiro tabela vez op) jogador1 jogador2 1
				else do
					putStr (jogador2 ++ ", é a sua vez! Onde você quer marcar? ")
					op <- getChar
					getChar -- descarta o Enter
					if not (elem op "123456789") then do
						putStrLn "\nEssa opção NÃO é válida, tente novamente..."
						-- como foi opção inválida, então ainda é a vez do jogador2
						rodarJogo  tabela jogador1 jogador2 1
					else
						if not (elem op tabela) then do
							putStrLn "\nEssa opção já foi marcada, escolha outra opção..."
							rodarJogo tabela jogador1 jogador2 1
						else
							-- se caiu aqui é porque a opção é válida e ainda NÃO foi marcada
							-- passa 0 para indicar que a vez é do jogador1
							-- a nova tabela será o retorno da função obterNovoTabuleiro
							rodarJogo (obterNovoTabuleiro tabela vez op) jogador1 jogador2 0
							-- essa função recebe uma lista com a configuração do tabuleiro,
							-- a vez, um elemento (opção escolhida pelo jogador), retorna uma nova configuração (nova lista)
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