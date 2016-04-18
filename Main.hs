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
	putStrLn ("NOVO JOGO")

trataOp _ = do
	putStrLn ("A opcao digitava e invalida")
	menu
