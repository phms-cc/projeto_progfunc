{-# LANGUAGE BlockArguments #-}
import Checkers(Game, getGameMoves, makeMove, Move, getDefaultGame, gameOver,
 gameWinner)

--Controla o fluxo do jogo
playGame :: Game -> IO()
playGame game = if not (gameOver game)
   then continueGame game
   else finishGame game

--Retorna todos o retorna todos os movimentos possíveis para um determinado estado do jogo
possibleMoves :: Game -> [Move]
possibleMoves = getGameMoves

--Verifica se um movimento é válido para um determinado estado do jogo
isValidMove :: Game -> Move -> Bool
isValidMove game move = move `elem` possibleMoves game

--Controla o jogo em execução
continueGame :: Game -> IO()
continueGame game = do
    print game
    putStrLn $ "\n\nMovimentos possíveis: \n\n " ++ show (possibleMoves game) ++ "\nInsira seu movimento no formato \"Move (COLUNA_ATUAL, LINHA_ATUAL) (COLUNA_FINAL, LINHA_FINAL)\" (s para Sair): \n"
    strMove <- getLine
    if strMove == "s"
      then putStrLn "Saindo jogo..."
      else if isValidMove game (getMove strMove)
        then do
          putStrLn $ "Movendo a peça " ++ strMove
          playGame $ applyMove game strMove
        else do
          putStrLn $ strMove ++ " \n\nNÃO é um movimento válido\nInsira um movimento válido:"
          continueGame game

--Chamada quando o jogo termina
finishGame :: Game -> IO()
finishGame game = do
    putStrLn $ "\nGAME OVER! O JOGO ACABOU \n TABULEIRO FINAL:\n\n" ++ show game
    putStrLn $ show (gameWinner game) ++ " venceu!"

--Converte uma string em um tipo de dado Move
getMove :: String -> Move
getMove strMove = read strMove :: Move

--Aplica um movimento ao estado atual do jogo
applyMove :: Game -> String -> Game
applyMove game strMove = makeMove game $ getMove strMove

--Função principal que inicia o jogo
main :: IO ()
main = do
 putStrLn "\n\n\nCOMEÇANDO JOGO DE DAMAS!\n\n\n"

 putStrLn "\nREGRAS DO JOGO:\n"
 putStrLn "-Não é possível 'comer' mais uma peça no mesmo turno"
 putStrLn "-Peças normais NÃO podem nem 'comer' nem andar para trás"
 putStrLn "-Damas podem 'comer' e andar para trás, mas NÃO podem fazer saltos longos"
 playGame getDefaultGame
