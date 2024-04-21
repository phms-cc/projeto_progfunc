module Checkers ( Game, getGameMoves, makeMove, Move, getDefaultGame, gameOver, gameWinner ) where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Map (Map)

--Declaração de sinônimos

type Size = (Int, Int) -- Largura e Altura do tabuleiro
type Position = (Int, Int) -- Linha e Coluna da peça
type PositionMap = Map Position Marker -- Associa a posição do tabuleiro com o respectivo marcador 

--Declaração dos tipos

data Player where
  Player :: Marker -> Player
  deriving (Show, Eq)

data Game where
  Game :: GameState -> [Player] -> Game
  deriving (Show, Eq)

data Board where
  Board :: Size -> PositionMap -> Board
  deriving (Eq) -- Width, Height, Mark

data GameState where
  GameState :: Board -> Player -> GameState
  deriving (Show, Eq) -- State, Board, Current Player

data Marker where
  None :: Marker
  Black :: Marker
  Red :: Marker
  King :: Marker -> Marker
  deriving (Show, Eq)
  
data Move where
  Move :: Position -> Position -> Move
  deriving (Show, Eq, Read)

--Convertendo o tabuleiro para uma representação em String
instance Show Board where
  show :: Board -> String
  show board@(Board (width, height) posMap) = join linhas where
    linhas = addColNums 8 $ addRowNums 1 $ chunk width markers
    addRowNums start (r:rs) = (show start ++ " " ++ r) : addRowNums (start + 1) rs
    addRowNums _ [] = []
    addColNums count rs = rs ++ ["  " ++ foldr (\d s-> s ++ show d) "" (reverse [1..count])]
    markers = concatMap (\k -> showMarker $ posMap Map.! k) positions
    positions = boardPositions board
    chunk _ [] = []
    chunk n xs = y1 : chunk n y2 where
      (y1, y2) = splitAt n xs
    join = foldr (\s ss -> ss ++ ('\n' : s)) ""


-- Retorna uma string que representa visualmente um marcador no tabuleiro de damas.
showMarker :: Marker -> String
showMarker marker = case marker of
                      None       -> "."
                      Black      -> "b"
                      Red        -> "r"
                      King Black -> "B"
                      King Red   -> "R"
                      _          -> "?"

-- Retorna uma lista de todas as posições possíveis em um tabuleiro de damas.
boardPositions :: Board -> [Position]
boardPositions (Board (width, height) _) = [(c, r) | r <- [1..height], c <- [1..width]]


-- Retorna um jogo padrão de damas com um estado inicial.
getDefaultGame :: Game
getDefaultGame = Game startingState [Player Black, Player Red] where
  startingState = GameState getDefaultBoard $ Player Black


-- Retorna um tabuleiro padrão de damas preenchido com peças nas posições iniciais.
getDefaultBoard :: Board
getDefaultBoard = filledBoard where
  emptyBoard = getBoard (8, 8)
  filledBoard = updateBoardPositions filledRedBoard blackPositions Black
  filledRedBoard = updateBoardPositions emptyBoard redPositions Red
  blackPositions = genPos [1..3]
  redPositions = genPos [6..8]
  genPos rows = [(c, d) | c <- [1..8], d <- rows, (even c && even d) || (odd c && odd d) ]


-- Retorna o marcador em uma posição específica no tabuleiro.
markerAt :: PositionMap -> Position -> Marker
markerAt orig pos = Maybe.fromMaybe None (Map.lookup pos orig)

-- Troca as posições de duas peças no tabuleiro.
swap :: PositionMap -> Position -> Position -> PositionMap
swap orig from to = new where
  mrk = markerAt orig
  new = Map.insert from (mrk to) $ Map.insert to (mrk from) orig

-- Verifica se uma posição está dentro dos limites do tabuleiro.
isInBounds :: Board -> Position -> Bool
isInBounds board pos = posX > 0 && posX <= boardX && posY > 0 && posY <= boardY where
  (boardX, boardY) = size board
  (posX, posY) = pos

-- Retorna uma lista de marcadores representando um tabuleiro.
getBoard :: Size -> Board
getBoard sz = Board sz $ getBoardPositionMap sz


-- Gera um mapeamento de posição vazio para um tabuleiro.
getBoardPositionMap :: Size -> PositionMap
getBoardPositionMap (width, height) = Map.fromList $ map (, None) positions where
  positions = [(row, col) | row <- [1..height], col <- [1..width]]

-- Cria um par de posição e marcador.
posMapPair :: Marker -> Int -> Int -> (Position, Marker)
posMapPair mark r c = ((r,c), mark)

-- Retorna o tamanho do tabuleiro.
size :: Board -> Size
size (Board s _) = s

-- Recupera os marcadores do tabuleiro.
markers :: Board -> PositionMap
markers (Board _ ms) = ms

-- Atualiza os marcadores associados ao tabuleiro.
swapMarkers :: Board -> PositionMap -> Board
swapMarkers (Board sz _) = Board sz --  substitui o PositionMap existente pelo novo PositionMap fornecido como argumento

-- Atualiza o marcador no tabuleiro.
updateBoard :: Board -> Position -> Marker -> Board
updateBoard board pos@(row, col) mark = newBoard where
  ms = markers board
  newBoard = swapMarkers board newMarks
  newMarks = Map.insert pos mark ms -- retorna um novo objeto Board com o marcador na posição especificada atualizado para o novo marcador fornecido.

-- Atualiza os marcadores do tabuleiro para várias posições.
updateBoardPositions :: Board -> [Position] -> Marker -> Board
updateBoardPositions board positions marker = updatedBoard where
  updatedBoard = foldr updatePos board positions
  updatePos pos board = updateBoard board pos marker

-- Retorna as posições vazias do tabuleiro.
emptyPositions :: Board -> [Position]
emptyPositions (Board _ posMap) = Map.keys $ Map.filter (== None) posMap

-- Verifica se um jogador ganhou o jogo.
hasWon :: Board -> Marker -> Bool
hasWon _ None = False
hasWon board (King marker) = hasWon board marker
hasWon (Board _ posMap) marker = not otherMarkerExists where
  otherMarkerExists = any (eqMarker otherMarker) elems
  otherMarker = basicMarker marker
  eqMarker m a = basicMarker a == m
  elems = Map.elems posMap

-- Retorna o tabuleiro atual de um jogo.
gameBoard :: Game -> Board
gameBoard (Game (GameState board _) _) = board

-- Verifica se o jogo acabou.
gameOver :: Game -> Bool
gameOver game = noMovesForCurrentPlayer || any (hasWon board) markers where
  board = gameBoard game
  markers = [Red, Black]
  noMovesForCurrentPlayer = null $ getGameMoves game

-- Retorna o vencedor do jogo.
gameWinner :: Game -> Marker
gameWinner game@(Game (GameState b (Player m)) _) = winner where
  winner = if gameOver game then
                            if m == Red then Black
                                        else Red
                            else None

-- Limpa a posição
clearStatePos :: GameState -> Position -> GameState
clearStatePos orig posSrc = replaceStatePos orig posSrc None

-- Substitui o item na posição por um marcador arbitrário.
replaceStatePos :: GameState -> Position -> Marker -> GameState
replaceStatePos orig@(GameState origBoard origPlayer) posSrc newMarker =
  GameState newBoard origPlayer where
  newBoard = updateBoard origBoard posSrc newMarker

-- Obtém o marcador na posição especificada do estado do jogo.
getStateMarkerAt :: GameState -> Position -> Marker
getStateMarkerAt (GameState board _) = markerAt (markers board)

-- Atualiza o estado do jogo após fazer um movimento.
updateState :: GameState -> Position -> Position -> GameState
updateState orig@(GameState origBoard origPlayer) posSrc posDes =
  GameState newBoard next where
  sourceMarker = markerAt (markers origBoard) posSrc
  removeBoard = updateBoard origBoard posSrc None
  newBoard = updateBoard removeBoard posDes sourceMarker
  next = case origPlayer of
    Player Black        -> Player Red
    Player (King Black) -> Player Red
    _                   -> Player Black

-- Dado um tabuleiro e uma posição no tabuleiro, essa função retorna os movimentos válidos para a peça.
-- As peças podem se mover "para frente" diagonalmente, a menos que sejam promovidas a reis
-- As peças podem pular sobre peças do jogador adversário para se mover duas casas diagonalmente.
boardMoves :: Board -> Position -> [Position]
boardMoves board src = boardWalk board src ++ boardJump board src


-- Dado um tabuleiro e uma posição no tabuleiro, esta função retorna os movimentos válidos para a peça
-- ou seja, os lugares onde ela pode se mover para uma única posição sem pular sobre outra peça.
boardWalk :: Board -> Position -> [Position]
boardWalk board src = filter (inBoundsAndEmpty board) (possibleMoves marker) where
  marker = markerAt (markers board) src
  (x, y) = src
  possibleMoves (King _) = possibleMoves Black ++ possibleMoves Red
  possibleMoves Black = [(x-1,y+1), (x+1,y+1)]
  possibleMoves Red = [(x-1,y-1), (x+1,y-1)]
  possibleMoves _ = []

-- Verifica se uma posição de destino está em direção à frente para a peça.
isTowards :: Marker -> Position -> Position -> Bool
isTowards Red (_, sy) (_, dy)   = dy > sy
isTowards Black (_, sy) (_, dy) = dy < sy

-- Verifica se uma posição está dentro dos limites do tabuleiro e se está vazia.
inBoundsAndEmpty :: Board -> Position -> Bool
inBoundsAndEmpty board pos = isInBounds board pos && boardEmptyAt board pos

-- Verifica se uma posição em um tabuleiro está vazia.
boardEmptyAt :: Board -> Position -> Bool
boardEmptyAt board pos = m == None where
  m = markerAt (markers board) pos

-- Retorna os saltos válidos para uma peça em um tabuleiro a partir de uma posição específica.
boardJump :: Board -> Position -> [Position]
boardJump board src = jumps where
  jumps = filter (inBoundsAndEmpty board) (possibleMoves marker)
  marker = markerAt (markers board) src
  
  --Retorna a lista de posições válidas para o marcador especificado
  possibleMoves :: Marker -> [Position]
  possibleMoves Black        = possibleMovesColor Black
  possibleMoves Red          = possibleMovesColor Red
  possibleMoves (King color) = possibleJumps
  possibleMoves _            = []

  --Retorna a lista de posições válidas para a cor especificada
  possibleMovesColor :: Marker -> [Position]
  possibleMovesColor color = filter towards moves where
    towards = isTowards (toggleColor color) src
    moves = possibleJumps

  -- Tuplas de uma posição saltada e o destino do salto
  possibleJumps = map (\(_, a, _) -> a) jumps where
    jumps = filter canJump possibleJumpTuples
    possibleJumpTuples = zip3 diag1 diag2 piecesAtDiagonals
    diag1 = diagonals src
    diag2 = diagonalN 2 src
    piecesAtDiagonals = map (markerAt (markers board)) diag1
    -- Uma peça pode saltar para a posição se ela saltar sobre uma peça da cor oposta
    canJump :: (Position, Position, Marker) -> Bool
    canJump (_, _, m) = markerMayJumpMarker marker m

-- Alterna a cor do marcador
toggleColor :: Marker -> Marker
toggleColor Black        = Red
toggleColor Red          = Black
toggleColor (King color) = King $ toggleColor color
toggleColor c            = c

-- Verifica se um marcador pode pular outro marcador
markerMayJumpMarker :: Marker -> Marker -> Bool
markerMayJumpMarker None _ = False
markerMayJumpMarker marker (King m) = m /= None && markerMayJumpMarker marker m
markerMayJumpMarker marker Red = marker /= Red
markerMayJumpMarker marker Black = marker /= Black
markerMayJumpMarker _ _ = False

--Retorna todas as posições que estão a uma distância dada da posição passada
diagonalN :: Int -> Position -> [Position]
diagonalN d (r, c) = [(r+d, c+d), (r-d, c-d), (r+d, c-d), (r-d, c+d)]


--Retorna todas as posições que estão a uma diagonal de distância da posição passada
diagonals :: Position -> [Position]
diagonals = diagonalN 1

--Recupera os movimentos possíveis da posição anterior
getStateMoves :: GameState -> Position -> [Move]
getStateMoves state@(GameState board@(Board sz posMap) player) posSrc = moves where
  moves = if markerMatches player marker
            then map (Move posSrc) (boardMoves board posSrc) else []
  marker = markerAt posMap posSrc
  markerMatches (Player m1) m2 = basicMarker m1 == basicMarker m2

--Retorna as posições do tabuleiro ocupadas pelo jogador especificado
getPlayerPositions :: Player -> Board -> [Position]
getPlayerPositions player board = positions where
  marker = case player of
             (Player m) -> basicMarker m
  matchesMarker pos = marker == basicMarker (markerAt (markers board) pos)
  positions = filter matchesMarker $ boardPositions board

--Retorna uma lista de todos os movimentos possíveis para o jogador no estado atual
getAllStateMoves :: GameState -> [Move]
getAllStateMoves state@(GameState board player) = moves where
  moves = concatMap (getStateMoves state) positions
  positions = boardPositions board

-- Retorna uma lista de todos os movimentos possíveis do jogo
getGameMoves :: Game -> [Move]
getGameMoves (Game gs _) = getAllStateMoves gs

-- Remove o status de rei da peça.
basicMarker :: Marker -> Marker
basicMarker (King m) = basicMarker m
basicMarker Black = Black
basicMarker Red = Red
basicMarker None = None

-- Realiza um movimento no jogo.
makeMove :: Game -> Move -> Game
makeMove game@(Game gs players) move@(Move p1 p2) = newGame where
  newGame = Game newState3 players
  newState1 = updateState gs p1 p2
  newState2 = if isJump move
                then clearStatePos newState1 jumpedPos
                else newState1
  newState3 = if shouldKingPosition movedMarker p2
                then kingPieceAt newState2 p2
                else newState2

  movedMarker = getStateMarkerAt newState2 p2

  jumpedPos = getJumpedPosition move

--Torna uma peça um rei.
kingPieceAt :: GameState -> Position -> GameState
kingPieceAt gs pos = newState where
  newState = replaceStatePos gs pos kingedPiece
  kingedPiece = kingPiece $ getStateMarkerAt gs pos

-- Verifica se o movimento é um salto.
isJump :: Move -> Bool
isJump (Move (sc,sr) (dc,dr)) = diff > 1 where
  diff = abs $ sr - dr

-- Obtém a posição pulada em um movimento de salto.
getJumpedPosition :: Move -> Position
getJumpedPosition m@(Move (sc,sr) (dc,dr)) = mPos where
  mPos = (sc+deltaColumn,sr+deltaRow)
  deltaColumn = if dc > sc then 1 else -1
  deltaRow = if dr > sr then 1 else -1

-- Determina se uma peça em uma posição deve ser coroada.
shouldKingPosition :: Marker -> Position -> Bool
shouldKingPosition (King _) _ = False
shouldKingPosition Black (_, 8) = True
shouldKingPosition Red (_, 1) = True
shouldKingPosition _ _ = False

-- Coroa uma peça preta ou vermelha e deixa outras peças inalteradas.
kingPiece :: Marker -> Marker
kingPiece Black = King Black
kingPiece Red = King Red
kingPiece m = m


