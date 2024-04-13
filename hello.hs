---- bu da chatgpt nin yazdığı hatalı kod dosyası



import Data.List

type Board = [Int]
type Player = Int
type Position = Int

-- Initial board state
initialBoard :: Board
initialBoard = replicate 14 4  -- 14 holes with 4 stones each

-- Main function to start the game
main :: IO ()
main = do
    putStrLn "Welcome to Mancala!"
    playGame initialBoard 0

-- Function to play the game
playGame :: Board -> Player -> IO ()
playGame board player = do
    printBoard board
    if gameOver board
        then endGame board
        else do
            putStrLn $ "Player " ++ show player ++ "'s turn"
            position <- getPlayerMove
            let (newBoard, nextPlayer) = moveStones board player position
            playGame newBoard nextPlayer

-- Function to print the current board state
printBoard :: Board -> IO ()
printBoard board = do
    putStrLn $ "   " ++ intercalate " | " (map show (reverse (take 6 board)))
    putStrLn $ " |---+---+---+---+---+---|"
    putStrLn $ " | " ++ intercalate " | " (map show (drop 7 board))

-- Function to determine if the game is over
gameOver :: Board -> Bool
gameOver board = all (== 0) (take 6 board) || all (== 0) (drop 7 board)

-- Function to end the game and determine the winner
endGame :: Board -> IO ()
endGame board = do
    let player1Score = sum (take 6 board)
        player2Score = sum (drop 7 board)
    putStrLn $ "Player 1 score: " ++ show player1Score
    putStrLn $ "Player 2 score: " ++ show player2Score
    if player1Score > player2Score
        then putStrLn "Player 1 wins!"
        else if player1Score < player2Score
            then putStrLn "Player 2 wins!"
            else putStrLn "It's a tie!"

-- Function to get the player's move
getPlayerMove :: IO Position
getPlayerMove = do
    putStrLn "Enter the position to move stones from (1-6):"
    position <- readLn
    if position >= 1 && position <= 6
        then return position
        else do
            putStrLn "Invalid position, please try again"
            getPlayerMove

-- Function to move stones on the board
moveStones :: Board -> Player -> Position -> (Board, Player)
moveStones board player position =
    let stones = board !! (position - 1)
        newBoard = distributeStones board (position - 1) stones
        nextPlayer = if (position - 1) + stones `mod` 13 == 6 then player else (player + 1) `mod` 2
    in (newBoard, nextPlayer)

-- Function to distribute stones from a given position
distributeStones :: Board -> Position -> Int -> Board
distributeStones board _ 0 = board
distributeStones board position stones =
    let nextPosition = (position + 1) `mod` 14
        nextBoard = if nextPosition == 13 then board else updateBoard board nextPosition (board !! nextPosition + 1)
    in distributeStones nextBoard nextPosition (stones - 1)

-- Function to update the board with a new value at a given position
updateBoard :: Board -> Position -> Int -> Board
updateBoard board position value = take position board ++ [value] ++ drop (position + 1) board
