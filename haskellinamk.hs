--- bu ana ödev dosyası





type Board = [Int]
type Player = Int
type Position = Int
writeListElements :: Board -> String
writeListElements xs = 
    show (head xs) ++ " | " ++ show (xs !! 1) ++ " | " ++ show (xs !! 2) ++ " | " ++ show (xs !! 3) ++ " | " ++ show (xs !! 4) ++ " | " ++ show (xs !! 5) ++ " | " ++ show (xs !! 6) ++ " | "

main :: IO ()
main =
    let list = [0,4,4,4,4,4,4,4,4,4,4,4,4,0]
        
    in do
        putStrLn "enter input"
        hole <- readLn
        let list2 = moveStones list 2 hole
            printedList = writeListElements list2       
        
        putStrLn printedList

incrementBeforeIndex :: Board -> Position -> Board
incrementBeforeIndex xs 0 = xs  -- Base case: reached the specified index, stop incrementing
incrementBeforeIndex (x:xs) n = (x + 1) : incrementBeforeIndex xs (n - 1)

moveStones :: Board -> Player -> Position -> Board
moveStones board player position =
    let 
        index = if player == 2 then 7 - position else position + 7
        newBoard = incrementBeforeIndex board index 
        
    in newBoard   