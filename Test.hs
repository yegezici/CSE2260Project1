incrementBeforeIndex :: Int -> [Int] -> [Int]
incrementBeforeIndex 0 xs = xs  -- Base case: reached the specified index, stop incrementing
incrementBeforeIndex n (x:xs) = (x + 1) : incrementBeforeIndex (n - 1) xs

updateElementAtIndex :: Int -> a -> [a] -> [a]
updateElementAtIndex 0 newValue (_:xs) = newValue : xs  -- Update the first element
updateElementAtIndex n newValue (x:xs) = x : updateElementAtIndex (n - 1) newValue xs

writeListElements :: [Int] -> String
writeListElements xs = show (head xs) ++ " | " ++ show (xs !! 1) ++ " | " ++ show (xs !! 2) ++ " | " ++ show (xs !! 3) ++ " | " ++ show (xs !! 4) ++ " | " ++ show (xs !! 5) ++ " | " ++ show (xs !! 6) ++ " | "
main :: IO ()
main = 
    
    let x = [0,4,4,4,4,4,4]
        --user2 = writeListElements x
        user1 = writeListElements [4,4,4,4,4,4]
        treasureForUser1 = 0
        treasureForUser2 = 0
        x' = writeListElements x
    in do
        putStrLn x'
        putStrLn "Enter a new hole number:"
        hole <- readLn
        let 
            index = 6 - hole
            list = incrementBeforeIndex index x
            list1 = updateElementAtIndex index 1 list
            user2 = writeListElements list1
        putStrLn user2
        
         
        
