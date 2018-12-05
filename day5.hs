import Data.List
import Data.Char
import Common ((?))

diff = abs $ ord 'A' - ord 'a'

shouldDelete :: Char -> Char -> Bool
shouldDelete x y = abs (ord x - ord y) == diff

deleteAdjacent :: [Char] -> [Char]
deleteAdjacent (x:y:xs) = (shouldDelete x y) ? deleteAdjacent xs $ x : (deleteAdjacent (y:xs))
deleteAdjacent (x:[]) = [x] 
deleteAdjacent [] = [] 

recursivelyDeleteAdjacents :: [Char] -> [Char]
recursivelyDeleteAdjacents input = let transformed = deleteAdjacent input in 
    (length input == length transformed) ? transformed $ recursivelyDeleteAdjacents transformed

part1 = do
    contents <- readFile "day5.in"
    return $ length $ recursivelyDeleteAdjacents contents

removePolymer :: Char -> [Char] -> [Char]
removePolymer polymer = filter (\x -> x /= polymer && not (shouldDelete x polymer))

part2 = do 
    contents <- readFile "day5.in"
    let removedPolymers = zipWith removePolymer ['a'.. 'z'] $ cycle [contents]
    return $ minimum $ map length $ map recursivelyDeleteAdjacents removedPolymers