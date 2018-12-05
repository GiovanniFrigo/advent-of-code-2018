-- http://adventofcode.com/2018/day/2
-- Inventory Management System

import Data.List
import Data.Maybe
import Common ((?), combineElements)

boxChecksum :: [Char] -> [Int]
boxChecksum id = map length $ group $ sort id 

countListsWithElement n boxChecksums = length $ filter (n `elem`) boxChecksums

checksum :: [[Char]] -> Int
checksum lines = let checksums = map boxChecksum lines in
    (countListsWithElement 2 checksums) * (countListsWithElement 3 checksums)

part1 = do
    contents <- readFile "day2.in"
    return $ checksum $ lines contents

matchingChecksum :: ([Char], [Char]) -> Maybe [Char]
matchingChecksum (list1, list2) = let sameCharacters = filter (\(a, b) -> a == b) $ zip list1 list2 in
    (length list1 == 1 + length sameCharacters) ? Just (map fst sameCharacters) $ Nothing

-- alternative version using zipWith. I like this one better
matchingChecksum' :: ([Char], [Char]) -> Maybe [Char]
matchingChecksum' (list1, list2) = 
    let sameCharacters = catMaybes $ zipWith (\x -> \y -> (x == y) ? Just x $ Nothing) list1 list2 in
    (length list1 == 1 + length sameCharacters) ? Just sameCharacters $ Nothing 

part2 = do
    contents <- readFile "day2.in"
    return $ head $ catMaybes $ map matchingChecksum' $ combineElements $ lines contents

main = do
    putStrLn "Solving.."
    solution1 <- part1
    putStrLn $ "Part 1: " ++ show solution1
    solution2 <- part2
    putStrLn $ "Part 2: " ++ show solution2