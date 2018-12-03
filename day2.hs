import Data.List
import Data.Maybe

boxChecksum :: [Char] -> [Int]
boxChecksum id = map length $ group $ sort id 

has2 checksum = 2 `elem` checksum
has3 checksum = 3 `elem` checksum

with2 boxChecksums = length $ filter (has2) boxChecksums
with3 boxChecksums = length $ filter (has3) boxChecksums

checksum :: [[Char]] -> Int
checksum lines = let checksums = map boxChecksum lines in
    (with2 checksums) * (with3 checksums)

part1 = do
    contents <- readFile "day2.in"
    return $ show $ checksum $ lines contents

matchingChecksum :: ([Char], [Char]) -> Maybe [Char]
matchingChecksum (list1, list2) = let sameCharacters = filter (\(a, b) -> a == b) $ zip list1 list2 in
    (length list1 == 1 + length sameCharacters) ? Just (map fst sameCharacters) $ Nothing

(?) :: Bool -> a -> a -> a
True  ? x = const x
False ? _ = id

-- alternative version using zipWith. I like this one better
matchingChecksum' :: ([Char], [Char]) -> Maybe [Char]
matchingChecksum' (list1, list2) = 
    let sameCharacters = catMaybes $ zipWith (\x -> \y -> (x == y) ? Just x $ Nothing) list1 list2 in
    (length list1 == 1 + length sameCharacters) ? Just sameCharacters $ Nothing 

combineLines :: [[Char]] -> [([Char], [Char])]
combineLines lines = [(x, y) | x <- lines, y <- lines]

part2 = do
    contents <- readFile "day2.in"
    return $ show $ head $ catMaybes $ map matchingChecksum' $ combineLines $ lines contents