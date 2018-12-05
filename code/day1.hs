-- http://adventofcode.com/2018/day/1
-- Chronal Calibration

toInt x = read x :: Int

readSignedInt ('+':value) = toInt value
readSignedInt value = toInt value

part1 = do
    contents <- readFile "day1.in"
    return $ foldr (+) 0 $ map (readSignedInt) $ lines contents

duplicateFreq :: [Int] -> [Int] -> Int -> Int
duplicateFreq seenFreqs (x:xs) freq = let nextFreq = freq + x in 
    if (elem nextFreq seenFreqs) then nextFreq
    else duplicateFreq (seenFreqs ++ [nextFreq]) xs nextFreq
duplicateFreq seenFreqs [] freq  = error "unsolvable"

part2 = do
    contents <- readFile "day1.in"
    let inputAsInt = map (readSignedInt) $ lines contents
    return $ duplicateFreq [0] (cycle inputAsInt) 0

main = do
    putStrLn "Solving.."
    solution1 <- part1
    putStrLn $ "Part 1: " ++ show solution1
    solution2 <- part2
    putStrLn $ "Part 2: " ++ show solution2