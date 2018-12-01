toInt x = read x :: Int

readSignedInt ('+':value) = toInt value
readSignedInt value = toInt value

part1 = do
    contents <- readFile "day1.in"
    return $ show $ foldr (+) 0 $ map (readSignedInt) $ lines contents


duplicateFreq :: [Int] -> [Int] -> Int -> Int
duplicateFreq seenFreqs (x:xs) freq = let nextFreq = freq + x in 
    if (elem nextFreq seenFreqs) then nextFreq
    else duplicateFreq (seenFreqs ++ [nextFreq]) xs nextFreq
duplicateFreq seenFreqs [] freq  = error "unsolvable"

part2 = do
    contents <- readFile "day1.in"
    let input = map (readSignedInt) $ lines contents
    let res = duplicateFreq [0] (cycle input) 0
    return (show res)
