module Common where

-- an infix ternary operator: `cond ? truthy $ falsey`
(?) :: Bool -> a -> a -> a
True  ? x = const x
False ? _ = id

-- combine the elements of a list
combineElements :: [a] -> [(a, a)]
combineElements list = [(x, y) | x <- list, y <- list]

-- solution template
part1 = do
    contents <- readFile "dayX.in"
    return $ lines contents