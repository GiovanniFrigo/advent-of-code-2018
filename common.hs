-- ternary operator
bool :: a -> a -> Bool -> a
bool t _ True = t
bool _ f False  = f

-- an infix ternary operator `cond ? truthy $ falsey`
(?) :: Bool -> a -> a -> a
True  ? x = const x
False ? _ = id

-- comibine two lists
combineLists :: [a] -> [(a, a)]
combineLists lists = [(x, y) | x <- lists, y <- lists]

-- solution template
part1 = do
    contents <- readFile "dayX.in"
    return $ show $ lines contents