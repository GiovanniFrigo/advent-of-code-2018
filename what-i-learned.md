# What I learned
#### by trial and error

## File reading

This year I'll be properly reading inputs from files, rather than feeding them manually like last year. A tiny exercise on using the IO monad!

## Day 2

I keep forgetting the basics: how do I create every possibile combination of the elements in a list? With a list comprehension:

```haskell
combineElements :: [a] -> [(a, a)]
combineElements list = [(x, y) | x <- list, y <- list]
```

Of course this will create all the permutations, including specular ones:
```haskell
*Main> combineElements [1..2]
[(1,1),(1,2),(2,1),(2,2)]
```

I could also have used `permutations :: [a] -> [[a]]` from `Data.List` which avoids the duplicates:

```haskell
*Main Data.List> permutations [1..2]
[[1,2],[2,1]]
```

but honestly I forgot that and reinvented the wheel 🤷‍♀️

---- 

Already feeling the lack of a ternary operator. Turns out you can easily define one yourself, with:

```haskell
-- an infix ternary operator: `cond ? truthy $ falsey`
(?) :: Bool -> a -> a -> a
True  ? x = const x
False ? _ = id
```

which can then be used by writing `didHanShotFirst ? "Everybody saw that" $ "Uncle George says no"`.

Adding a Common.hs module where I'll define reusable functions like this.

----
Also, `catMaybes :: [Maybe a] -> [a]` from `Data.Maybe` is super useful to filter out `Nothing`s from list of `Maybe`s:
```haskell
*Main Data.Maybe> catMaybes [Just "a", Nothing, Just "c", Nothing]
["a","c"]
````