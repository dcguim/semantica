-- To understand higher-orderism, it must
-- firstly be understood that functions can
-- take functions as parameters and also
-- return functions.
-- Let`s see the scope of the apply function
applyTwice :: (a->a) -> a -> a
applyTwice f x = f (f x)
-- The first parameter is a function
-- of type (a->a)

-- Lets try know to implement:
-- "It takes a function and two
-- lists as parameters and then
-- joins the two lists by applying
-- the function between corresponding
-- elements. Here's how we'll implement it:"
zpWth :: (a -> b -> c) -> [a] -> [b] -> [c]
zpWth _ [] _ = []
zpWth _ _ [] = []
zpWth f (x:xs) (y:ys) = (f x y): (zpWth f xs ys)
-- Observe the scope...
-- it takes two lists of type [a]
-- and [b], so as the first parameter
-- joins elements of the first and
-- second lists, it must have
-- the (a->b-> ... in its type.
-- As the return is a list of c`s
-- each time the parameter function
-- is called it must each time
-- return a c, which yields: (a->b->c)

-- Lets try to implement flip now
-- flip takes f a b and flips it, i.e.,
-- f b a
flp :: (a -> b -> c) -> (b -> a -> c)
flp f x y = f y x
-- That kind of thing "f x y = f y x"
-- it is only possible because Haskell
-- has "the advantage" of currying when
-- making higher-order functions
-- obs: Thing is ... what is curry??

-- Lets implement the famous and also
-- lispy function map
mp :: (a->b) -> [a] -> [b]
mp _ [] = []
mp f (x:xs) = (f x):(map f xs)
-- It`s interesting to think
-- what would mp (mp (^2)) [[1,2],[3..6],[7,8]]
-- Well, what mp does is to apply the given
-- function to each and every element of
-- the list. So it is simply mapping to each
-- list within the main list a  (^2) function.
-- which must return:[[1,4],[9,16,25,36],[49,64]]

-- Now it`s time to filter, that means
-- we need a function that given a predicate
-- (tells whether something is true or not)
-- and a list, it retuns a list with the elements
-- that satisfy that predicate
fltr :: (a -> Bool) -> [a] -> [a]
fltr _ [] = []
fltr f (x:xs)
  | f x  = x : (fltr f xs)
  | otherwise = (fltr f xs)

-- We could now redefine the quicksort
-- using the fltr list we just made,
-- ch-check it out:
qst :: (Ord a) => [a] -> [a]
qst [] = []
qst (x:xs) = smallerPivot ++ [x] ++ biggerPivot
  where smallerPivot = qst (filter (<= x) xs)
        biggerPivot = qst (filter (> x) xs)

-- Finding the largest denominator for 3829
-- that is under 100k
lgdn :: (Integral a) => a
lgdn = head (filter p [100000,99999..])
  where p x = x `mod` 3829 == 0
