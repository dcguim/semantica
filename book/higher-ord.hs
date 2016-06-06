-- To understand higher-orderism, it must
-- firstly be understood that functions can
-- take functions as parameters and also
-- return functions.
-- Let`s see the scope of the apply function
apply_twice :: (a->a) -> a -> a
apply_twice f x = f (f x)
-- The first parameter is a function
-- of type (a->a)

-- Lets try know to implement:
-- "It takes a function and two
-- lists as parameters and then
-- joins the two lists by applying
-- the function between corresponding
-- elements. Here's how we'll implement it:"
zip_with :: (a -> b -> c) -> [a] -> [b] -> [c]
zip_with _ [] _ = []
zip_with _ _ [] = []
zip_with f (x:xs) (y:ys) = (f x y): (zip_with f xs ys)
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
filtr :: (a -> Bool) -> [a] -> [a]
filtr _ [] = []
filtr f (x:xs)
  | f x  = x : (filtr f xs)
  | otherwise = (filtr f xs)

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

-- Let`s make a chain out of a number x
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain x
  | even x = x:(chain(x `div` 2))
  | odd x = x:(chain(x*3 + 1))

-- But if we only want the chains that
--  have the size bigger than 15
long_chains:: (Integral a) => [[a]]
long_chains = (filter isLong (map chain [1..100]))
  where isLong xs = length xs > 15
-- Instead of writing the latter way,
-- it`s possible to use a let function
-- to eliminate the where binding.
long_chainz:: (Integral a) => [[a]]
long_chainz = (filter (\xs -> length xs > 15) (map chain [1..100]))
        

-- We could do something like map (*) [0..]
-- which is something weird cause (*) would
-- take 2 arguments, once we know:
-- (Num a) -> a ->  a -> a. And we only
-- used map with a function that takes only one
-- parameter.
list_funs a b = (multList !! a) b
  where multList = map (*) [0..]
-- when we say multList !! a, we are
-- taking from [(*1), (*2), (*3)..] list
-- its fourth element, i.e. (*4) and
-- then applying this function to
-- the second argument

-- Now we learn how to 'iterate'
-- or fold a list from left.
-- Lets reimplement sum.
sm::(Num a)=> [a] -> a 
sm xs = (foldl (\acc x -> acc + x) 0 xs)
-- this function called such as in: sm [1,88,2]
-- acumulates from 0 every single value
-- of the list bound in x
-- There is a simpler way:
sm'::(Num a)=> [a] -> a 
sm' = foldl (+) 0
-- It would return a function that takes a list.

-- Lets practice foldr now!
-- how could i reimplement the
-- map function, or mp, as we called
-- in this module
mp'::(a->b) -> [a] -> [b]
mp' _ [] = []
mp' f xs = (foldr (\x acc -> f x:acc) [] xs)
-- In this case the acc is bound to the []
-- and the x to the list, say [1,2].
-- The foldr apply f 2:acc = [(f 2)], then
-- it applies f 1, and prepend in the acc
-- that is : [(f 1),(f 2)]

-- The foldr1 and foldl1 functions
-- doesnt require to pass a explicit
-- list. So to implement the maximum
-- and minumum function, we could:
maxi:: (Ord a)=> [a] -> a
maxi = (foldr1 (\acc x -> if x > acc then x else acc))
-- obs: why the order of the paramteres
-- of the foldr1 function does not matter?
-- could either be: "\acc x ->" or "\x acc"?

-- Let`s practice folds, once it is
-- one of the most important tools
-- in a functional programmer toolbox.
-- The reverse function
revrs:: [a] -> [a]
revrs = (foldr (\x acc -> acc ++ [x]) [])
-- A more efficient way to write the
-- reverse function is to use the (:) function
-- intead of the expansive (++)
revers::[a]-> [a]
revers = (foldl (\acc x -> x:acc) [])
-- the (:) function is similar to
-- "\acc x -> x:acc" but with the
-- arguments fliped, so revers could
-- also be written like: (foldl (flip (:)) [])

-- The product function
prodt::(Num a)=> [a] -> a
prodt = (foldl1 (\acc x -> x*acc))

-- filter function
fltr::(a->Bool)-> [a] -> [a]
fltr f xs = (revers (foldl (\acc x -> if f x then x:acc else acc) [] xs))
-- or in a more succint expression
ftr::(a->Bool)->[a]->[a]
ftr f = (foldr (\x acc -> if f x then x:acc else acc) [])

-- To scan the value of add
-- use scanl or scanr

-- Using the $ function
-- defined as:
-- ($) :: (a -> b) -> a -> b  
-- f $ x = f x
sum_sqrt = sum (filter (> 10) (map sqrt [1..130]))
-- Can also be defined as
sm_sqrt = sum $ filter (> 10) $ map sqrt [1..130]

-- Using the (.) composition function
-- we could build a function that
-- simply negate a list of arguments
-- given, in the following way:
ngt = map (negate . abs)

-- So know we can map functions
-- composing them inside the
-- map func param like:
ngt_sqrt = map (negate . sqrt . abs)

-- Pointless Style
-- There are two ways we can look
-- into functions, imagine if we want a
-- function like:
fun x = ceiling (negate (tan (cos (max 50 x))))
-- But how would we remove the parameter x?
-- Or how would we write in point free style?
fn = ceiling . negate . tan .cos . max 50
-- Pointless style makes you think about functions
-- and what kind of functions composing them results
-- in instead of thinking about data and how it's
-- shuffled around.
