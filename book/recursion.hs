-- Now I`ll dive in recursion functions!

-- I'll start trying to rewrite the maximum
-- function, with the guard arquitecture in mind
maxi:: (Ord a) => [a] -> a
maxi [] = error "The list is empty!"
maxi [x] = x
maxi (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = maxi xs

-- Try to replicate a number n times,
replict :: (Num i, Ord i) => i -> a -> [a]
replict n x
  | n <= 0 = []
  | otherwise = x:replict (n-1) x
-- obs: why when I change Num to Int, it
-- does not compile, why should I even
-- use two types instead of just "Int"?

-- Think, before reading the answer,
-- how would i make a function that
-- given a number i and a list it
-- returns the first i numbers of this
-- list?
tk :: (Num i, Ord i) => i -> [a] -> [a]
tk n _
  | n <= 0 = []
tk _ [] = []
tk n (x:xs) = x: tk (n-1) xs

-- Make a function that reverses a
-- given list.
revers :: [a] -> [a]
revers [] = []
revers (x:xs) = reverse xs ++ [x]

-- Lets make an infinite function!
-- that is very interesting about Haskell
-- cause it exhibits its lazy evaluation
-- feature!
rept :: a -> [a]
rept x = x:repeat x
-- obs: Why when I evaluated it just
-- outputed infinite '3's and I
-- had to interrrupt the process
-- manually? Isn`t it early eval?

-- Let`s zip things, make
-- a function that given two
-- lists, say:
-- a = [10,20,30,40] and
-- b = [-1,-2,-3]
-- it zips both list by returning:
-- [(10, -1), (20, -2), (30, -3)]
zp::[a]->[b]-> [(a,b)]
zp _ [] = []
zp [] _ = []
zp (xa:xas) (xb:xbs) = (xa,xb): (zip xas xbs)

-- Make a simple function that given
-- list and an element and checks if
-- that list contains the element
elm :: (Eq a) => [a] -> a -> Bool
elm [] _ = False
elm (x:xs) k
  | k == x = True
  | otherwise = elm xs k

-- Lets organize things, quicksort in
-- Haskell!
qs :: (Ord a) => [a] -> [a]
qs [] = []
qs (x:xs) = smallerSort (x:xs) ++ [x] ++ biggerSort (x:xs)
  where smallerSort (x:xs) = qs [a | a <-xs, a < x ]
        biggerSort (x:xs) = qs [b | b <-xs, b > x ]
-- obs: Why is this first implementation of
-- quicksort wrong, try for instance:
-- qs ([1..18]++[13..23]++[-2..7])
-- Notice that the first one deletes
-- duplicates while the next
-- implementation won`t, that`s
-- simply because of the '<' and "<="
-- difference
qks :: (Ord a) => [a] -> [a]  
qks [] = []  
qks (x:xs) =
  let smallerSorted = qks [a | a <- xs, a <= x]  
      biggerSorted = qks [a | a <- xs, a > x]  
  in  smallerSorted ++ [x] ++ biggerSorted  
        
