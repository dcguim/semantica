-- Lists Comprehension
-- This haskell file has been deeply inspired by:
-- http://learnyouahaskell.com/syntax-in-functions
-- http://learnyouahaskell.com/types-and-typeclasses
-- where "inspired by" = "stolen from"

-- In haskell there`s a cool feature that you can do something like
-- ghci> [x*2 | x <- [1..10]]
-- and outputs: [2,4,6,8,10,12,14,16,18,20]
-- But how to create a function which given a interval and a ratio
-- it generates the latter list?

-- How to make foo independent of the number 2? Solve that
foo :: [Integer] -> Integer -> [Integer]
foo xs r = [x*r | x <- xs]
-- The ten first multiples of three: 1:foo [1..10] 3

-- If inside a set comprehensions!
boomBangs xs = [if x < 10 then "BOOM!" else "BANG!"| x<-xs, odd x]

-- Types and Functions
-- In Haskell I can add a 2D vector like this:
add2D :: (Num a) => (a,a) -> (a,a) -> (a,a)
add2D (x1, x2) (y1, y2) = (x1 + y1, x2 + y2)
-- add2D (1,2) (3,4)
-- yields: (4,6)
-- Obs: Num is a typeclass that contains other typeclasses
-- e.g. Integral and Double. Integral is also a typeclass which
-- contain the types Int and Integer.

-- Functions
-- A naive factorial function
fact :: (Integral a) => a -> a
fact 0 = 1
fact x = x*fact (x - 1)

-- summing the elements of a list
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- Using guards
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= skinny = "You`re underweight"
  | bmi <= normal = "You`re just normal"
  | bmi <= fat = "You`re fat"
  | otherwise = "You`re a whale, congratulations!"
  where bmi = weight / height ^ 2
        skinny = 18.5
        normal = 25.0
        fat = 30.0
-- Comparing using guards
mycomp :: (Ord a) => a -> a -> Ordering
a `mycomp` b
  | a > b = GT
  | a == b = EQ
  | otherwise = LT
-- If you want to put the arguments a and b between
-- a function it is necessary to use the grave accent
-- between the function when defining it.
-- Ord covers all the stardard comparing function such as >
-- but Haskell evaluates: True > False, which returns True,
-- and all other types, Num and String can also be compared
-- through '>', '<', ">=" or "<=."
-- Notice that: True `mycomp` False, yields: GT.

-- Using the where function
initials :: String -> String -> String
initials firstn lastn = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstn
        (l:_) = lastn
-- It is necessary bracket those characters in order to use
-- them in the (++) function.

-- Let`s use the concept of bmi to take a list of weight and height
-- and return a list of bmis
calcBmis:: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi w h | (w,h) <- xs ]
  where bmi weight height = weight / height ^ 2
-- Why when I change RealFloat to Float haskell outputs a:
-- > "‘Float’ is applied to too many type arguments"

-- Let, just like an if statement, "you can cram in almost anywhere".
ifgive42 = 4 * (if 10 > 5 then 10 else 5) + 2
letgive42 = 4 * (let a = 9 in a + 1) + 2
-- both yields 42
-- you can even use it on a local scope:
retlist5 = [let square x = x*x; root x = sqrt x in (square 5, root 25)]
-- or for dismantling a tuple into components:
dismantuple = (let (a,b,c) = (1,2,3) in a+b+c)* 100

-- Now we can go back to the bmi concept to 
