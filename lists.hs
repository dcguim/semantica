-- Lists Comprehension

-- In haskell there`s a cool feature that you can do something like
-- ghci> [x*2 | x <- [1..10]]
-- and outputs: [2,4,6,8,10,12,14,16,18,20]
-- But how to create a function which given a interval and a ratio
-- it generates the list latter list?

-- How to make foo independent of the number 2? Solve that
-- foo xs = [x*2 | x <- xs] 
-- main = print(foo([1..10]))

-- If inside a set comprehensions!
boomBangs xs = [if x < 10 then "BOOM!" else "BANG!"| x<-xs, odd x]
main = print(boomBangs([1..30]))
