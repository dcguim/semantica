-- Now I`ll dive in recursion functions, I'll start
-- trying to rewrite a maximum function, with the
-- guard arquitecture in mind
maxi:: (Ord a) => [a] -> a
maxi [] = error "The list is empty!"
maxi [x] = x
maxi (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = maxi xs
  
