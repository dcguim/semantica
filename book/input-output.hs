-- An I/O action is something that, when performed,
-- carry out a side effect (that`s usually, either
-- reading from input or printing stuff to screen).
-- An I/O action is performed when we give it a
-- name of main in and then run the program. such as:
--main = do
--  putStrLn "Hello, what `s your name?"
--  name <- getLine
--  putStrLn ("Hey "++ name ++ ", you rock!")

-- And if we're taking data out of an I/O action,
-- we can only take it out when we're inside
-- another I/O action. This is how Haskell manages
-- to neatly separate the pure and impure parts of
-- our code. getLine is in a sense impure because
-- its result value is not guaranteed to be the
-- same when performed twice. That's why it's sort
-- of tainted with the IO type constructor and we
-- can only get that data out in I/O code.
-- And because I/O code is tainted too, any
-- computation that depends on tainted I/O data will
-- have a tainted result.
-- Important features of I/O operations
-- * In a do block the last action cannot be bound
-- to a name (e.g. "result <- putStrLn ("Hey" ...)),
-- but every other line can also written with a bind
-- such as in: str <- putStrLn "Hello, what `s your name?"
-- * I/O actions will only be performed when they are
-- given the name of main, or when they are inside
-- a bigger I/O action in another do block and so on.
-- Or if they are inside a bigger I/O action within a
-- do block
-- An I/O action is also performed when you type out
-- an I/O action in GHCI and press ENTER.

-- import Data.Char  
      
-- main = do  
--  putStrLn "What's your first name?"  
--  firstName <- getLine  
--  putStrLn "What's your last name?"  
--  lastName <- getLine  
--  let bigFirstName = map toUpper firstName  
--      bigLastName = map toUpper lastName  
--  putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"  

-- It must be said that the "<-" is normally
-- used to map I/O action to names such as
-- in: firstName <- getLine. But when we need
-- to bind pure expressions to names we use let
-- as: let firstName = map toUpper firstName.

-- main = do
--  line <- getLine
--  if null line
--     then return ()
--    else (do
--          putStrLn $ reverseWords line
--          main)
    
--reverseWords :: String -> String
--reverseWords = unwords . map reverse . words

-- Three important points must be made about I/O actions:

-- 1) "return" and "<-" act as opposites,
-- "return" takes a value and wraps in a
-- box, while "<-" takes a value box
-- from a box and binds to a name.
-- obs: the "return" doesnt end the execution
-- of a program or anything like that.

-- 2) The else do block is one I/O action
-- that is why it is important to be idented
-- correctly or limited by the parethesis.
-- obs main which is an I/O action is called 
-- recursively at the end of the block.

-- 3) When dealing with I/O do blocks, "return"
-- is normally used either because its necessary
-- to create an I/O action that doesn`t do anything
-- or when its necessary to return the return a value
-- of a do block which is not the last action.

-- putStr function, putStr:: String -> IO ()
-- main = do putStr "Hey, "
--          putStr "I`m "
--          putStrLn "Andy!"
-- the only diff. from putStrLn is that it doesn`t
-- jump a line. It could be defined in terms of
-- putChar which takes a character and returns an I/O action

-- putStr':: String -> IO ()
-- putStr' [] = return ()
-- putStr' (x:xs) = do
--  putChar x
--  putStr' xs

-- main = do putStr' "Hello Folks!"

-- import Data.Char  

-- function putChar reads a character from input (getChar:: IO Char)
-- and the result contained within the I/O action is a Char.
-- main = do
--   c <- getChar
--   if c /= ' '
--      then (do
--            putChar c
--            main)
--     else return ()

-- when function takes a boolean value and an I/O action
-- if that boolean is True it returns the same I/O action

-- import Control.Monad

-- main = do
--  c <- getChar
--  when (c /= ' ') $ do
--    putChar c
--    main

-- Sequence takes a list of I/O actions and return
-- an I/O action that will perform those actions one
-- after the other. The result contained in that I/O
-- action will be a list of results of all I/O actions
-- of all I/O actions that were performed.
-- main = do
--  rs <- sequence [getLine, getLine, getLine]
--  print rs

-- It is common to map functions like print or putStrLn
-- aka I/O actions over lists: sequence (map print [1,2,3,4])
-- But there`s a utility function mapM and mapM_ that do the job
-- mapM_ can be used when we are not interested in the result.

import Control.Monad  
  
main = do   
  colors <- forM [1,2,3,4]
            (\a -> do  
                putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"  
                color <- getLine  
                return color)  
  putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "  
  mapM putStrLn colors  

