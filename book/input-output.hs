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

import Data.Char  
      
main = do  
  putStrLn "What's your first name?"  
  firstName <- getLine  
  putStrLn "What's your last name?"  
  lastName <- getLine  
  let bigFirstName = map toUpper firstName  
      bigLastName = map toUpper lastName  
  putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"  
