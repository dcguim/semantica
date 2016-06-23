-----------------------------                                             
-- Simple Imperative Language                                             
-----------------------------                                             
                                                                          
                                                                          
-- variables are just names                                               
type Var = String                                                         
                                                                          
-- values are always integers (for now)                                   
type Value = Integer                                                      
                                                                          
-- a Memory maps variables to Values                                      
type Mem = Var -> Value                                                   
                                                                          
                                                                          
-- auxiliary function to map Values to Booleans                           
isTrue :: Value -> Bool                                                   
isTrue i = (i /= 0)                                                       
                                                                          
                                                                          
-- An empty memory                                                        
emptyMem :: Mem                                                           
emptyMem v = error ("invalid access to variable '" ++ v ++ "'")           
                                                                          
-- update the value of a variable in a memory                             
update :: Var -> Value -> Mem -> Mem                                      
update var val m = \v -> if v == var then val else m v                    
                                                                          
                                                                          
-- fixed-point operator                                                   
fix :: (a -> a) -> a                                                      
fix f = x                                                                 
  where x = f x                                                           
--------------------------------------------------------------------      
-- Abstract Syntax Tree for Expressions                                   
data Exp = ExpK Integer          -- constants                             
         | ExpVar Var            -- variables                             
         | ExpAdd Exp Exp        -- e1 + e2                               
         | ExpSub Exp Exp        -- e1 - e2                               
         | ExpMul Exp Exp        -- e1 * e2                               
         | ExpDiv Exp Exp        -- e1 / e2                               
         | ExpNeg Exp            -- -e                                    
                                                                          
-- Evaluates an expression in a given memory                              
evalExp :: Exp -> Mem -> Value                                            
                                                                          
evalExp (ExpK i) m = i                                                    
evalExp (ExpVar v) m = m v                                                
evalExp (ExpAdd e1 e2) m = (evalExp e1 m) + (evalExp e2 m)                
evalExp (ExpSub e1 e2) m = (evalExp e1 m) - (evalExp e2 m)                
evalExp (ExpMul e1 e2) m = (evalExp e1 m) * (evalExp e2 m)                
evalExp (ExpDiv e1 e2) m = (evalExp e1 m)  `div` (evalExp e2 m)           
evalExp (ExpNeg e) m = -(evalExp e m)                                     
                                                                          
                                                                          
----------------------------------------------------------------------    
-- Abstract Syntax Tree for Statements (commands)                         
data Cmd = CmdAsg Var Exp            -- assignment (var = exp)            
         | CmdIf Exp Cmd Cmd         -- if exp then c1 else c2            
         | CmdSeq Cmd Cmd            -- c1; c2                            
         | CmdWhile Exp Cmd          -- while e do c                      
         | CmdSkip                   -- do nothing                        
         | CmdRepeat Exp Exp                                        
       	 | CmdSeqs [Cmd]
         | CmdRepeatUntil Cmd Exp

evalCmd :: Cmd -> Mem -> Mem                                              
                                                                          
evalCmd (CmdSkip) m = m

evalCmd (CmdSeq c1 c2) m = evalCmd c2 (evalCmd c1 m)
                        
evalCmd (CmdIf e ct ce) m =                                               
  if isTrue(evalExp e m)                                                  
    then (evalCmd ct m) else (evalCmd ce m)                               

evalCmd (CmdAsg v e) m = update v (evalExp e m) m                         
                                                                          
evalCmd (CmdWhile e c) m = w m                                            
  where w = \m -> (if isTrue(evalExp e m) then w (evalCmd c m) else m)    
-- update the value of a variable in a memory                             
-- update :: Var -> Value -> Mem -> Mem                                      


evalCmd (CmdSeqs []) m = m
evalCmd (CmdSeqs (x:xs)) m =
  evalCmd (CmdSeqs xs) (evalCmd x m) 

evalCmd (CmdRepeatUntil cmd e) m = 
  let m' = (evalCmd cmd m)
      in if (isTrue(evalExp e m')) then m'
         else evalCmd (CmdRepeatUntil cmd e) m'                               
                                        
    

-- evalCmd (CmdRepeat n u) m =
-- 	if (isTrue(evalExp u m) <= 0) then m
-- 	else let m' = (evalExp n m)
-- 	     	 m'' = evalExp (ExpSub u (ExpK 1)) m'
--              	 in evalCmd (CmdRepeat n) m''
                                                                           
                                                                          
-------------------------------------------------------------------       
-------------------------------------------------------------------       
-- example                                                                
                                                                          
--y = 10; x = 1; while y do  x = x * y; y = y - 1                        
--cmd1 = CmdSeqs [(CmdSkip),
--                (CmdAsg "y" (ExpK 10))]

-- cmd2 = (CmdAsg "y"(CmdRepeatUntil  
--        	       	   (CmdSeq (CmdAsg "x" (ExpMul (ExpVar "x") (ExpVar "y")))
--                            (CmdAsg "y" (ExpSub (ExpVar "y") (ExpK 1))))
-- 			   (ExpVar "y"))

cmd2 = (CmdSeq 
	(CmdSeq (CmdAsg "y" (ExpK 0)) (CmdAsg "x" (ExpK 3)))
	(CmdRepeatUntil cmd (ExpVar "y"))
	)
       where cmd = (CmdIf (ExpVar "x")
			  (CmdAsg "x" (ExpSub (ExpVar "x") (ExpK 1)))
			  (CmdAsg "y" (ExpK 1)))

                                                                          
--cmd3 = CmdSeq (CmdSeq (CmdAsg "y" (ExpK 10))                              
--                      (CmdAsg "x" (ExpK 1)))                              
--              (CmdWhile (ExpVar "y")                                      
--                        (CmdSeq (CmdAsg "x" (ExpMul (ExpVar "x") (ExpVar "y")))
--                                (CmdAsg "y" (ExpSub (ExpVar "y") (ExpK 1)))))
                                                                          
-------------------------------------------------------------------       
-- code to show the final value of "x" after running "cmd1" on            
-- an initially empty memory                                              
                                                                          
finalMem = evalCmd cmd2 emptyMem                                          
                                                                          
main = print (finalMem "y")                                               
                                                                          
