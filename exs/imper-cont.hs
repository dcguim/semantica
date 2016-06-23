

--------------------------                                                
-- Imperative Language CPS                                                
--------------------------                                                

-- variables are just names                                               
type Var = String                                                         
                                                                          
-- values are always integers (for now)                                   
type Value = Int                                                          
                                                                          
-- a Memory maps variables to Values                                      
type Mem = Var -> Value                                                   
                                                                          
-- final result of a program                                              
type Result = String                                                      
                                                                          
                                                                          
-- a continuation finishes the execution of a program                     
type K = Mem -> Result                                                    
                                                                          
                                                                          
-- auxiliary function to map Values to Booleans                           
isTrue :: Value -> Bool                                                   
isTrue i = (i /= 0)                                                       
                                                                          
                                                                          
-- auxiliary function to map Values to Booleans                           
bool2Int :: Bool -> Value                                                 
bool2Int True = 1                                                         
bool2Int False = 0                                                        
                                                                          
                                                                          
-- An empty memory                                                        
emptyMem :: Mem                                                           
emptyMem v = 0                                                            
                                                                          
-- update the value of a variable in a memory                             
update :: Var -> Value -> Mem -> Mem                                      
update var val m = \v -> if var == v then val else m v                    
                                                                          
                                                                          
--------------------------------------------------------------------      
-- Abstract Syntax Tree for Expressions                                   
data Exp = ExpK Int              -- constants                             
         | ExpVar Var            -- variables                             
         | ExpAdd Exp Exp        -- e1 + e2                               
         | ExpSub Exp Exp        -- e1 - e2                               
         | ExpMul Exp Exp        -- e1 * e2                               
         | ExpDiv Exp Exp        -- e1 / e2                               
         | ExpAnd Exp Exp        -- e1 & e2                               
         | ExpOr Exp Exp         -- e1 | e2                               
         | ExpNot Exp            -- !e                                    
         | ExpNeg Exp            -- -e                                    
                                                                          
-- Evaluates an expression in a given memory                              
evalExp :: Exp -> Mem -> Value                                            
                                                                          
evalExp (ExpK i) m = i                                                    
evalExp (ExpVar v) m = m v                                                
evalExp (ExpAdd e1 e2) m = (evalExp e1 m) + (evalExp e2 m)                
evalExp (ExpSub e1 e2) m = (evalExp e1 m) - (evalExp e2 m)                
evalExp (ExpMul e1 e2) m = (evalExp e1 m) * (evalExp e2 m)                
evalExp (ExpDiv e1 e2) m = (evalExp e1 m)  `div` (evalExp e2 m)           
evalExp (ExpAnd e1 e2) m =                                                
    bool2Int (isTrue(evalExp e1 m)  && isTrue(evalExp e2 m))              
evalExp (ExpOr e1 e2) m =                                                 
    bool2Int(isTrue(evalExp e1 m)  || isTrue(evalExp e2 m))               
evalExp (ExpNeg e) m = -(evalExp e m)                                     
evalExp (ExpNot e) m = bool2Int(not (isTrue(evalExp e m)))                
                                                                          
                                                                          
----------------------------------------------------------------------    
-- Abstract Syntax Tree for Statements (commands)                         
data Cmd = CmdAss Var Exp            -- assignment (var = exp)            
         | CmdIf Exp Cmd Cmd         -- if exp then c1 else c2            
         | CmdComp [Cmd]             -- c1; c2                            
         | CmdWhile Exp Cmd          -- while e do c                      
         | CmdSkip                   -- do nothing
         | CmdRepeat Cmd Exp         -- Repeat cmd until exp

-- a continuation finishes the execution of a program                     
-- type K = Mem -> Result
-- a Memory maps variables to Values                                      
-- type Mem = Var -> Value
evalCmd:: Cmd -> K -> Mem -> Result                                      
                                                                          
evalCmd (CmdSkip) k m = k m
-- evalCmd (CmdComp c1 c2) k m = evalCmd c1 (evalCmd c2 k) m 
evalCmd (CmdComp []) k m = k m
evalCmd (CmdComp (x:xs)) k m = k' m
  where k' = \m -> (evalCmd x (evalCmd (CmdComp xs) k) m)

evalCmd (CmdIf e ct ce) k m =                                             
  if isTrue(evalExp e m)                                                  
    then (evalCmd ct k m) else (evalCmd ce k m)                           
evalCmd (CmdAss v e) k m = k (update v (evalExp e m) m)                   
evalCmd (CmdWhile e c) k m = k' m                                         
  where k' = \m -> (if isTrue(evalExp e m) then evalCmd c k' m            
                                           else k m)                      

evalCmd (CmdRepeat c e) k m =
  let r = (evalCmd c k)
  in if (isTrue(evalExp e m)) then k m
     else evalCmd c r m 

--evalList::[Cmds] -> K -> Mem -> Result
--evalList 
-------------------------------------------------------------------       
-- some examples                                                          
                                                                          
-- (34 + 52) or 0                                                         
exp1 = ExpOr (ExpAdd (ExpK 34) (ExpK 52)) (ExpK 0)                        
                                                                          
-- y = 10; x = 0; while y do y = x - 1; x = x + 1                         
-- cmd1 = CmdComp                                                            
--          (CmdAss "y" (ExpK 10))                                           
--        (CmdComp                                                           
--          (CmdAss "x" (ExpK 0))                                            
--          (CmdWhile (ExpVar "y")                                           
--                    (CmdComp                                               
--                      (CmdAss "y" (ExpSub (ExpVar "y") (ExpK 1)))          
--                      (CmdAss "x" (ExpAdd (ExpVar "x") (ExpK 1))))))
         
-- cmd2 = (CmdComp 
-- 	(CmdComp (CmdAss "y" (ExpK 0)) (CmdAss "x" (ExpK 3)))
-- 	(CmdRepeat cmd (ExpVar "y"))
-- 	)
--        where cmd = (CmdIf (ExpVar "x")
-- 			  (CmdAss "x" (ExpSub (ExpVar "x") (ExpK 1)))
-- 			  (CmdAss "y" (ExpK 1)))

cmd3 = (CmdComp [(CmdAss "x" (ExpK 10)),
     	         (CmdAss "x" (ExpK 20))])
                                                                                                                                                  
-------------------------------------------------------------------       
-- code to show the final value of "x" after running "cmd1" on            
-- an initially empty memory                                              
                                                                          
-- initial continuation: get the value of "x" from the memory             
ik :: K                                                                   
ik m = show (m "x")                                                       
                                                                          
main :: IO ()                                                             
main = print (evalCmd cmd3 ik emptyMem)                                   
                                                                          

