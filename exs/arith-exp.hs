-------------------------                                                 
-- Arithmetic Expressions                                                 
-------------------------                                                 
                                                                          
                                                                          
-- values are always integers (for now)                                   
type Value = Integer                                                      
                                                                          
--------------------------------------------------------------------      
-- Abstract Syntax Tree for Expressions                                   
data Exp = ExpK Integer          -- constants                             
         | ExpAdd Exp Exp        -- e1 + e2                               
         | ExpSub Exp Exp        -- e1 - e2                               
         | ExpMul Exp Exp        -- e1 * e2                               
         | ExpDiv Exp Exp        -- e1 / e2                               
         | ExpNeg Exp            -- -e                                    
                                                                          
-- Evaluates an expression                                                
evalExp :: Exp -> Value                                                   
                                                                          
evalExp (ExpK i) = i                                                      
evalExp (ExpAdd e1 e2) = (evalExp e1) + (evalExp e2)                      
evalExp (ExpSub e1 e2) = (evalExp e1) - (evalExp e2)                      
evalExp (ExpMul e1 e2) = (evalExp e1) * (evalExp e2)                      
evalExp (ExpDiv e1 e2) = (evalExp e1)  `div` (evalExp e2)                 
evalExp (ExpNeg e) = -(evalExp e)                                         
                                                                          
                                                                          
-------------------------------------------------------------------       
-------------------------------------------------------------------       
-- an example                                                             
                                                                          
-- 34 + 52 * 3                                                            
exp1 = ExpAdd (ExpK 34) (ExpMul (ExpK 52) (ExpK 3))                       
                                                                          
                                                                          
-- code to show the value of an expression                                
main :: IO ()                                                             
main = print (evalExp exp1)                                               


       
