------------------------------------------------------                    
-- Functional language with side effects monadic-style                    
------------------------------------------------------                    
                                                                          
-- variables are just names                                               
type Var = String                                                         
                                                                          
                                                                          
-- locations are just numbers (addresses)                                 
type Location = Int                                                       
                                                                          
                                                                          
-- values are integers and functions                                      
data Value = ValInt Integer                                               
           | ValFunc (Value -> Cmpt Value)                                
           | ValError                                                     
                                                                          
                                                                          
-- an Environment maps variables to Locations                             
type Env = [(Var, Location)]                                              
                                                                          
                                                                          
-- a Memory maps locations to Values                                      
type Mem = [(Location, Value)]                                            
                                                                          
                                                                          
-- auxiliary function to map Values to Booleans                           
isTrue :: Value -> Bool                                                   
isTrue (ValInt i) = (i /= 0)                                              
                                                                          
                                                                          
-- the "side-effect" of an expression                                     
type Cmpt a = Mem -> (a, Mem)                                             
                                                                          
                                                                          
-- transforms a value into a computation                                  
op0 :: a -> Cmpt a                                                        
op0 v = \m -> (v, m)                                                      
                                                                          
-- executes an unary operation on computations                            
op1 :: Cmpt a -> (a -> Cmpt b) -> Cmpt b                                  
op1 ca op m = op v m'                                                     
  where (v, m') = ca m                                                    
                                                                          
cerror _ = op0 ValError                                                   
                                                                          
                                                                          
query :: Eq a => [(a, b)] -> a -> b                                       
query ((y, v):l) x | (x == y) = v                                         
query (_:l) x | otherwise = query l x                                     
                                                                          
update :: a -> b -> [(a, b)] -> [(a, b)]                                  
update k v m = (k,v):m                                                    
                                                                          
                                                                          
queryVar :: Location -> Cmpt Value                                        
queryVar loc m = (query m loc, m)                                         
                                                                          
                                                                          
updateVar :: Location -> Cmpt Value -> Cmpt Value                         
updateVar loc c1 = op1 c1 f                                               
  where f v = \m -> (v, update loc v m)                                   
                                                                          
                                                                          
-- find a free location in a memory                                       
freeM :: Cmpt Location                                                    
freeM = \m -> (free m, m)                                                 
                                                                          
free :: Mem -> Location                                                   
free [] = 1                                                               
free ((l,v):m') = (max l (free m')) + 1                                   
                                                                          
                                                                          
-- bind a new value in a new location                                     
bind :: Var -> Value -> Env -> Cmpt Env                                   
bind var val env = op1 freeM f                                            
  where f l = op1 (updateVar l (op0 val)) (\_ -> op0 (update var l env))  
                                                                          
                                                                          
-- executes a binary operation on computations                            
op2 :: (a -> b -> Cmpt c) -> Cmpt a -> Cmpt b -> Cmpt c                   
op2 op ca cb = op1 ca (\a -> op1 cb (op a))                               
                                                                          
-- executes a binary integer operation on computations                    
arith :: (Integer -> Integer -> Integer) ->                               
             Cmpt Value -> Cmpt Value -> Cmpt Value                       
arith op = op2 op_aux                                                     
  where op_aux (ValInt i1) (ValInt i2) = op0 (ValInt (op i1 i2))          
        op_aux _ _ = cerror "binary operation over non-int value"         
                                                                          
                                                                          
--------------------------------------------------------------------      
-- Abstract Syntax Tree for Expressions                                   
data Exp = ExpK Integer          -- constants                             
         | ExpVar Var            -- variables                             
         | ExpAssg Var Exp       -- v := e                                
         | ExpAdd Exp Exp        -- e1 + e2                               
         | ExpSub Exp Exp        -- e1 - e2                               
         | ExpMul Exp Exp        -- e1 * e2                               
         | ExpDiv Exp Exp        -- e1 / e2                               
         | ExpIf Exp Exp Exp     -- if e1 then e2 else e3                 
         | ExpApp Exp Exp        -- e1 e2                                 
         | ExpLambda Var Exp     -- \x -> e                               
         | ExpLet Var Var Exp Exp    -- letrec x=e1 in e2                 
         | ExpWhile Exp Exp                                                                 
                                                                          
closure :: Var -> Exp -> Env -> Value                                     
closure v e env = ValFunc f                                               
  where f x = op1 (bind v x env) (\env' -> evalExp e env')                
                                                                          
                                                                          
-- Evaluates an expression in a given environment                         
evalExp :: Exp -> Env -> Cmpt Value                                       
                                                                          
evalExp (ExpK i) env = op0 (ValInt i)                                     
evalExp (ExpVar var) env = queryVar (query env var)                       
                                                                          
evalExp (ExpAssg var e) env = updateVar (query env var) (evalExp e env)   
                                                                          
evalExp (ExpAdd e1 e2) env = arith (+) (evalExp e1 env) (evalExp e2 env)  
evalExp (ExpSub e1 e2) env = arith (-) (evalExp e1 env) (evalExp e2 env)  
evalExp (ExpMul e1 e2) env = arith (*) (evalExp e1 env) (evalExp e2 env)  
evalExp (ExpDiv e1 e2) env = arith div (evalExp e1 env) (evalExp e2 env)  
                                                                          
evalExp (ExpIf e1 e2 e3) env = op1 (evalExp e1 env) f                     
  where f (ValInt 0) = evalExp e3 env                                     
        f (ValInt _) = evalExp e2 env                                     
        f _ = cerror "invalid value for 'if'"                             
                                                                          
evalExp (ExpApp e1 e2) env = op2 app (evalExp e1 env) (evalExp e2 env)    
  where app (ValFunc f) vp = f vp                                         
        app _ _ = cerror "attempt to call a non-function value"           
                                                                          
evalExp (ExpLambda v e) env = op0 (closure v e env)                       
                                                                          
evalExp (ExpLet v v' e' e) env = op1 freeM f                              
  where f l = let env' = update v l env;                                  
                  c = updateVar l (op0 (closure v' e' env')) in           
                op1 c (\_ -> evalExp e env')

evalExp w@(ExpWhile e c) env = op1 (evalExp e env) f
  where f (ValInt 0) = op0 (ValInt 0)                       
        f (ValInt _) = op1 (evalExp c env) (\_ -> evalExp (ExpAdd (ExpK 1) w) env)        
        f _ = cerror "The expression given does not evaluate to an integer value"                                                                  
--------------------------------------------------------------------------
-------------------------------------------------------------------       
-- examples                                                               
                                                                          
comma :: Exp -> Exp -> Exp                                                
comma e1 e2 = ExpApp (ExpLambda "_" e2) e1                                
                                                                          
                                                                          
-- (\x -> (x := x + 10); x) 15                                            
t1 = ExpApp                                                               
       (ExpLambda "x"                                                     
           ((ExpAssg "x" (ExpAdd (ExpVar "x") (ExpK 10)))                 
               `comma`                                                    
            (ExpVar "x")))                                                
       (ExpK 15)                                                          
                                                                          
                                                                          
-- letrec f = (\x -> if x then x * f(x - 1) else 1) in f 10               
t2 = ExpLet "f" "x"                                                       
      (ExpIf (ExpVar "x")                                                 
             (ExpMul (ExpVar "x")                                         
                     (ExpApp (ExpVar "f") (ExpSub (ExpVar "x") (ExpK 1))))
             (ExpK 1))                                                    
      (ExpApp (ExpVar "f") (ExpK 10))                                     

t3 = (ExpWhile (ExpK 5) (ExpK 5))
-- code to show the final value of an expression                          
main :: IO ()                                                             
main = print ((case (evalExp t3 [] []) of                                 
                         (ValInt i, _) -> i                               
                         _        -> -1))                                 
                                                                          

