-----------------------------                                             
-- Simple Functional Language                                             
-----------------------------                                             
                                                                          
                                                                          
-- variables are just names                                               
type Var = String                                                         
                                                                          
-- values are integers and functions                                      
data Value = ValInt Integer                                               
           | ValFunc ([Value] -> Value)                                     
           | ValError String                                              
                                                                          
                                                                          
-- an Environment maps variables to Values                                
type Env = Var -> Value                                                   
                                                                          
                                                                          
-- auxiliary function to map Values to Booleans                           
isTrue :: Value -> Bool                                                   
isTrue (ValInt i) = (i /= 0)                                              
                                                                          
                                                                          
-- An empty Environment                                                   
emptyEnv :: Env                                                           
emptyEnv v = ValError ("undefined variable " ++ v)                        
                                                                          
                                                                          
-- binda new value in an environment                                     
bind :: Var -> Value -> Env -> Env                                        
bind var val env = \k -> if k == var then val else env k


--bind var val env = \v -> if var == v then val else env v                  
                                                                          
                                                                          
-- executes a binary operation on values                                  
binOp :: (Integer -> Integer -> Integer) -> Value -> Value -> Value       
binOp op (ValInt i1) (ValInt i2) = ValInt (op i1 i2)                      
binOp _ _ _ = ValError "binary operand is not a number"                   
                                                                          
fix :: (a -> a) -> a                                                      
fix f = x where x = f x                                                   
                                                                          
--------------------------------------------------------------------      
-- Abstract Syntax Tree for Expressions                                   
data Exp = ExpK Integer          -- constants                             
         | ExpVar Var            -- variables                             
         | ExpAdd Exp Exp        -- e1 + e2                               
         | ExpSub Exp Exp        -- e1 - e2                               
         | ExpMul Exp Exp        -- e1 * e2                               
         | ExpDiv Exp Exp        -- e1 / e2                               
         | ExpIf Exp Exp Exp     -- if e1 then e2 else e3                 
         | ExpApp Exp [Exp]      -- e1 e2                                 
         | ExpLambda [Var] Exp     -- \x -> e                               
         | ExpLet Var Exp Exp    -- letrec x=e1 in e2                     
                                                                          
-- Evaluates an expression in a given environment                         
evalExp :: Exp -> Env -> Value                                            
                                                                          
evalExp (ExpK i) env = ValInt i                                           
evalExp (ExpVar v) env = env v                                            
evalExp (ExpAdd e1 e2) env = binOp (+) (evalExp e1 env) (evalExp e2 env)  
evalExp (ExpSub e1 e2) env = binOp (-) (evalExp e1 env) (evalExp e2 env)  
evalExp (ExpMul e1 e2) env = binOp (*) (evalExp e1 env) (evalExp e2 env)  
evalExp (ExpDiv e1 e2) env = binOp div (evalExp e1 env) (evalExp e2 env)  
evalExp (ExpIf e1 e2 e3) env =                                            
    if isTrue(evalExp e1 env) then evalExp e2 env else evalExp e3 env

evalExp (ExpApp e l) env =                                              
  case (evalExp e env) of                                                
     ValFunc f -> f (evalList l env)                                       
     _        -> ValError "calling a non-function value"
                          
evalExp (ExpLambda lvars e) env =
  ValFunc (\lvals ->evalExp e (bindList lvars lvals env))
                                                                          
evalExp (ExpLet v e1 e2) env = evalExp e2 env'                            
  where env' = bind v (evalExp e1 env') env

evalList::[Exp] -> Env -> [Value]
evalList [] env = []
evalList (x:xs) env = (evalExp x env):(evalList xs env)

bindList:: [Var] -> [Value] -> Env -> Env
bindList [] [] env = env
bindList (r:rs) (l:ls) env =
  let env' = bind r l env
      in bindList rs ls env'

-- some examples

-- letrec f = \x -> if x then x * f(x - 1) else 1                         
-- in f 10                                                                
fat =                                                                     
  ExpLet "f"                                                              
    (ExpLambda ["x"]                                                        
       (ExpIf (ExpVar "x")                                                
              (ExpMul (ExpVar "x")                                        
                      (ExpApp (ExpVar "f") [(ExpSub (ExpVar "x") (ExpK 1))]))
              (ExpK 1)))                                                  
    (ExpApp (ExpVar "f") [(ExpK 3)])                                       
                                                                          
                                                                          
-- y = \f -> (\x -> f (x x)) (\x -> f (x x))                              
-- y = ExpLambda "f" (ExpApp y' y')                                          
--   where y' = ExpLambda "x"                                                
--                (ExpApp (ExpVar "f") (ExpApp (ExpVar "x") (ExpVar "x")))   
                                                                          
-- -- fatG = \f -> \x -> if x then x * f(x - 1) else 1                       
-- fatG =                                                                    
--   ExpLambda "f"                                                           
--     (ExpLambda "x"                                                        
--        (ExpIf (ExpVar "x")                                                
--               (ExpMul (ExpVar "x")                                        
--                       (ExpApp (ExpVar "f") (ExpSub (ExpVar "x") (ExpK 1))))
        
--               (ExpK 1)))                                                  
                                                                          
-- -- fat' = (y fatG) 20                                                     
-- fat' = ExpApp (ExpApp y fatG) (ExpK 20)                                   


-- code to show the final value of an expression                          
main :: IO ()                                                             
main = ((case (evalExp fat emptyEnv) of                                  
                          ValInt i -> print i                              
                          ValFunc _ -> print "function"                    
                          ValError err -> print ("error: " ++ err)))   
       
                                                                          
