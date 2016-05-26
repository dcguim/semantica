----------------------                                                    
-- Regular Expressions                                                    
----------------------                                                    
exists :: [a] -> (a -> Bool) -> Bool                                      
exists l f = or (map f l)                                                 
                                                                          
                                                                          
data RE = REEmpty                                                         
        | REEpsilon                                                       
        | REAny                                                           
        | REChar Char                                                     
        | RESeq RE RE                                                     
        | REOr RE RE                                                      
        | REKleene RE
                                                                          
eval :: RE -> String -> Bool                                              
                                                                          
eval REEmpty s = False                                                    
                                                                          
eval REEpsilon s = (s == [])                                              
                                                                          
eval REAny s = (length s == 1)                                            
                                                                          
eval (REChar c) s = (s == [c])                                            
                                                                          
eval (REOr e e') s = eval e s || eval e' s                                
                                                                          
eval (RESeq e e') s = exists [0..length s] f                              
  where f i = eval e (take i s) && eval e' (drop i s)                     
                                                                          
eval (REKleene e) s = (s == []) || exists [1..length s] f                 
  where f i = eval e (take i s) && eval (REKleene e) (drop i s)           
                                                                          
----------------------------------------------                            
-- example: e1 = (b*)a                                                    
e1 = RESeq (REKleene (REChar 'b')) (REChar 'a')

-- main = print(eval  e1 "bbbbba")
-- main = print(eval (REChar 'b') "ab")
-- main = print(eval (REOr (REChar 'b') (REChar 'c')) "b")
main = print(eval (RESeq (REChar 'a') (REChar 'b')) "ab")
-- main = print()
