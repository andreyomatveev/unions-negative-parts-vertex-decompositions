module Main where

import UNPVD

main :: IO ()
main = do
    putStrLn ("\n" ++ "theorem7Dot5I 5 (3, 4) (3, 3, 3)    returns   "  ++ show (theorem7Dot5I 5 (3, 4) (3, 3, 3)))
    putStrLn ("\n" ++ "theorem7Dot5II 5 (3, 4) (5, 1, 3)    returns   "  ++ show (theorem7Dot5II 5 (3, 4) (5, 1, 3)))
    putStrLn ("\n" ++ "theorem7Dot5III 5 (4, 4) (5, 3, 3)    returns   "  ++ show (theorem7Dot5III 5 (4, 4) (5, 3, 3)))
    putStrLn ("\n" ++ "theorem7Dot5IV 5 (4, 3) (3, 3, 3)    returns   "  ++ show (theorem7Dot5IV 5 (4, 3) (3, 3, 3)))
    putStrLn ("\n" ++ "theorem7Dot5V 5 (3, 3) (1, 3, 5)    returns   "  ++ show (theorem7Dot5V 5 (3, 3) (1, 3, 5)) ++ "\n\n")

    putStrLn ("\n" ++ "theorem7Dot7I 5 (3, 4) (3, 3)    returns   "  ++ show (theorem7Dot7I 5 (3, 4) (3, 3)))
    putStrLn ("\n" ++ "theorem7Dot7II 5 (3, 3) (3, 3)    returns   "  ++ show (theorem7Dot7II 5 (3, 3) (3, 3)))
    putStrLn ("\n" ++ "theorem7Dot7III 5 (4, 4) (3, 3)    returns   "  ++ show (theorem7Dot7III 5 (4, 4) (3, 3)))
    putStrLn ("\n" ++ "theorem7Dot7IV 5 (4, 3) (3, 3)    returns   "  ++ show (theorem7Dot7IV 5 (4, 3) (3, 3)))
    putStrLn ("\n" ++ "theorem7Dot7V 5 (3, 4) (3, 3)    returns   "  ++ show (theorem7Dot7V 5 (3, 4) (3, 3)) ++ "\n\n")

    putStrLn ("\n" ++ "theorem7Dot9I 5 (3, 3, 2) (3, 3, 3, 1)    returns   "  ++ show (theorem7Dot9I 5 (3, 3, 2) (3, 3, 3, 1)))
    putStrLn ("\n" ++ "theorem7Dot9II 5 (3, 3, 2) (3, 1, 3, 3)    returns   "  ++ show (theorem7Dot9II 5 (3, 3, 2) (3, 1, 3, 3)))
    putStrLn ("\n" ++ "theorem7Dot9III 5 (2, 2, 1) (1, 3, 3, 1)    returns   "  ++ show (theorem7Dot9III 5 (2, 2, 1) (1, 3, 3, 1)))
    putStrLn ("\n" ++ "theorem7Dot9IV 5 (2, 3, 1) (1, 3, 3, 3)    returns   "  ++ show (theorem7Dot9IV 5 (2, 3, 1) (1, 3, 3, 3)))
    putStrLn ("\n" ++ "theorem7Dot9V 5 (3, 3, 2) (3, 3, 3, 3)    returns   "  ++ show (theorem7Dot9V 5 (3, 3, 2) (3, 3, 3, 3)))
    putStrLn ("\n" ++ "theorem7Dot9VI 5 (3, 2, 1) (3, 3, 3, 3)    returns   "  ++ show (theorem7Dot9VI 5 (3, 2, 1) (3, 3, 3, 3)))
    putStrLn ("\n" ++ "theorem7Dot9VII 5 (2, 2, 1) (1, 3, 1, 5)    returns   "  ++ show (theorem7Dot9VII 5 (2, 2, 1) (1, 3, 1, 5)))
    putStrLn ("\n" ++ "theorem7Dot9VIII 5 (2, 2, 1) (3, 3, 3, 3)    returns   "  ++ show (theorem7Dot9VIII 5 (2, 2, 1) (3, 3, 3, 3)))
    putStrLn ("\n" ++ "theorem7Dot9IX 5 (3, 2, 1) (3, 1, 3, 1)    returns   "  ++ show (theorem7Dot9IX 5 (3, 2, 1) (3, 1, 3, 1)))
    putStrLn ("\n" ++ "theorem7Dot9X 5 (2, 2, 1) (3, 3, 1, 3)    returns   "  ++ show (theorem7Dot9X 5 (2, 2, 1) (3, 3, 1, 3)) ++ "\n\n")

    putStrLn ("\n" ++ "theorem7Dot11I 5 (2, 2, 1) (3, 1, 3)    returns   "  ++ show (theorem7Dot11I 5 (2, 2, 1) (3, 1, 3)))
    putStrLn ("\n" ++ "theorem7Dot11II 5 (3, 3, 2) (1, 3, 3)    returns   "  ++ show (theorem7Dot11II 5 (3, 3, 2) (1, 3, 3)))
    putStrLn ("\n" ++ "theorem7Dot11III 5 (3, 2, 1) (1, 3, 1)    returns   "  ++ show (theorem7Dot11III 5 (3, 2, 1) (1, 3, 1)))
    putStrLn ("\n" ++ "theorem7Dot11IV 5 (3, 3, 2) (3, 5, 3)    returns   "  ++ show (theorem7Dot11IV 5 (3, 3, 2) (3, 5, 3)))
    putStrLn ("\n" ++ "theorem7Dot11V 5 (3, 3, 2) (3, 3, 3)    returns   "  ++ show (theorem7Dot11V 5 (3, 3, 2) (3, 3, 3)))
    putStrLn ("\n" ++ "theorem7Dot11VI 5 (3, 2, 1) (3, 3, 3)    returns   "  ++ show (theorem7Dot11VI 5 (3, 2, 1) (3, 3, 3)))
    putStrLn ("\n" ++ "theorem7Dot11VII 5 (2, 2, 1) (3, 1, 3)    returns   "  ++ show (theorem7Dot11VII 5 (2, 2, 1) (3, 1, 3)))
    putStrLn ("\n" ++ "theorem7Dot11VIII 5 (2, 2, 1) (3, 3, 3)    returns   "  ++ show (theorem7Dot11VIII 5 (2, 2, 1) (3, 3, 3)))
    putStrLn ("\n" ++ "theorem7Dot11IX 5 (2, 2, 1) (3, 3, 3)    returns   "  ++ show (theorem7Dot11IX 5 (2, 2, 1) (3, 3, 3)))
    putStrLn ("\n" ++ "theorem7Dot11X 5 (2, 2, 1) (3, 1, 3)    returns   "  ++ show (theorem7Dot11X 5 (2, 2, 1) (3, 1, 3)))
    

    
    




    
    



