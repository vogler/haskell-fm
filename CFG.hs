module CFG where

import ParseWhile
import Control.Monad.State

data Cmd  = Ass String AExpr | Guard BExpr

instance Show Cmd where
  show (Ass v e) = v ++ ":=" ++ show e
  show (Guard e) = show e

type CFG  = [(Int,Cmd,Int)] 

fresh :: State Int Int
fresh = do n <- get
           put (n+1)
           return n

genCFG :: Stmt -> CFG
genCFG stmt = evalState (genCFG' (0,-1) stmt) 1

genCFG' :: (Int,Int) -> Stmt -> State Int CFG
genCFG' (i,o) (Assign s e) = 
  return [(i, Ass s e, o)]
genCFG' (i,o) (If e s1 s2) = do 
  n1 <- fresh
  c1 <- genCFG' (n1,o) s1
  n2 <- fresh 
  c2 <- genCFG' (n2,o) s2
  return ((i, Guard e, n1) : (i, Guard (Not e), n2) : c1 ++ c2)
genCFG' (i,o) (While e s) = do 
  n' <- fresh
  c  <- genCFG' (n',i) s
  return ((i, Guard e, n') : (i, Guard (Not e), o) : c)
genCFG' (i,o) (Seq [])     = return []
genCFG' (i,o) (Seq [x])    = genCFG' (i,o) x
genCFG' (i,o) (Seq (x:xs)) = do 
  n' <- fresh
  c1 <- genCFG' (i,n') x
  c2 <- genCFG' (n',o) (Seq xs)
  return (c1++c2)

test = do ast <- parseFile "test.while"
          let cfg = genCFG ast
          mapM_ (putStrLn . show) cfg
