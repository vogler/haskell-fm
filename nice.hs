{-# OPTIONS_GHC -XRank2Types #-}

{- Implements a translation between monadic parametric representation of RHS
 - and Strategy Trees. (Andrej Bauer, Martin Hofmann and Aleksandr Karbyshev.
 - "On Monadic Parametricity of Second-Order Functionals", FoSSaCS 2013.) -}
import Control.Monad
import Control.Monad.Cont
import Control.Monad.State

type V = String
type D = Integer
type Env = [(V,D)]
type Sol = V -> D

type RHS  = Monad m => (V -> m D) -> ((V,D) -> m ()) -> m D
data Tree = Answ D | Read V (D -> Tree) | Write (V,D) Tree
data Ops  = Wt V | Rd V deriving Show

depsFun :: Sol -> RHS -> [Ops]
depsFun rho f = reverse $ snd $ runState (f read write) []
  where
  read x      = get >>= put . (Rd x:) >> return (rho x)
  write (x,_) = get >>= put . (Wt x:)

evalFun :: Env -> RHS -> D
evalFun env f = fst (runState (f read write) env)
  where
  read x  = get >>= return . find x
  write x = get >>= put . (x:)

fun2tree :: RHS -> Tree
fun2tree f = runCont (f read write) Answ
  where 
  read x  = cont (Read x)
  write x = cont (\k -> Write x (k ()))

depsTree :: Sol -> Tree -> [Ops]
depsTree rho (Read x c)      = Rd x : depsTree rho (c (rho x))
depsTree rho (Write (x,_) c) = Wt x : depsTree rho c
depsTree _   (Answ x)        = []

evalTree :: Env -> Tree -> D
evalTree env (Answ x)        = x
evalTree env (Read  x c)     = evalTree env (c (find x env))
evalTree env (Write (x,d) c) = evalTree ((x,d):env) c 

-- For Testing.
f :: RHS
f get set = do x <- get "x"
               if (x > 4) 
               then do forM_ ["x","y","z"] (\x -> set (x, 42))
                       get "z"
               else do set ("x",x+1)
                       y <- get "x"
                       return (x+y)

test1 = [evalTree testenv (fun2tree f), evalFun testenv f]
test2 = [depsTree (mksol testenv) (fun2tree f), depsFun (mksol testenv) f]

find x = maybe 0 id . lookup x
testenv  = [("x",5), ("y",10)]
testenv' = [("x",2), ("y",10)]

mksol :: Env -> Sol
mksol = flip find
