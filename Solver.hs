{-# OPTIONS_GHC -XRank2Types #-}

{- Simple implementation of a local demand-driven solver. Using monadic
 - parametricity to ensure that RHS are pure functionals.  (Martin Hofmann,
 - Aleksandr Karbyshev and Helmut Seidl, "What is a pure functional?", ICALP 2010.) -}

module Solver where

import Control.Monad
import Control.Monad.Cont
import Control.Monad.State
import Data.Maybe
import Data.Set as S
import Data.Map as M
import Domain
import Debug.Trace

type RHS m v d = (v -> m d) -> m d
type Sys v d = Monad m => v -> RHS m v d
type Sol v d = Map v d

verify :: (Ord v, Domain d) => Sol v d -> Sys v d -> [v] -> Bool
verify σ f vars     = all (`M.member` σ) vars && isJust (mapM_ verifyVar (keys σ))
  where verifyVar v   = do l <- get v 
                           r <- (f v) get
                           guard (l ⊒ r)
        get v         = M.lookup v σ

sigma  (x,y,z) = x
infl   (x,y,z) = y
stable (x,y,z) = z
updSigma  f    = modify $ \(x,y,z) -> (f x,y,z)
updInfl   f    = modify $ \(x,y,z) -> (x,f y,z)
updStable f    = modify $ \(x,y,z) -> (x,y,f z)

solveAll :: (Ord v, Domain d) => Sys v d -> [v] -> Sol v d
solveAll f vs = sigma $ execState (solveAll' vs) initState
  where 
  initState = (M.empty, M.empty, S.empty)
  solveAll' = mapM_ solve

  solve x = do 
    s <- gets stable
    when (x ∉ s) $ do
      updStable (S.insert x)
      v <- (f x) (eval x)
      σ <- gets sigma
      let p = M.findWithDefault bot x σ 
      unless (v ⊑ p) $ do
        updSigma $ M.insert x (v ⊔ p)
        i <- gets infl
        let w = M.findWithDefault (∅) x i
        updStable (S.\\ w)
        updInfl $ M.insert x (∅)
        solveAll' (S.elems w)

  eval x y = do
    solve y
    updInfl $ M.insertWith (∪) y (S.singleton x)
    σ <- gets sigma
    return  (M.findWithDefault bot y σ)
  

-- For Testing.

data Vars = X1 | X2 | X3 deriving (Eq,Ord,Show)
f X1 get = do val <- get X3 
              return (S.fromList ['a'] ∪ val)
f X2 get = do val <- get X3 
              return (val ∩ S.fromList ['a','b'])
f X3 get = get X1 >>= \val -> return (val ∪ S.fromList ['c'])
