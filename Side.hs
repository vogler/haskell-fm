{-# OPTIONS_GHC -XRank2Types #-}

{- A purely functional solver for Side-Effecting Constraint Systems.
 - (Kalmer Apinis, Helmut Seidl, and Vesal Vojdani. "Side-Effecting Constraint
 - Systems: A Swiss Army Knife for Program Analysis", APLAS 2012.) -}

import Control.Monad
import Control.Monad.Cont
import Control.Monad.State
import Data.Maybe
import Data.Set as S
import Data.Map as M
import Domain

type RHS m v d = (v -> m d) -> (v -> d -> m ()) -> m d
type Sys v d = Monad m => v -> RHS m v d
type Sol v d = Map v d

verify :: (Ord v, Domain d) => Sol v d -> Sys v d -> [v] -> Bool
verify σ f vs       = all (`M.member` σ) vs && 
                      isJust (mapM_ verifyVar (keys σ))
  where verifyVar v = f v get check >>= check v
        get v       = M.lookup v σ
        check v r   = do l <- M.lookup v σ 
                         guard (l ⊒ r)

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
    v <- (f x) (eval x) set
    set x v

  eval x y = do
    solve y
    updInfl $ M.insertWith (∪) y (S.singleton x)
    σ <- gets sigma
    return  (M.findWithDefault bot y σ)
  
  set x v = do
    σ <- gets sigma
    when (x `M.notMember` σ) (solve x)
    let p = M.findWithDefault bot x σ 
    unless (v ⊑ p) $ do
      updSigma $ M.insert x (v ⊔ p)
      i <- gets infl
      let w = M.findWithDefault (∅) x i
      updStable (S.\\ w)
      updInfl $ M.insert x (∅)
      solveAll' (S.elems w)


-- For Testing.
f :: Monad m => RHS m String Integer
f get set = do x <- get "x"
               if (x > 4) 
               then do forM_ ["x","y","z"] (\x -> set x 42)
                       get "z"
               else do set "x" (x+1)
                       y <- get "x"
                       return (x+y)
