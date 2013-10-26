{- Solving a constraint system by translating it into a vector function and
 - applying Kleene iteration. -}

module Simple where

import Domain
import qualified Data.Set as S

type RHS v d = (v -> d) -> d
type Sys v d = v -> RHS v d
type Sol v d = v -> d

class FSet v where vars :: [v]

instance (FSet v, Show v, Show d) => Show (v -> d) where
  show f = concatMap (\v -> show v ++ " -> " ++ show (f v) ++ "\n") vars

verify :: (FSet v, Domain d) => Sol v d -> Sys v d -> Bool
verify σ f      = all verifyVar vars where 
  verifyVar v   = σ v ⊒ f v σ

instance (FSet v, Domain d) => Domain (v -> d) 
  where
  f ⊑ g = all (\v -> f v ⊑ g v) vars
  f ⊔ g = \v -> f v ⊔ g v
  bot   = \v -> bot

solve :: (Domain d, FSet v) => Sys v d -> Sol v d
solve = lfp . flip

data V = X1 | X2 | X3 deriving (Eq,Show)
instance FSet V where vars = [X1,X2,X3]

sys X1 = \σ -> S.fromList ['a'] ∪ (σ X3)
sys X2 = \σ -> (σ X3) ∩ S.fromList ['a','b']
sys X3 = \σ -> (σ X1) ∪ S.fromList ['c']
