{-# OPTIONS_GHC -XFlexibleInstances -XUndecidableInstances #-}
{- Simple interfaces for domains based on
 -    http://matt.might.net/articles/partial-orders -}

module Domain where

import qualified Data.Set as S

class Domain t where
  (⊑) :: t -> t -> Bool
  (⊔) :: t -> t -> t
  bot :: t

x ⊒ y = y ⊑ x

joinAll :: Domain d => [d] -> d
joinAll = foldr (⊔) bot

instance Domain Integer where
  x ⊑ y = x <= y
  x ⊔ y = max x y
  bot   = 0

instance Ord e => Domain (S.Set e) where
  x ⊑ y = x ⊆ y
  x ⊔ y = x ∪ y
  bot   = S.empty

data Flat a = Bot | Flat a | Top deriving (Show, Ord, Bounded)

instance Ord a => Domain a where
  x ⊑ y = x <= y
  x ⊔ y = min x y
  -- Bot    ⊑ _      = True
  -- _      ⊑ Top    = True
  -- Flat x ⊑ Flat y = x == y
  -- _      ⊑ _      = False

  -- Bot    ⊔ x      = x
  -- x      ⊔ Bot    = x
  -- Top    ⊔ _      = Top
  -- _      ⊔ Top    = Top
  -- Flat x ⊔ Flat y 
  --   | x == y      = Flat x
  --   | otherwise   = Top
  bot             = minBound
  
  

lfp :: Domain d => (d -> d) -> d
lfp f = stable (iterate f bot)
  where stable (x:fx:tl) | x ⊒ fx    = x
                         | otherwise = stable (fx:tl)


x ⊆ y = S.isSubsetOf x y
x ⊇ y = y ⊆ x
x ∪ y = S.union x y
x ∩ y = S.intersection x y
(∅)   = S.empty

x ∈ s = S.member x s
s ∋ x = x ∈ s
x ∉ s = S.notMember x s
s ∌ x = x ∉ s
