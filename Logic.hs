
module Logic where

boxesIntersect :: (Int,Int) -> (Int,Int) -> (Int,Int) -> (Int,Int) -> Bool
boxesIntersect a1 a2 b1 b2
  | (fst a2) < (fst b1) = False
  | (fst a1) > (fst b2) = False
  | (snd a2) < (snd b1) = False
  | (snd a1) > (snd b2) = False
  | otherwise           = True

