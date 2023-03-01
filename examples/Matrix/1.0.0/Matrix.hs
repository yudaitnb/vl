module Matrix where

import List

determinant mx = sum (map
    (\xs -> (product (pick 1 xs mx)) * (sign xs))
    (mkPerm (length mx)))
pick c xs mx = case xs of
  [] -> []
  _  -> (index (head xs) (index c mx)) : (pick (c+1) (tail xs) mx)

sign xs = if (mod2 (inversion xs)) > 0 then -1 else 1
inversion xs = case xs of
  x:[] -> 0
  x:xs -> sum (map (\y -> if x > y then 1 else 0) xs) + inversion xs

index c xs = last (take c xs)

mod2 n = case n of
  0 -> 0
  1 -> 1
  _ -> mod2 (n-2)

mkPerm n = permutations (mkLst n)
mkLst n = reverse (mkLst' n)
mkLst' n = case n of
  0 -> []
  _ -> n : mkLst' (n-1) 

join xs ys = case xs of
  []   -> ys
  x:xs -> x : (join xs ys)