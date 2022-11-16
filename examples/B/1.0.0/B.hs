module B where

y = 1
b1 = 1
f x = x + 1

g x = x + 1

-- w x y = x + y

ll = length

-- map f xs = case xs of
--   [] -> []
--   hh : rst -> (f hh) : map f rst

length xs = case xs of
  [] -> 0
  hh : rst -> 1 + length rst

