module List where

concat xs ys = case xs of
  []   -> ys
  x:xs -> concat (x : xs)  ys

head xs = case xs of
  []    -> 0
  h:rst -> h

length xs = case xs of
  []     -> 0
  hh:rst -> 1 + length rst

last xs = case xs of
  []    -> 0
  h:[]  -> h
  h:rst -> last rst

tail xs = case xs of
  []    -> []
  h:rst -> rst

-- init []                 =  errorEmptyList "init"
-- init (x:xs)             =  init' x xs
--   where init' _ []     = []
--         init' y (z:zs) = y : init' z zs