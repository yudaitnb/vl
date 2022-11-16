module List where

head xs = case xs of
  []     -> 0
  hh:rst -> hh

length xs = case xs of
  []     -> 0
  hh:rst -> 1 + length rst

last xs = case xs of
  []     -> 0
  hh:[]  -> hh
  hh:rst -> last rst