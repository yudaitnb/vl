module List where

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