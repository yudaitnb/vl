module Test where

-- id x = x
-- y = 1
-- -- add1 x = x + 1
-- app x y = x y
main =
  let app x y = x y
      id x = x
      y = 1
  in app id y