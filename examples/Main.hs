module Main where

import A
import B

-- main = let xx = unversion (g (version {A=1.0.0} of a))
--            yy = unversion (h (version {A=1.0.1} of a))
--        in xx + yy -- OK

-- main = let xx = (unversion g) a
--            yy = (unversion h) a
--        in xx + yy -- OK

main = let xx = (unversion (g a))
           yy = (unversion (h a))
       in xx + yy -- Rejected

-- a67 <= (B:1.0.0), (bundled w)
-- a103 <= a80, a80 <= a79, a79 <= a68, a68 <= (B:1.0.0), (B:1.0.1), (bundled y)
-- a103 <= a65, a65 <= (B:1.0.0), (bundled g)
-- a97 <= a80, a80 <= a79, a79 <= a68, a68 <= (B:1.0.0), (B:1.0.1), (bundled y)
-- a97 <= a66, a66 <= (B:1.0.1), (bundled h)

-- a : [...]_{a0}
-- old
-- g a -> a1 < a0 and a1 < ag
-- h a -> a2 < a0 and a2 < ah

-- new
-- a^ : a01 a01 < a0
-- a_ : a02 a02 < a0
-- g a^ -> a1 < a01 and a1 < ag
-- h a_ -> a2 < a01 and a2 < ah

-- 2引数関数の時めんどくさそう

-- main = let w' = (unversion w)
--            xx = g a
--            yy = h a
--        in w' xx yy -- error

-- main = let w' = (unversion w)
--            xx = unversion (g a)
--            yy = unversion (h a)
--        in w' xx yy -- OK

-- a : [Int]_a??
-- g : [!_a??{Int} -> Int]_a??

-- 関数の取り扱い of dynamic化 in soft typing 

-- main = g a + h a

-- main =
--   let xx = g a
--       yy = h a
--   in xx + yy

-- main = a + 1