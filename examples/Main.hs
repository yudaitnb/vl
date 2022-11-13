module Main where

import A
import B

-- Sample programs
-- main = a + 1 -- OK

-- main = g a + h a -- Rejected
-- main = (unversion (g y)) + (unversion (h y)) -- RejectedだがOKにしたい

-- main =
--   let xx = g a
--       yy = h a
--   in xx + yy -- Rejected

-- main = let xx = unversion (g (version {A=1.0.0} of a))
--            yy = unversion (h (version {A=1.0.1} of a))
--        in xx + yy -- OK

-- main = let xx = (unversion g) a
--            yy = (unversion h) a
--        in xx + yy -- OK

-- main = let xx = (unversion (g a))
--            yy = (unversion (h a))
--        in xx + yy -- duplicatingによってOKになった

-- main = let xx = (unversion (g (version {A=1.0.1} of a)))
--            yy = (unversion (h (version {A=1.0.0} of a)))
--        in xx + yy -- OK。gやhによって暗黙的にaのバージョンが決まっている。

-- main = let xx = (unversion (g (version {A=1.0.1} of a)))
--            yy = (unversion (h (version {A=1.0.1} of a)))
--        in xx + yy -- Rejected。v1.0.1のaは複数のBのバージョンに依存できないため。

-- main = 1
-- main = \x -> case x of (x1,x2) -> x1
-- main = [1, 2]
-- main = let ff = \x -> 1 in ff 1
-- main =
--   let fst = \x -> case x of (x1,x2) -> x1
--   in fst (y,b1)

main =
  let sumTpl = \x -> case x of (x1,x2) -> x1 + x2
  in sumTpl (y,b1)

-- main = (\x -> x) 1