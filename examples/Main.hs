module Main where

import A
import B

-- Sample programs
-- main = a + 1 -- OK

-- main = g a + h a -- Rejected

-- main =
--   let xx = g a
--       yy = h a
--   in xx + yy -- Rejected

-- main = let xx = unversion (g (version {A=1.0.0} of a))
--            yy = unversion (h (version {A=1.0.1} of a))
--        in xx + yy -- OK

main = let xx = (unversion g) a
           yy = (unversion h) a
       in xx + yy -- OK

-- main = let xx = (unversion (g a))
--            yy = (unversion (h a))
--        in xx + yy -- RejectedだがOKにしたい
