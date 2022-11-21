module Main where

import A
import B

-- Sample programs

-- 1. All programs acceptable at STLC are also acceptable at VL.
-- main = a + 1 -- OK

-- 2-1. "unversion" makes programs that require multiple versions acceptable.
-- （Identifying the cause of version inconsistencies is currently not implemented）
main = g a + h a -- Rejected
-- main = (unversion (g y)) + (unversion (h y)) -- OK

-- 2-2. unversion can qualify any value.
-- main = let xx = (unversion g) a
--            yy = (unversion h) a
--        in xx + yy -- OK

-- 3-1. version ... of ... : user-defined version specification.
-- main = let xx = unversion (g (version {A=1.0.0} of a))
--            yy = unversion (h (version {A=1.0.1} of a))
--        in xx + yy -- OK

-- 3-2. Simply put, one version is determined for each "data flow".
-- The version of a is implicitly determined by g and h.
-- main = let xx = (unversion (g (version {A=1.0.1} of a)))
--            yy = (unversion (h (version {A=1.0.0} of a)))
--        in xx + yy -- OK

-- 3-3. Rejected because a v1.0.1 cannot depend on multiple B versions.
-- main = let xx = (unversion (g (version {A=1.0.1} of a)))
--            yy = (unversion (h (version {A=1.0.1} of a)))
--        in xx + yy -- Rejected

-- 4. Data consturctor requires that all elements have consistent versions.
-- (現状バージョン変数について単相だからそうなっているだけで、将来的には変更する予定)
-- main = [b1,b2] -- Rejected
-- main = (b1, y)  -- OK

-- 4-2. Pattern match (時間があったら)
-- main = 1
-- main = \x -> case x of (x1,x2) -> x1
-- main = [1, 2]
-- main = let ff = \x -> 1 in ff 1
-- main =
--   let fst x = case x of (x1,x2) -> x1
--   in fst (y,b2)

-- main =
--   let sumTpl x = case x of (x1,x2) -> x1 + x2
--   in sumTpl (y,b1)

-- main =
--   let lst = [y,b2]
--       n = length lst
--   in head lst

-- head xs = case xs of
--   []     -> 0
--   h:rst -> h

-- length xs = case xs of
--   []     -> 0
--   hh:rst -> 1 + length rst