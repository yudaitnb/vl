module Main where
import Matrix
import List

main = let vec = [2, 1]
           sorted = unversion (sortVector vec)
           m22 = join -- [[1,2],[2,1]]
                  (singleton sorted)
                  (singleton vec)
       in determinant m22 -- error

-- main = ...
--     -- sortVector
--     (let sortVector = \xs ->
--         case xs of
--             []  -> []
--             [x] -> [x]
--             xs  -> (\r -> (let vjoin = ...  in vjoin)
--                             (sortVector
--                               ((let init = ... in init) r))
--                             [(let last = ... in last) r])
--                    ((let bubble = ... in bubble) xs)
--     in sortVector)
--   ...
--     -- join
--     (let join = \xs -> \ys ->
--         case xs of
--             [] -> ys
--             x : xs -> (:) x (join xs ys)
--     in join)

-- module Main where

-- import A
-- import B

-- Sample programs
-- main = a + 1 -- OK

-- 2-1. "unversion" makes programs that require multiple versions acceptable.
-- （Identifying the cause of version inconsistencies is currently not implemented）
-- main = g a + h a -- Rejected
-- main = (unversion (g y)) + (unversion (h y)) -- OK

-- main = let xx = (unversion g) a
--            yy = (unversion h) a
--        in xx + yy -- OK

-- main = let xx = (unversion (g a))
--            yy = (unversion (h a))
--        in xx + yy -- duplicatingによってOKになった

-- main = let a' = a
--            xx = (unversion (g a'))
--            yy = (unversion (h a'))
--        in xx + yy -- rejected. ローカル変数はダメ。

-- main = let xx = (unversion (g (version {A=1.0.1} of a)))
--            yy = (unversion (h (version {A=1.0.0} of a)))
--        in xx + yy -- OK。gやhによって暗黙的にaのバージョンが決まっている。

-- main = let xx = (unversion (g (version {A=1.0.1} of a)))
--            yy = (unversion (h (version {A=1.0.1} of a)))
--        in xx + yy -- Rejected。v1.0.1のaは複数のBのバージョンに依存できないため。

-- import List

-- main =
--   let lst = [1, 2]
--   in reverse lst

-- main = 1

-- import Matrix

-- main =
--   let vec = [2, 1]
--       sorted = sortVector vec
--       m22 = join -- [[1,2],[2,1]]
--               (singleton sorted)
--               (singleton vec)
--   in determinant m22

-- main =
--   --determinant [[1,2],[2,1]]
--   let vec = [1,2]
--   in join [1,2] [2,1]