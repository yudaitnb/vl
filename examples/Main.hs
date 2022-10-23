module Main where

import A
import B

-- main = let xx = unversion (g (version {A=1.0.0} of a))
--            yy = unversion (h (version {A=1.0.1} of a))
--        in xx + yy -- OK

main = let xx = (unversion g) a
           yy = (unversion h) a
       in xx + yy -- OK

-- main = let xx = (unversion (g a))
--            yy = (unversion (h a))
--        in xx + yy -- Rejected

-- 0. 各モジュールの各バージョンの型検査が終わった後に一回z3で制約を解き、最般型を求める (型だけが同型であればよい)
-- 1. 型検査の前に各外部モジュール変数の名前を区別し、TEnv中にそれらをコピーする（制約はどうする？）
-- 2. バンドリング：
--    a_bundled <= av1 && a_bundled <= av2 && a_bundled <= [Av1,Av2] ではなく、
--    ((a_bundled <= av1) || (a_bundled <= av2)) && a_bundled <= [Av1,Av2]
-- 3. コンパイルバックのために全てのシンボルについてベクトルを保存する

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