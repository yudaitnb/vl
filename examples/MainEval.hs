module Main where
import List

main = let vec = [2, 1]
           sorted = unversion (sortVector vec)
           m22 = join -- [[1,2],[2,1]]
                  (singleton sorted)
                  (singleton vec)
       in determinant m22