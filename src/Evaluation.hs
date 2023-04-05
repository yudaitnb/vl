module Evaluation (
    module System.TimeIt
  , printTimes
  , unlineTimes
  , formatTime
) where

import System.TimeIt ( timeIt, timeItT )
import Text.Printf (printf)
import Util

type TimesWithHeader = [(String, Double)]

printTimes :: TimesWithHeader -> IO ()
printTimes lst = print $ unlineTimes lst

unlineTimes :: TimesWithHeader -> Doc ann
unlineTimes lst =
  let widthH = maximum $ map (length . fst) lst
  in unlineTimes' widthH lst
  where
    unlineTimes' :: Int -> TimesWithHeader -> Doc ann
    unlineTimes' i         [] = pretty ""
    unlineTimes' i ((s,d):xs) = formatTime i s d <> line <> unlineTimes' i xs

formatTime :: Int -> String -> Double -> Doc ann
formatTime i h t = 
      fill i (pretty h)
  <+> colon
  <+> pretty (printf "%.3f s" t :: String)