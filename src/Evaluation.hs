module Evaluation where

import Text.Printf (printf)
import Data.Time
import Control.Monad.IO.Class (MonadIO(liftIO))
import System.CPUTime (getCPUTime)

import Util

type TimesWOfhHeader = [(String, Double)]

printTimes :: TimesWOfhHeader -> IO ()
printTimes lst = print $ unlineTimes lst

unlineTimes :: TimesWOfhHeader -> Doc ann
unlineTimes lst =
  let widthH = maximum $ map (length . fst) lst
  in unlineTimes' widthH lst
  where
    unlineTimes' :: Int -> TimesWOfhHeader -> Doc ann
    unlineTimes' i         [] = pretty ""
    unlineTimes' i ((s,d):xs) = formatTimeWOfhHeader i s d <> line <> unlineTimes' i xs

formatTimeWOfhHeader :: Int -> String -> Double -> Doc ann
formatTimeWOfhHeader i h t = 
      fill i (pretty h)
  <+> colon
  -- <+> pretty (show t)
  <+> pretty (printf "%.3f s" t :: String)



timeOf :: MonadIO m => m a -> m a
timeOf = timeOfNamed "Real time"

timeOfNamed :: MonadIO m => String -> m a -> m a
timeOfNamed name ioa = do
    (t, a) <- timeOfT ioa
    liftIO $ printf (name ++ ": %6.2fs\n") t
    return a

timeOfT :: MonadIO m => m a -> m (Double, a)
timeOfT ioa = do
    ct1 <- liftIO getCurrentTime
    a <- ioa
    ct2 <- liftIO getCurrentTime
    let ctdiff :: Double
        ctdiff = realToFrac $ diffUTCTime ct2 ct1
    return (ctdiff, a)

timeOfWFFI :: MonadIO m => m a -> m (Double, Double, a)
timeOfWFFI ioa = do
    ct1 <- liftIO getCurrentTime
    cput1 <- liftIO getCPUTime
    a <- ioa
    cput2 <- liftIO getCPUTime
    ct2 <- liftIO getCurrentTime
    let ctdiff, cputdiff :: Double
        cputdiff = fromIntegral (cput2 - cput1) * 1e-12
        ctdiff = realToFrac $ diffUTCTime ct2 ct1
    return (ctdiff, cputdiff, a)