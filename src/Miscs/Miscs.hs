module Miscs.Miscs where

import Text.Pretty.Simple (pPrint)
import Prelude

{--
   NOTE: foldl (+) is much more efficient than foldr (+).
   The two operations start diverging in time complexity around 10_000_000, where
   sumL does in ~400ms and sumR does in ~900ms. The rate of diverging accelarates
   for every additional zero.

   foldl is said to be
    - lazy in the accumulator
    - good for structs with efficient RTL sequencing and an operator with lazy left arg.

   this means that a list of Ring type (Num) has these features.
    - (+) is lazy in its left arg
    - list [] is efficient RTL sequencing.
--}
sumL :: (Num a) => a -> [a] -> a
sumL = foldl (+)

sumR :: (Num a) => a -> [a] -> a
sumR = foldr (+)

pull :: [a] -> [a] -> [a]
-- pull acc [] = acc
-- pull acc (x:xs) = pull (acc ++ [x]) xs
pull = foldr (:)

pull' :: [a] -> [a] -> [a]
-- pull' acc [] = acc
-- pull' acc (x:xs) = pull ([x] ++ acc) xs
pull' = foldl $ flip (:)

-- | a GADT for pretty printing the output of functions.
data Result where
  Result :: {title :: String, value :: String} -> Result
  deriving (Show)

test :: IO ()
test = do
  let s = "abcde"
  pPrint $
    Result
      { title = "sumL " ++ "[1..10_000_000]",
        value = show (sumL 0 [1 .. 10_000_000] :: Integer)
      }
  pPrint $
    Result
      { title = "sumR " ++ "[1..10_000_000]",
        value = show (sumR 0 [1 .. 10_000_000] :: Integer)
      }
  pPrint $
    Result
      { title = "pull one element at a time from " ++ s,
        value = pull [] s
      }
  pPrint $
    Result
      { title = "pull one element at a time from " ++ s ++ " (reversed)",
        value = pull' [] s
      }
