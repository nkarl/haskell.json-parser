module Miscs.Miscs where

import Text.Pretty.Simple (pPrint)
import Prelude

pull :: [a] -> [a] -> [a]
pull = foldr (:)

pullreverse :: [a] -> [a] -> [a]
pullreverse = foldl $ flip (:)

mySum :: (Num a) => a -> [a] -> a
mySum = foldl (+)

mySumR :: (Num a) => a -> [a] -> a
mySumR = foldr $ flip (+)

data Result = Result
  { name :: String,
    result :: Int
  }
  deriving (Show)

test :: IO ()
test = do
  let s = "abcde"
  pPrint $ Result {name = "mySum", result = mySum 0 [1, 2, 3]}
  pPrint $ "mySumR" ++ show (mySumR 0 [1, 2, 3])
  pPrint $ "pull " ++ s ++ " " ++ show (pull [] s)
  pPrint $ "pull (reversed) " ++ s ++ " " ++ show (pullreverse [] s)
