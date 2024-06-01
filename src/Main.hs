module Main where

-- import Miscs.Miscs as Miscs
-- import Parser.String
-- import Parser.Jsonn
import Parser.JsonWithErrorHandling

main :: IO ()
main = do
  -- Parser.String.testt
  Parser.JsonWithErrorHandling.test
