module Main where

--import Miscs.Miscs as Miscs
--import Parser.String
--import Parser.Json
import Parser.JsonWithErrorHandling

main :: IO ()
main = do
  --Parser.String.test
  Parser.JsonWithErrorHandling.test
