module Main where

import Miscs.Miscs as Miscs
import Parser.Parser as Parser

main :: IO ()
main = do
  Parser.test
  Miscs.test
