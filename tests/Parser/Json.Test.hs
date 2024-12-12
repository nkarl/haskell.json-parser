module Main where

import Parser.Json
import Test.HUnit
import Control.Monad (void)

main :: IO ()
main = do
  let
    tests =
      TestList
        [ TestCase
            ( let expected = Just ("aa", 'a')
                  actual = (unwrap $ parseChar 'a') "aaa"
                  testName = "parse one char `a` from the string `aaa`, producing (\"aa\", 'a')"
               in assertEqual testName expected actual
            )
        , TestCase
            ( let expected = Nothing
                  actual = (unwrap $ parseChar 'x') "aaa"
                  testName = "parse one char `x` not in the string `aaa`, producing Nothing"
               in assertEqual testName expected actual
            )
        , TestCase
            ( let expected = Just ("abcde", "abcde")
                  actual = unwrap parseString "abcde"
                  testName = "parse en entire string, keeping the original for comparison"
               in assertEqual testName expected actual
            )
        , TestCase
            ( let expected = Just ("defgh", "abc")
                  actual = (unwrap . parseString1) "abc" "abcdefgh"
                  testName = "parse a substring `abc` from a string `abcdefgh"
               in assertEqual testName expected actual
            )
        , TestCase
            ( let expected = Just ("defgh", "abc")
                  actual = (unwrap . parseString2) "abc" "abcdefgh"
                  testName = "parse a substring `abc` from a string `abcdefgh`"
               in assertEqual testName expected actual
            )
        , TestCase
            ( let expected = Nothing
                  actual = (unwrap . parseString1) "xbc" "abcdefgh"
                  testName = "parse a substring `xbc` not in a string `abcdefgh`, producing Nothing"
               in assertEqual testName expected actual
            )
        ]
  void $ runTestTT tests
