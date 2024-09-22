module Main where

import Parser.Json
import Test.HUnit

main :: IO ()
main = do
  let
    tests =
      TestList
        [ TestCase
            ( let expected = Just ("aa", 'a')
                  actual = (unwrap $ parseChar 'a') "aaa"
                  title = "parse one char `a` from the string `aaa`, producing (\"aa\", 'a')"
               in assertEqual title expected actual
            )
        , TestCase
            ( let expected = Nothing
                  actual = (unwrap $ parseChar 'x') "aaa"
                  title = "parse one char `x` not in the string `aaa`, producing Nothing"
               in assertEqual title expected actual
            )
        , TestCase
            ( let expected = Just ("abcde", "abcde")
                  actual = unwrap parseString "abcde"
                  title = "parse en entire string, keeping the original for comparison"
               in assertEqual title expected actual
            )
        , TestCase
            ( let expected = Just ("defgh", "abc")
                  actual = (unwrap . parseString1) "abc" "abcdefgh"
                  title = "parse a substring `abc` from a string `abcdefgh"
               in assertEqual title expected actual
            )
        , TestCase
            ( let expected = Just ("defgh", "abc")
                  actual = (unwrap . parseString2) "abc" "abcdefgh"
                  title = "parse a substring `abc` from a string `abcdefgh`"
               in assertEqual title expected actual
            )
        , TestCase
            ( let expected = Nothing
                  actual = (unwrap . parseString1) "xbc" "abcdefgh"
                  title = "parse a substring `xbc` not in a string `abcdefgh`, producing Nothing"
               in assertEqual title expected actual
            )
        ]
  _ <- runTestTT tests
  pure ()
