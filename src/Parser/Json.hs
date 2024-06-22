{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use traverse" #-}

module Parser.Json where

import Test.HUnit

import Control.Applicative (Alternative (empty, (<|>)))
import Prelude

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Integer -- NOTE: no support for floats
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

newtype Parser a
  = Parser
  { runParser :: String -> Maybe (String, a)
  }

{--
  FUNCTOR
--}
instance Functor Parser where
  fmap f (Parser px) =
    Parser $ h . px
   where
    h = fmap (fmap f)

{--
   The composing style works for `fmap` because `f` is first-ordered.
   We only need to peel off the layers of `Parser a` and then apply `f` to the inner most type.
   NOTE: `Parser a` is double-layered monad `m1 (m2 a)` where
      `m1 :: Maybe`
      `m2 :: Tuple`, more accurately the curried 2-tuple `(String, ...)`
--}

{--
  APPLICATIVE
--}
instance Applicative Parser where
  (<*>) (Parser pf) (Parser px) =
    Parser $ \s -> do
      (s', f) <- pf s
      (s'', x) <- px s'
      Just (s'', f x)
  pure x =
    Parser $ \s -> Just (s, x)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (<|>) (Parser p1) (Parser p2) =
    Parser $ \s -> p1 s <|> p2 s

{--
   MONAD
--}
instance Monad Parser where
  (>>=) (Parser px) f =
    Parser $ \s -> do
      (s', x) <- px s
      Parser g <- Just (f x)
      g s'

unwrap :: Parser a -> (String -> Maybe (String, a))
unwrap (Parser x) = x

-- | parses a single character from a string.
parseChar :: Char -> Parser Char
parseChar x = Parser f
 where
  f (y : ys) -- = Just (ys, y)
    | x == y = Just (ys, y)
    | otherwise = Nothing
  f [] = Nothing

pull :: [a] -> [a] -> [a]
pull = foldr (:)

parseString :: Parser String
parseString =
  Parser $ \s -> do
    Just (s, pull s [])

parseString1 :: String -> Parser String
parseString1 = sequenceA . map parseChar

parseString2 :: String -> Parser String
parseString2 = traverse parseChar

jsonBool :: Parser JsonValue
jsonBool = f <$> (parseString1 "true" <|> parseString1 "false")
 where
  f "true" = JsonBool True
  f "false" = JsonBool False
  f _ = undefined

-- NOTE: no proper error reporting
jsonValue :: Parser JsonValue
jsonValue = undefined

jsonNull :: Parser JsonValue
jsonNull = undefined

test :: IO ()
test = do
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
