{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use traverse" #-}

module Parser.Json where

import Control.Monad ((<=<), (>=>))
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

newtype Parser a = Parser
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
    -- Parser $ \s -> do
    --    (s1, f) <- pf s
    --    (s2, x) <- px s1
    --    Just (s2, f x)
    -- Parser $ \s ->
    --    pf s >>= \(s1, f) ->
    --    px s1 >>= \(s2, x) ->
    --    Just (s2, f x)
    Parser $
      pf
        >=> ( \(s1, f) ->
                px s1 >>= \(s2, x) ->
                  Just (s2, f x)
            )
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
