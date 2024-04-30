{-# LANGUAGE LambdaCase #-}

module Main where

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

-- | parses a single character from a string.
charP :: Char -> Parser Char
charP x = Parser $ \case
  y : ys
    | y == x -> Just (ys, x)
    | otherwise -> Nothing
  [] -> Nothing

-- NOTE: no proper error reporting
jsonValue :: Parser JsonValue
jsonValue = undefined

jsonNull :: Parser JsonValue
jsonNull = undefined

main :: IO ()
main = putStrLn "Hello, Haskell!"
