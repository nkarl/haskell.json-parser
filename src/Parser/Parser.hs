{-# LANGUAGE BlockArguments #-}

module Parser.Parser where

import Text.Pretty.Simple (pPrint)
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
  -- { runParser :: [Char] -> Parsed a
  }

instance Functor Parser where
  -- fmap f (Parser x) = Parser \s -> g `fmap` x s
  -- fmap f (Parser x) = Parser $ fmap (fmap f) . x
  -- fmap f (Parser x) = Parser $ h . x
  -- where
  -- h = fmap g
  -- g = fmap f
  fmap f mx = Parser $ h . unwrap' mx
    where
      h = fmap g
      g = fmap f
      unwrap' (Parser x) = x -- TODO: refactor with Lens to `get` from the nested Maybe Tuple

instance Applicative Parser where
  (<*>) (Parser f) (Parser x) =
    Parser \s -> do
      (s', g) <- f s
      let h = x s'
      fmap g <$> h
  pure x = Parser \s -> Just (s, x)

type Parsed a = String -> Maybe (String, a)

unwrap :: Parser a -> Parsed a
unwrap (Parser x) = x

(#) :: b -> (b -> c) -> c
(#) = flip ($)

instance Monad Parser where
  (>>=) (Parser x) f = Parser \s -> do
    (s', x') <- x s
    f x' # \(Parser g) -> g s'

-- | parses a single character from a string.
charP :: Parser Char
charP = Parser f
  where
    f (y : ys) = Just (ys, y)
    f [] = Nothing

pull :: [a] -> [a] -> [a]
pull = foldr (:)

stringP :: Parser [Char]
stringP = Parser \s ->
  case s of
    "" -> Nothing
    _ -> Just (s, pull s [])

-- NOTE: no proper error reporting
jsonValue :: Parser JsonValue
jsonValue = undefined

jsonNull :: Parser JsonValue
jsonNull = undefined

test :: IO ()
test = do
  pPrint $ unwrap charP "aaa"
  pPrint $ unwrap stringP "abcde"
