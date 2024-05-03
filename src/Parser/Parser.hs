{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use traverse" #-}

module Parser.Parser where

import Miscs.Miscs (Result (..))
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
  }

instance Functor Parser where
  fmap f (Parser px) =
    -- Parser \s -> do
    -- (s', x) <- px s
    -- Just (s', f x)
    Parser $ h . px -- NOTE: this `compose` style works because fmap is done only once.
    where
      h = fmap g
      g = fmap f

instance Applicative Parser where
  (Parser pf) <*> (Parser px) =
    Parser \s ->
      -- case pf s of
      --  Nothing -> Nothing
      --  Just (s', f) -> case px s' of
      --    Nothing -> Nothing
      --    Just (s'', x) -> Just (s'', f x)
      do
        (s', f) <- pf s
        (s'', x) <- px s'
        Just (s'', f x)
  pure x = Parser \s -> Just (s, x)

unwrap :: Parser a -> (String -> Maybe (String, a))
unwrap (Parser x) = x

(#) :: b -> (b -> c) -> c
(#) = flip ($)

instance Monad Parser where
  (>>=) (Parser px) f =
    Parser \s ->
      do
        (s', x) <- px s
        -- f x # \(Parser g) -> g s'
        Parser g <- Just (f x)
        g s'

-- | parses a single character from a string.
charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y : ys) -- = Just (ys, y)
      | x == y = Just (ys, y)
      | otherwise = Nothing
    f [] = Nothing

pull :: [a] -> [a] -> [a]
pull = foldr (:)

stringP :: Parser String
stringP = Parser \s ->
  case s of
    "" -> Nothing
    _ -> Just (s, pull s [])

stringP' :: String -> Parser String
stringP' = sequenceA . map charP

stringP'' :: String -> Parser String
stringP'' = traverse charP

-- NOTE: no proper error reporting
jsonValue :: Parser JsonValue
jsonValue = undefined

jsonNull :: Parser JsonValue
jsonNull = undefined

test :: IO ()
test = do
  pPrint $
    Result
      { title = "parse a char from a string `aaa`,leaving (\"aa\", 'a')",
        value = show $ (unwrap $ charP 'n') "aaa"
      }
  pPrint $
    Result
      { title = "parse a string continuously",
        value = show $ unwrap stringP "abcde"
      }
  pPrint $
    Result
      { title = "parse a string continuously",
        value = show $ (unwrap . stringP') "abc" "abcdefgh"
      }
  pPrint $
    Result
      { title = "parse a string continuously",
        value = show $ (unwrap . stringP'') "abc" "abcdefgh"
      }
