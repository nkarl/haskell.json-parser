{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use traverse" #-}

module Parser.Json where

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

{--
  FUNCTOR
--}
instance Functor Parser where
  fmap f (Parser px) =
    Parser $ h . px
   where
    h = fmap g
    g = fmap f

{--
   The composing style works for `fmap` because `f` is first-ordered.
   We only need to peel off the layers of `Parser a` and then apply `f` to the inner most type.
   `Parser a` is in fact a layered monad `m1 (m2 a)` where
      `m1 :: Maybe`
      `m2 :: Tuple`, more accurately the partially applied `Tuple String`
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

-- NOTE: no proper error reporting
jsonValue :: Parser JsonValue
jsonValue = undefined

jsonNull :: Parser JsonValue
jsonNull = undefined

test :: IO ()
test = do
  pPrint $
    Result
      { title = "parse a char from a string `aaa`,leaving (\"aa\", 'a')"
      , value = show $ (unwrap $ parseChar 'n') "aaa"
      }
  pPrint $
    Result
      { title = "parse a string continuously"
      , value = show $ unwrap parseString "abcde"
      }
  pPrint $
    Result
      { title = "parse a string continuously"
      , value = show $ (unwrap . parseString1) "abc" "abcdefgh"
      }
  pPrint $
    Result
      { title = "parse a string continuously"
      , value = show $ (unwrap . parseString2) "abc" "abcdefgh"
      }
