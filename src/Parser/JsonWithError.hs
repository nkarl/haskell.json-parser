{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use traverse" #-}

module Parser.JsonWithError where

import Data.Kind (Type)
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

type State a = (String, a)

type Process e a = (Failing e) => String -> Either e (State a)

newtype Parser e a = Parser (Process e a)

data Error = EOF deriving (Show)

class Failing {- with -} (e :: Type) where
  eof :: e

instance Failing {- with -} Error where
  eof = EOF

-- newtype Parser a = Parser
-- { runParser :: String -> Maybe (State a)
-- }

{--
  FUNCTOR
--}
instance Functor (Parser e) where
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
instance Applicative (Parser e) where
  (Parser pf) <*> (Parser px) =
    Parser $ \s -> do
      (s', f) <- pf s
      (s'', x) <- px s'
      Right (s'', f x)
  pure x =
    Parser $ \s -> Right (s, x)

{--
   MONAD
--}
instance Monad (Parser e) where
  (>>=) (Parser px) f =
    Parser $ \s -> do
      (s', x) <- px s
      Parser g <- Right (f x)
      g s'

unwrap' :: forall e a. (Failing e) => (Parser e) a -> (String -> Either e (String, a))
unwrap' (Parser x) = x

unwrap :: Parser Error a -> String -> Either Error (String, a)
unwrap = unwrap'

-- | parses a single character from a string.
parseChar :: Char -> Parser e Char
parseChar x = Parser f
 where
  f (y : ys) -- = Just (ys, y)
    | x == y = Right (ys, y)
    | otherwise = Left eof
  f [] = Left eof

pull :: [a] -> [a] -> [a]
pull = foldr (:)

parseString :: Parser e String
parseString =
  Parser $ \s -> do
    Right (s, pull s [])

parseString1 :: String -> Parser e String
parseString1 = sequenceA . map parseChar

parseString2 :: String -> Parser e String
parseString2 = traverse parseChar

-- NOTE: no proper error reporting
jsonValue :: Parser e JsonValue
jsonValue = undefined

jsonNull :: Parser e JsonValue
jsonNull = undefined

test :: IO ()
test = do
  pPrint $
    Result
      { title = "parse a char 'a' from a string `aaa`, returning Right (\"aa\", 'a')"
      , value = show $ (unwrap $ parseChar 'a') "aaa"
      }
  pPrint $
    Result
      { title = "parse a char 'b' from a string `aaa`, returning Left EOF"
      , value = show $ (unwrap $ parseChar 'b') "aaa"
      }
  pPrint $
    Result
      { title = "parse a char from an empty string ``, returning Left EOF"
      , value = show $ (unwrap $ parseChar 'n') ""
      }
  pPrint $
    Result
      { title = "parse a string continuously, returning the a new string with same content"
      , value = show $ unwrap parseString "abcde"
      }
  pPrint $
    Result
      { title = "parse a substring `abc` from `abcdefgh` using sequenceA"
      , value = show $ (unwrap . parseString1) "abc" "abcdefgh"
      }
  pPrint $
    Result
      { title = "parse a substring `abc` from `abcdefgh` using traverse"
      , value = show $ (unwrap . parseString2) "abc" "abcdefgh"
      }
