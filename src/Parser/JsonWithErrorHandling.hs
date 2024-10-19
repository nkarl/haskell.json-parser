{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use traverse" #-}

module Parser.JsonWithErrorHandling where

import Data.Kind (Type)
import Miscs.Miscs (Result (..))
import Text.Pretty.Simple (pPrint)
import Prelude

import Control.Applicative (Alternative (empty), (<|>))

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Integer -- NOTE: no support for floats
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [Element JsonValue]
  deriving (Show, Eq)

type Element a = (String, a)
type Process e a = String -> Either e (Element a)

newtype Parser e a = Parser (Process e a)

data ErrorMsg = EOF | Other deriving (Show)

class Failing {- with reason -} (r :: Type) where
  eof :: r 

instance Failing {- with -} ErrorMsg where
  eof = EOF

{--
  FUNCTOR

  The composing style works for `fmap` because `f` is first-ordered.
  We only need to peel off the layers of `Parser a` and then apply `f` to the inner most type.
  `Parser a` is in fact a layered monad `m1 (m2 a)` where
    `m1 :: Maybe`
    `m2 :: Tuple`, more accurately the partially applied `Tuple String`
--}
instance Functor (Parser e) where
  fmap f (Parser mx) =
    Parser $ ((f <$>) <$>) . mx

{--
  APPLICATIVE
--}
instance Applicative (Parser e) where
  (<*>) (Parser mf) (Parser mx) =
    Parser $ \s -> do
      (s', f) <- mf s
      (s'', x) <- mx s'
      Right (s'', f x)
  pure x =
    Parser $ \s -> do
      Right (s, x)

-- instance (Failing e) => Alternative (Parser e) where
-- empty = Parser $ \_ -> Left eof
-- (<|>) (Parser p1) (Parser p2) =
-- Parser $ \s -> p1 s <|> p2 s

{--
   MONAD
--}
instance Monad (Parser e) where
  (>>=) (Parser mx) f =
    Parser $ \s -> do
      (s', x) <- mx s
      Parser g <- Right (f x)
      g s'

unwrap' :: forall e a. Parser e a -> Process e a
unwrap' (Parser x) = x

unwrap :: Parser ErrorMsg a -> Process ErrorMsg a
unwrap = unwrap'

-- | parses a single character from a string. Can fail.
parseChar :: (Failing e) => Char -> Parser e Char
parseChar x =
  Parser $ \s -> do
    f s
 where
  f [] = Left eof
  f (y : ys)
    | x == y = Right (ys, y)
    | otherwise = Left eof

_pull :: [a] -> [a] -> [a]
_pull = foldr (:)

{- | parses a substring from a string.
 Can not fail, because `_pull` cannot fail on all strings, including the empty string [].
-}
parseString :: Parser e String
parseString =
  Parser $ \s -> do
    Right (s, _pull s [])

parseString1 :: (Failing e) => String -> Parser e String
parseString1 = sequenceA . (<$>) parseChar

parseString2 :: (Failing e) => String -> Parser e String
parseString2 = traverse parseChar

-- jsonBool :: Parser e JsonValue
-- jsonBool = f <$> (parseString1 "true" <|> parseString1 "false")
-- where
-- f "true" = JsonBool True
-- f "false" = JsonBool False
-- f _       = undefined

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
      , value = show $ (unwrap . parseChar) 'a' "aaa"
      }
  pPrint $
    Result
      { title = "parse a char 'b' from a string `aaa`, returning Left EOF"
      , value = show $ (unwrap . parseChar) 'b' "aaa"
      }
  pPrint $
    Result
      { title = "parse a char from an empty string ``, returning Left EOF"
      , value = show $ (unwrap . parseChar) 'n' ""
      }
  pPrint $
    Result
      { title = "parse a string continuously, returning the a new string with same content"
      , value = show $ unwrap parseString "abcde"
      }
  pPrint $
    Result
      { title = "parse a string continuously, returning the a new string with same content"
      , value = show $ unwrap parseString []
      }
  pPrint $
    Result
      { title = "parse a substring `abc` from `abcdefgh` using sequenceA"
      , value = show $ (unwrap . parseString1) "abc" "abcdefgh"
      }
  pPrint $
    Result
      { title = "parse a substring `xyz` from `abcdefgh` using sequenceA"
      , value = show $ (unwrap . parseString1) "cba" "abcdefgh"
      }
  pPrint $
    Result
      { title = "parse a substring `abc` from `` using sequenceA"
      , value = show $ (unwrap . parseString1) "abc" ""
      }
  pPrint $
    Result
      { title = "parse a substring `abc` from `abcdefgh` using traverse"
      , value = show $ (unwrap . parseString2) "abc" "abcdefgh"
      }
  pPrint $
    Result
      { title = "parse a substring `xyz` from `abcdefgh` using traverse"
      , value = show $ (unwrap . parseString2) "cba" "abcdefgh"
      }
  pPrint $
    Result
      { title = "parse a substring `abc` from `` using traverse"
      , value = show $ (unwrap . parseString2) "abc" ""
      }
