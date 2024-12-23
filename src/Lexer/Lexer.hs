{-# HLINT ignore "Use traverse" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Lexer.Lexer where

import Control.Monad ((>=>))

import Control.Applicative (Alternative (empty, (<|>)))

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
  | NotJsonKeyword -- escape signal when matching for keywords `true`, `false` and `null` -- NOTE: be careful to avoid cycles
  deriving (Show, Eq)

type Source = String

-- | the lexing function; evaluates upon receiving an input source.
type Lexing e a = Source -> Maybe (e, a)

type ErrorMsg = String

newtype Lexer a = Lexer (Lexing ErrorMsg a)

{--
  FUNCTOR
--}
instance Functor Lexer where
  f `fmap` (Lexer px) = Lexer $ h . px
   where
    h = fmap (fmap f)

{--
   The composing style works for `fmap` because `f` is first-ordered.
   We only need to peel off the layers of `Lexer a` and then apply `f` to the inner most type.
   NOTE: `Lexer a` is triple-layered monad `m1 (m2 (m3 a))` where
      `m1 :: Lexer`
      `m2 :: Maybe`
      `m3 :: Tuple`, more accurately the curried 2-tuple `(String, ...)`

   The outer most context `Lexer` is at the 3rd layer. Thus, we need to specify `fmap` twice to peel
   off the 2 inner layers.
--}

{--
  APPLICATIVE
--}
instance Applicative Lexer where
  Lexer pf <*> Lexer px =
    Lexer $ pf >=> \(s1, f) -> px s1 >>= \(s2, x) -> Just (s2, f x)
  pure x =
    Lexer $ \s -> Just (s, x)

{--
  ALTERNATIVE
--}
instance Alternative Lexer where
  empty = Lexer $ const Nothing
  Lexer p1 <|> Lexer p2 = Lexer $ \s -> p1 s <|> p2 s

{--
   MONAD
--}
instance Monad Lexer where
  Lexer px >>= f =
    Lexer $ px >=> \(s', x) -> Just (f x) >>= \(Lexer g) -> g s'

unwrap :: Lexer a -> Lexing ErrorMsg a
unwrap (Lexer x) = x

-- | lexes and checks a single character from a string.
lexChar :: Char -> Lexer Char
lexChar x = Lexer f
 where
  f [] = Nothing
  f (y : ys)
    | x == y = Just (ys, y)
    | otherwise = Nothing

pull :: [a] -> [a] -> [a]
pull = foldr (:)

-- | creates a Lexer context; contains a lexing function that evaluates upon receiving a source input.
lexString :: Lexer String
lexString =
  Lexer $ \s -> Just (s, pull s [])

type Keyword = String

-- | takes a pattern and match it against an input source (upon received by the lexing function).
lexString1 :: Keyword -> Lexer Keyword
lexString1 = sequenceA . map lexChar

-- | takes a pattern and match it against an input source (upon received by the lexing function).
lexString2 :: Keyword -> Lexer Keyword
lexString2 = traverse lexChar

tokenizeKeyword :: Lexer JsonValue
tokenizeKeyword = matchKeyword <$> (lexString1 "true" <|> lexString1 "false" <|> lexString1 "null")
 where
  matchKeyword "true" = JsonBool True
  matchKeyword "false" = JsonBool False
  matchKeyword "null" = JsonNull
  -- TODO: adds other JSON variants for braces, brackets and such
  matchKeyword _ = NotJsonKeyword

-- NOTE: no proper error reporting
jsonValue :: Lexer JsonValue
jsonValue = undefined

test :: IO ()
test = do
  pPrint $
    Result
      { title = "lex a char 'a' from a string `aaa`, returning Right (\"aa\", 'a')"
      , value = show $ (unwrap . lexChar) 'a' "aaa"
      }
  pPrint $
    Result
      { title = "lex a char 'b' from a string `aaa`, returning Left EOF"
      , value = show $ (unwrap . lexChar) 'b' "aaa"
      }
  pPrint $
    Result
      { title = "lex a char from an empty string ``, returning Left EOF"
      , value = show $ (unwrap . lexChar) 'n' ""
      }
  pPrint $
    Result
      { title = "lex a string continuously, returning the a new string with same content"
      , value = show $ unwrap lexString "abcde"
      }
  pPrint $
    Result
      { title = "lex a string continuously, returning the a new string with same content"
      , value = show $ unwrap lexString []
      }
  pPrint $
    Result
      { title = "lex a substring `abc` from `abcdefgh` using sequenceA"
      , value = show $ (unwrap . lexString1) "abc" "abcdefgh"
      }
  pPrint $
    Result
      { title = "lex a substring `xyz` from `abcdefgh` using sequenceA"
      , value = show $ (unwrap . lexString1) "cba" "abcdefgh"
      }
  pPrint $
    Result
      { title = "lex a substring `abc` from `` using sequenceA"
      , value = show $ (unwrap . lexString1) "abc" ""
      }
  pPrint $
    Result
      { title = "lex a substring `abc` from `abcdefgh` using traverse"
      , value = show $ (unwrap . lexString2) "abc" "abcdefgh"
      }
  pPrint $
    Result
      { title = "lex a substring `xyz` from `abcdefgh` using traverse"
      , value = show $ (unwrap . lexString2) "cba" "abcdefgh"
      }
  pPrint $
    Result
      { title = "lex a substring `abc` from `` using traverse"
      , value = show $ (unwrap . lexString2) "abc" ""
      }
  pPrint $
    Result
      { title = "lex a substring `true` from `true,` using traverse"
      , value = show $ unwrap tokenizeKeyword "true,"
      }
  pPrint $
    Result
      { title = "lex a substring `false` from `false,` using traverse"
      , value = show $ unwrap tokenizeKeyword "false,"
      }
  pPrint $
    Result
      { title = "lex a substring `null` from `null,` using traverse"
      , value = show $ unwrap tokenizeKeyword "null,"
      }
