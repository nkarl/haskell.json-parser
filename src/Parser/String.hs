module Parser.String where

import Data.Kind (Type)
import Data.List (uncons)
import Text.Pretty.Simple (pPrint)
import Prelude

{--
   NOTE: high level view
    - An `Parser` is a abstract symbol of a `Process`.
      - A `Process` takes some input and produces a `State`.
--}

{- | STATE
is a symbolic wrapper for a tuple of `String` and a polymorphic type `a`
-}
type State a = (String, a)

{- | PROCESS
is a function with an _input_ `String` and produces a _context_ `Either`
-}
type Process e a = String -> Either e (State a)

{- | ACTION
is a symbolic wrapper for Process
-}
newtype Parser e a = Parser (Process e a)

-- | ERROR TYPE
data Error = EOF deriving (Show)

{- | TYPECLASS FOR ERROR TYPE
typeclass with some polymorphic type `e`, which is a first-order Kind, aka Type.
-}
class Failing {- with -} (e :: Type) where
  eof :: e

instance Failing {- with -} Error where
  eof = EOF

{--
   FUNCTOR
--}
instance Functor (Parser e) where
  -- fmap :: (a -> b) -> m a -> m b
  fmap f (Parser mx) = Parser $ h . mx
   where
    h = fmap g
    g = fmap f

{--
   APPLICATIVE
--}
instance Applicative (Parser e) where
  -- apply :: m (a -> b) -> m a -> m b
  (<*>) (Parser mf) (Parser mx) =
    Parser $ \s -> do
      (s', f) <- mf s
      (s'', x) <- mx s'
      Right (s'', f x)

  -- pure :: a -> m a
  pure x =
    Parser $ \s -> do
      Right (s, x)

{--
   MONAD
--}
instance Monad (Parser e) where
  -- bind :: m a -> (a -> m b) -> m b
  (>>=) (Parser mx) f =
    Parser $ \s -> do
      (s', x) <- mx s
      Parser g <- Right $ f x
      g s'

{--
   FUNCTION IMPLEMENTATIONS FROM HERE
--}
take1char :: forall e. (Failing e) => Parser e Char
take1char =
  Parser $ \s -> do
    case uncons s of -- `uncons` produces a Maybe, thus a pattern match is required
      Nothing -> Left eof
      Just (x, xs) -> Right (xs, x)

unwrap :: forall e a. (Parser e) a -> Process e a
unwrap (Parser f) = f

{--
   PARSER TEST FUNCTION
--}
test :: IO ()
test = do
  let x = "ABC"
      y = unwrap take1char x :: Either Error (String, Char)
  pPrint y
