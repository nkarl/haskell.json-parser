module Parser.String where

import Data.Kind (Type)
import Data.List (uncons)
import Text.Pretty.Simple (pPrint)
import Prelude

{--
   NOTE: high level view
    - An `Action` is a abstract symbol of a `Process`.
      - A `Process` takes some input and produces a `State`.
--}

-- | STATE
-- is a symbolic wrapper for a tuple of `String` and a polymorphic type `a`
type State a = (String, a)

-- | PROCESS
-- is a function with an _input_ `String` and produces a _context_ `Either`
type Process e a = (Failing e) => (String -> Either e (State a))

-- | ACTION
-- is a symbolic wrapper for Process
newtype Action e a = Action (Process e a)

-- | ERROR TYPE
data Error = EOF deriving (Show)

-- | TYPECLASS FOR ERROR TYPE
-- typeclass with some polymorphic type `e`, which is a first-order Kind, aka Type.
class Failing {- with -} (e :: Type) where
  eof :: e

instance Failing {- with -} Error where
  eof = EOF

{--
   FUNCTOR
--}
instance Functor (Action e) where
  -- fmap :: (a -> b) -> m a -> m b
  fmap f (Action mx) = Action $ h . mx
    where
      h = fmap g
      g = fmap f

{--
   APPLICATIVE
--}
instance Applicative (Action e) where
  -- apply :: m (a -> b) -> m a -> m b
  (<*>) (Action mf) (Action mx) =
    Action $ \s -> do
      (s', f) <- mf s
      (s'', x) <- mx s'
      Right (s'', f x)

  -- pure :: a -> m a
  pure x =
    Action $ \s -> do
      Right (s, x)

{--
   MONAD
--}
instance Monad (Action e) where
  -- bind :: m a -> (a -> m b) -> m b
  (>>=) (Action mx) f =
    Action $ \s -> do
      (s', x) <- mx s
      Action g <- Right $ f x
      g s'

{--
   FUNCTION IMPLEMENTATIONS FROM HERE
--}
take1char :: forall e. Action e Char
take1char =
  Action $ \s -> do
    case uncons s of -- `uncons` produces a Maybe, thus a pattern match is required
      Nothing -> Left eof
      Just (x, xs) -> Right (xs, x)

unwrap :: forall e a. (Action e) a -> Process e a
unwrap (Action f) = f

{--
   PARSER TEST FUNCTION
--}
test :: IO ()
test = do
  let x = "ABC"
      y = unwrap take1char x :: Either Error (String, Char)
  pPrint y
