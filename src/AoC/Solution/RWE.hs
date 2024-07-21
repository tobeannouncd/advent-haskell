module AoC.Solution.RWE
  ( RWE(..)
  , readRWE, writeRWE, errorRWE
  ) where
import Control.Monad (ap)
import Data.String (IsString (fromString))


newtype RWE r w e a = RWE { runRWE :: r -> Either (e, w) (a, w) }
  deriving (Functor)

instance (Monoid w) => Applicative (RWE r w e) where
  pure x = RWE \_ -> Right (x, mempty)
  (<*>) = ap

instance (Monoid w) => Monad (RWE r w e) where
  RWE m >>= f = RWE \r -> do
    (a, w) <- m r
    case runRWE (f a) r of
      Left  (e, w') -> Left  (e, w <> w')
      Right (b, w') -> Right (b, w <> w')

instance (IsString e, Monoid w) => MonadFail (RWE r w e) where
  fail = errorRWE . fromString

readRWE :: (Monoid w) => RWE r w e r
readRWE = RWE \r -> Right (r, mempty)

writeRWE :: w -> RWE r w e ()
writeRWE w = RWE \_ -> Right ((), w)

errorRWE :: (Monoid w) => e -> RWE r w e a
errorRWE e = RWE \_ -> Left (e, mempty)