module AoC.Solution (
  Solution,
  runSolution,
  runSolution',
  testSolution,
  S (..),
  input,
  parseInput,
  answer,
) where

import AoC.Solution.RWE (RWE (runRWE), readRWE, writeRWE)
import Control.Monad.Reader (
  MonadTrans (lift),
  ReaderT (runReaderT),
  asks,
 )
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B8
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Data.Typeable (Typeable, cast)
import Text.Parsec (Parsec, Stream, parse)

data Prod m = Prod
  { pStr :: m String
  , pTxt :: m Text
  , pByt :: m ByteString
  }

class Producer m where
  prod :: Prod m

instance Producer IO where
  prod =
    Prod
      { pStr = getContents
      , pTxt = T.getContents
      , pByt = B8.getContents
      }

mkProd :: (Input a) => (forall b. (a -> b) -> m b) -> Prod m
mkProd f =
  Prod
    { pStr = f coerce
    , pTxt = f coerce
    , pByt = f coerce
    }

instance (Input r, Monad m) => Producer (ReaderT r m) where
  prod = mkProd asks

instance (Input r, Monoid w) => Producer (RWE r w e) where
  prod = mkProd (<$> readRWE)

class Input a where
  fromProd :: Prod m -> m a
  toRep :: a -> Rep
  fromRep :: Rep -> a

input :: (Producer m, Input a) => m a
input = fromProd prod

coerce :: (Input a, Input b) => a -> b
coerce = fromRep . toRep

parseInput ::
  (Stream s Identity Char, Producer m, Input s, MonadFail m) =>
  Parsec s () a ->
  m a
parseInput p = either (fail . show) return . parse p "" =<< input

data Rep
  = RStr String
  | RTxt Text
  | RByt ByteString

instance Input String where
  fromProd = pStr
  toRep = RStr
  fromRep = foldRep id T.unpack B8.unpack

instance Input Text where
  fromProd = pTxt
  toRep = RTxt
  fromRep = foldRep T.pack id T.decodeUtf8

instance Input ByteString where
  fromProd = pByt
  toRep = RByt
  fromRep = foldRep B8.pack T.encodeUtf8 id

foldAnswer ::
  (Typeable a) =>
  (String -> b) ->
  (Text -> b) ->
  (ByteString -> b) ->
  (a -> b) ->
  a ->
  b
foldAnswer sb tb bb ab a
  | Just s <- cast a = sb s
  | Just t <- cast a = tb t
  | Just b <- cast a = bb b
  | otherwise = ab a

answerRep :: (Typeable a, Show a) => a -> Rep
answerRep = foldAnswer RStr RTxt RByt (RStr . show)

foldRep :: (String -> t) -> (Text -> t) -> (ByteString -> t) -> Rep -> t
foldRep fs ft fb = \case
  RStr s -> fs s
  RTxt t -> ft t
  RByt b -> fb b

class Consumer m where
  answer :: (Show a, Typeable a) => a -> m ()

instance Consumer IO where
  answer = foldAnswer putStrLn T.putStrLn B8.putStrLn print

instance (Consumer m, Monad m) => Consumer (ReaderT r m) where
  answer = lift . answer

instance (Applicative t) => Consumer (RWE r (t Rep) e) where
  answer = writeRWE . pure . answerRep

type Solution m = (Producer m, Consumer m, MonadFail m)

newtype S = S (forall m. (Solution m) => m ())

runSolution :: (Input a) => S -> a -> IO ()
runSolution (S soln) = runReaderT soln

runSolution' :: S -> IO ()
runSolution' (S soln) = soln

testSolution :: (Input a) => S -> a -> ([Rep], Maybe String)
testSolution (S soln) inp =
  case runRWE soln inp of
    Left (e, xs) -> (xs, Just e)
    Right (_, xs) -> (xs, Nothing)