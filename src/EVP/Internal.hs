module EVP.Internal where

type Name = String

data Error = Missing Name
  | ParseError Name String
  deriving Show

data ScanF a = ScanF
  { name :: Name
  , parser :: Maybe String -> Either Error (String, a)
  , metavar :: Maybe String
  } deriving Functor
  
data Scan a where
  Pure :: a -> Scan a
  Scan :: ScanF a -> Scan (a -> b) -> Scan b
  Group :: String -> Scan a -> Scan a

instance Functor Scan where
  fmap f (Pure a) = Pure (f a)
  fmap f (Scan k c) = Scan k (fmap (f.) c)
  fmap f (Group name s) = Group name $ fmap f s

instance Applicative Scan where
  pure = Pure
  Pure f <*> k = f <$> k
  Scan k c <*> r = Scan k (flip <$> c <*> r)
  Group name s <*> r = Group name (s <*> r)
