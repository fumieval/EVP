{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module EVP
  ( Name
  , Error(..)
  -- * Parsers
  , string
  , yaml
  , parse
  , secret
  -- * Providing a default value
  , stringDefault
  , yamlDefault
  , parseDefault
  -- * Runner
  , Settings(..)
  , def
  , scan
  , scanWith
  , enumerate
  -- * Internal
  , Scan(..)
  ) where

import Data.Bifunctor
import Data.Default.Class
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.String
import Data.Yaml qualified as Yaml
import Data.Text.Encoding
import System.Environment
import System.Exit
import System.IO

type Name = String

data Error = Missing Name
  | ParseError Name String
  deriving Show

-- | Obtain the environment variable.
string :: (Show (v a), IsString a) => v a -> Scan a
string v = Var (show v) (\case
  Nothing -> Left (Missing (show v))
  Just x -> Right (x, fromString x)) (Pure id)

stringDefault :: (Show (v a), IsString a) => v a -> String -> Scan a
stringDefault v d = Var (show v) (\case
  Nothing -> Right (d <> " (default)", fromString d)
  Just x -> Right (x, fromString x)) (Pure id)

-- | Parse the environment variable as a YAML value.
yaml :: (Show a, Show (v a), Yaml.FromJSON a) => v a -> Scan a
yaml v = parse v decodeYaml

-- | Parse the environment variable as a YAML value.
yamlDefault :: (Show a, Show (v a), Yaml.FromJSON a) => v a -> a -> Scan a
yamlDefault v d = parseDefault v d decodeYaml

decodeYaml :: Yaml.FromJSON a => String -> Either String a
decodeYaml = first show . Yaml.decodeEither' . encodeUtf8 . fromString

parse :: (Show a, Show (v a)) => v a -> (String -> Either String a) -> Scan a  
parse v f = Var (show v) (\case
  Nothing -> Left (Missing (show v))
  Just x -> bimap (ParseError (show v)) withShow $ f x) (Pure id)

parseDefault :: (Show a, Show (v a)) => v a -> a -> (String -> Either String a) -> Scan a
parseDefault v d f = Var (show v) (\case
  Nothing -> Right (show d <> " (default)", d)
  Just x -> bimap (ParseError (show v)) withShow $ f x) (Pure id)

-- | Disable logging of parsed values.
secret :: Scan a -> Scan a
secret (Pure a) = Pure a
secret (Var v f k) = Var v (fmap (first (const "<REDACTED>")) . f) (secret k)

withShow :: Show a => a -> (String, a)
withShow x = (show x, x)
  
data Scan a where
  Pure :: a -> Scan a
  Var :: Name -> (Maybe String -> Either Error (String, a)) -> Scan (a -> b) -> Scan b

instance Functor Scan where
  fmap f (Pure a) = Pure (f a)
  fmap f (Var k g c) = Var k g (fmap (f.) c)

instance Applicative Scan where
  pure = Pure
  Pure f <*> k = f <$> k
  Var k f c <*> r = Var k f (flip <$> c <*> r)
   
type EnvMap = Map.Map String String

data Settings = Settings
  { parseLogger :: Name -> String -> IO ()
  , errorLogger :: Error -> IO ()
  , unusedLogger :: Name -> IO ()
  }
  
instance Default Settings where
  def = Settings
    { parseLogger = \name value -> putStrLn $ unwords ["[EVP Info]", name <> ":", value]
    , errorLogger = \e -> hPutStrLn stderr $ unwords ["[EVP Error]", show e]
    , unusedLogger = mempty
    }

-- | Enumerate the names of the variables it would parse.
enumerate :: Scan a -> [Name]
enumerate m = Set.toList $ go Set.empty m where
  go :: Set.Set Name -> Scan a -> Set.Set Name
  go !s (Pure _) = s
  go !s (Var k _ cont) = go (Set.insert k s) cont

scan :: Scan a -> IO a
scan = scanWith def
  
scanWith :: Settings -> Scan a -> IO a
scanWith Settings{..} action = do
  envs0 <- Map.fromList <$> getEnvironment
  (remainder, errors, result) <- go envs0 envs0 action
  mapM_ unusedLogger $ Map.keys remainder
  mapM_ errorLogger errors
  case result of
    Nothing -> exitFailure
    Just a -> pure a
  where
    go :: EnvMap -> EnvMap -> Scan a -> IO (EnvMap, [Error], Maybe a)
    go _ envs (Pure a) = pure (envs, [], Just a)
    go allEnvs envs (Var name parser cont) = case parser (Map.lookup name allEnvs) of
      Left e -> do
        (remainder, errors, _) <- go allEnvs (Map.delete name envs) cont
        pure (remainder, e : errors, Nothing)
      Right (display, v) -> do
        parseLogger name display
        (remainder, errors, func) <- go allEnvs (Map.delete name envs)  cont
        pure (remainder, errors, ($ v) <$> func)
