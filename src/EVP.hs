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
  -- * Logger
  , assumePrefix
  , obsolete
  -- * Internal
  , Scan(..)
  ) where

import Control.Monad
import Data.Bifunctor
import Data.Default.Class
import Data.List (isPrefixOf)
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
string :: (IsString a) => Name -> Scan a
string v = Var v (\case
  Nothing -> Left (Missing v)
  Just x -> Right (x, fromString x)) (Pure id)

stringDefault :: (IsString a) => Name -> String -> Scan a
stringDefault v d = Var v (\case
  Nothing -> Right (d <> " (default)", fromString d)
  Just x -> Right (x, fromString x)) (Pure id)

-- | Parse the environment variable as a YAML value.
yaml :: (Show a, Yaml.FromJSON a) => Name -> Scan a
yaml v = parse v decodeYaml

-- | Parse the environment variable as a YAML value.
yamlDefault :: (Show a, Yaml.FromJSON a) => Name -> a -> Scan a
yamlDefault v d = parseDefault v d decodeYaml

decodeYaml :: Yaml.FromJSON a => String -> Either String a
decodeYaml = first show . Yaml.decodeEither' . encodeUtf8 . fromString

parse :: (Show a) => Name -> (String -> Either String a) -> Scan a
parse v f = Var v (\case
  Nothing -> Left (Missing v)
  Just x -> bimap (ParseError v) withShow $ f x) (Pure id)

parseDefault :: (Show a) => Name -> a -> (String -> Either String a) -> Scan a
parseDefault v d f = Var v (\case
  Nothing -> Right (show d <> " (default)", d)
  Just x -> bimap (ParseError v) withShow $ f x) (Pure id)

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
  , unusedLogger :: Name -> Maybe (IO ())
  , pedantic :: Bool -- ^ exit on warning
  }
  
instance Default Settings where
  def = Settings
    { parseLogger = \name value -> putStrLn $ unwords ["[EVP Info]", name <> ":", value]
    , errorLogger = \e -> hPutStrLn stderr $ unwords ["[EVP Error]", show e]
    , unusedLogger = mempty
    , pedantic = False
    }

-- | Custom logging function for 'unusedLogger'.
-- @'assumePrefix' p@ prints a warning for each unused environment variable prefixed by @p@.
assumePrefix :: String -> Name -> Maybe (IO ())
assumePrefix prefix name
  | isPrefixOf prefix name = Just $ hPutStrLn stderr $ unwords ["[EVP Warn]", name, "is set but not used"]
  | otherwise = Nothing

-- | @'obsolete' names@ prints a warning if any of the @names@ is set but not used.
obsolete :: [Name] -> Name -> Maybe (IO ())
obsolete nameSet name
  | elem name nameSet = Just $ hPutStrLn stderr $ unwords ["[EVP Warn]", name, "is obsolete"]
  | otherwise = Nothing

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
  mapM_ errorLogger errors
  case foldMap unusedLogger $ Map.keys remainder of
    Nothing -> pure ()
    Just m -> do
      m
      when pedantic exitFailure
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
