{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}
module EVP
  ( Name
  , Error(..)
  -- * Parsers
  , Scan
  , Var(..)
  , string
  , yaml
  , parse
  , secret
  , group
  -- * Runner
  , Settings(..)
  , def
  , scan
  , scanWith
  , enumerate
  -- * Logger
  , assumePrefix
  , obsolete
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
import EVP.Internal
import System.Environment
import System.Exit
import System.IO

data Var a = Var
  { name :: Name
  , defaultValue :: Maybe a
  } deriving (Show, Eq)

instance IsString (Var a) where
  fromString name = Var name Nothing

-- | Obtain the environment variable.
string :: (IsString a, Show a) => Var a -> Scan a
string Var{..} = Scan ScanF
  { name = name
  , parser = \case
    Nothing -> case defaultValue of
      Nothing -> Left (Missing name)
      Just d -> Right (show d <> " (default)", d)
    Just x -> Right (x, fromString x)
  } (Pure id)

-- | Parse the environment variable as a YAML value.
yaml :: forall a. (Show a, Yaml.FromJSON a) => Var a -> Scan a
yaml v = parse v decodeYaml

decodeYaml :: Yaml.FromJSON a => String -> Either String a
decodeYaml = first show . Yaml.decodeEither' . encodeUtf8 . fromString

parse :: (Show a) => Var a -> (String -> Either String a) -> Scan a
parse Var{..} f = Scan ScanF
  { name
  , parser = \case
    Nothing -> case defaultValue of
      Nothing -> Left (Missing name)
      Just d -> Right (show d <> " (default)", d)
    Just x -> bimap (ParseError name) withShow $ f x
  }
  (Pure id)

-- | Disable logging of parsed values.
secret :: Scan a -> Scan a
secret (Pure a) = Pure a
secret (Scan v k) = Scan (v { parser = fmap (first (const "<REDACTED>")) . parser v }) (secret k)
secret (Group name s) = Group name $ secret s

-- | Give a name to a group of parsers.
group :: String -> Scan a -> Scan a
group = Group

withShow :: Show a => a -> (String, a)
withShow x = (show x, x)

type EnvMap = Map.Map String String

data Settings = Settings
  { groupLogger :: [String] -> IO ()
  , parseLogger :: Name -> String -> IO ()
  , errorLogger :: Error -> IO ()
  , unusedLogger :: Name -> Maybe (IO ())
  , pedantic :: Bool -- ^ exit on warning
  }
  
instance Default Settings where
  def = Settings
    { groupLogger = \names -> putStrLn $ unwords $ "[EVP Info] Group:" : names
    , parseLogger = \name value -> putStrLn $ unwords ["[EVP Info]", name <> ":", value]
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
  go !s (Scan k cont) = go (Set.insert (name k) s) cont
  go !s (Group _ cont) = go s cont

scan :: Scan a -> IO a
scan = scanWith def

scanWith :: Settings -> Scan a -> IO a
scanWith Settings{..} action = do
  envs0 <- Map.fromList <$> getEnvironment
  (remainder, errors, result) <- go envs0 envs0 [] action
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
    go :: EnvMap -> EnvMap -> [String] -> Scan a -> IO (EnvMap, [Error], Maybe a)
    go _ envs _ (Pure a) = pure (envs, [], Just a)
    go allEnvs envs groupStack (Group name inner) = do
      let stack = name : groupStack
      groupLogger $ reverse stack
      go allEnvs envs stack inner
    go allEnvs envs groupStack (Scan ScanF{..} cont) = case parser (Map.lookup name allEnvs) of
      Left e -> do
        (remainder, errors, _) <- go allEnvs (Map.delete name envs) groupStack cont
        pure (remainder, e : errors, Nothing)
      Right (display, v) -> do
        parseLogger name display
        (remainder, errors, func) <- go allEnvs (Map.delete name envs) groupStack cont
        pure (remainder, errors, ($ v) <$> func)
