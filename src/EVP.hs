{-# LANGUAGE RecordWildCards #-}
module EVP
  ( Name
  , Error(..)
  -- * Parsers
  , string
  , yaml
  , parse
  , secret
  -- * Runner
  , Settings(..)
  , def
  , scan
  , scanWith
  -- * Internal
  , Scan(..)
  ) where

import Data.Bifunctor
import Data.Default.Class
import Data.Map qualified as Map
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
string v = Var (show v) (\x -> Right (x, fromString x)) (Pure id)

-- | Parse the environment variable as a YAML value.
yaml :: (Show a, Show (v a), Yaml.FromJSON a) => v a -> Scan a
yaml v = parse v $ first show . Yaml.decodeEither' . encodeUtf8 . fromString

parse :: (Show a, Show (v a)) => v a -> (String -> Either String a) -> Scan a  
parse v f = Var (show v) (fmap withShow . f) (Pure id)

-- | Disable logging of parsed values.
secret :: Scan a -> Scan a
secret (Pure a) = Pure a
secret (Var v f k) = Var v (fmap (first (const "<REDACTED>")) . f) (secret k)

withShow :: Show a => a -> (String, a)
withShow x = (show x, x)
  
data Scan a where
  Pure :: a -> Scan a
  Var :: Name -> (String -> Either String (String, a)) -> Scan (a -> b) -> Scan b

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

scan :: Scan a -> IO a
scan = scanWith def
  
scanWith :: Settings -> Scan a -> IO a
scanWith Settings{..} action = do
  envs0 <- Map.fromList <$> getEnvironment
  (remainder, errors, result) <- go envs0 action
  mapM_ unusedLogger $ Map.keys remainder
  mapM_ errorLogger errors
  case result of
    Nothing -> exitFailure
    Just a -> pure a
  where
    go :: EnvMap -> Scan a -> IO (EnvMap, [Error], Maybe a)
    go envs (Pure a) = pure (envs, [], Just a)
    go envs (Var name parser cont) = do
      let (mstr, rest) = Map.alterF (\v -> (v, Nothing)) name envs
      case mstr of
        Nothing -> do
          (remainder, errors, _) <- go rest cont
          pure (remainder, Missing name : errors, Nothing)
        Just str -> case parser str of
          Left e -> do
            (remainder, errors, _) <- go rest cont
            pure (remainder, ParseError name e : errors, Nothing)
          Right (display, v) -> do
            parseLogger name display
            (remainder, errors, func) <- go rest cont
            pure (remainder, errors, ($ v) <$> func)
