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
  , defaultsTo
  -- * Runner
  , Settings(..)
  , def
  , scan
  , scanWith
  , enumerate
  , help
  -- * Logger
  , GroupStack
  , renderError
  , assumePrefix
  , obsolete
  -- * Advanced
  , modifyError
  ) where

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Default.Class
import Data.List (isPrefixOf, intercalate)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.String
import Data.Yaml qualified as Yaml
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Encoding
import Data.Typeable
import EVP.Internal
import System.Environment
import System.Exit
import System.IO

data Var a = Var
  { name :: Name
  , defaultValue :: Maybe a
  , metavar :: Maybe String
  } deriving (Show, Eq)

-- | Provide a default value.
defaultsTo :: Var a -> a -> Var a
defaultsTo v a = v { defaultValue = Just a }
infixl 1 `defaultsTo`

instance Typeable a => IsString (Var a) where
  fromString name = Var name Nothing Nothing

-- | Obtain the environment variable.
string :: (IsString a, Show a, Typeable a) => Var a -> Scan a
string Var{..} = Scan ScanF
  { name
  , parser = \case
    Nothing -> case defaultValue of
      Nothing -> Left (Missing name)
      Just d -> Right (show d <> " (default)", d)
    Just x -> Right (x, fromString x)
  , metavar = metavar <|> toString <$> defaultValue
  } (Pure id)

toString :: (Typeable a, Show a) => a -> String
toString val
  | Just str <- cast val = str
  | Just str <- cast val = T.unpack str
  | Just str <- cast val = TL.unpack str
  | Just str <- cast val = T.unpack $ decodeUtf8 str
  | otherwise = show val

-- | Parse the environment variable as a YAML value.
yaml :: forall a. (Show a, Typeable a, Yaml.FromJSON a, Yaml.ToJSON a) => Var a -> Scan a
yaml Var{..} = parse Var
  { metavar = metavar
    <|> fmap (T.unpack . T.strip . decodeUtf8 . Yaml.encode) defaultValue
    <|> Just (show $ typeRep (Proxy :: Proxy a))
  , ..
  }
  decodeYaml

decodeYaml :: Yaml.FromJSON a => String -> Either String a
decodeYaml = first Yaml.prettyPrintParseException . Yaml.decodeEither' . encodeUtf8 . fromString

-- | Parse the environment variable with a custom parser.
parse :: (Show a) => Var a -> (String -> Either String a) -> Scan a
parse Var{..} f = Scan ScanF
  { name
  , parser = \case
    Nothing -> case defaultValue of
      Nothing -> Left (Missing name)
      Just d -> Right (show d <> " (default)", d)
    Just x -> bimap (ParseError name x) withShow $ f x
  , metavar
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

type GroupStack = [String]

renderError :: Error -> String
renderError (Missing name) = unwords ["Missing environment variable", name]
renderError (ParseError name value reason) = unwords ["Failed to parse", name <> "=" <> value <> ":", reason]
renderError (CustomError reason) = reason

data Settings = Settings
  { parseLogger :: GroupStack -> Name -> String -> IO ()
  , errorLogger :: GroupStack -> Error -> IO ()
  , unusedLogger :: Name -> Maybe (IO ())
  , pedantic :: Bool -- ^ exit on warning
  , helpFlag :: Maybe Name -- ^ when an environment varialbe with this name is set, print the help message and exit
  }

header :: GroupStack -> String -> String
header [] level = "[EVP " <> level <> "] "
header xs level = mconcat ["[EVP ", level, concatMap ("/"<>) $ reverse xs, "] "]

instance Default Settings where
  def = Settings
    { parseLogger = \stack name value -> do
      isTerminal <- hIsTerminalDevice stdout
      hPutStrLn (if isTerminal then stdout else stderr)
        $ header stack "Info" <> name <> ": " <> value
    , errorLogger = \stack e -> hPutStrLn stderr $ intercalate "\n" $ map (header stack "Error" <>) $ lines $ renderError e
    , unusedLogger = mempty
    , pedantic = False
    , helpFlag = Just "EVP_HELP"
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

-- | Parse environment variables with the default settings.
scan :: Scan a -> IO a
scan = scanWith def

-- | Parse environment variables with custom settings.
scanWith :: Settings -> Scan a -> IO a
scanWith Settings{..} action = do
  envs0 <- Map.fromList <$> getEnvironment

  forM_ helpFlag $ \flag -> do
    when (flag `Map.member` envs0) $ do
      putStrLn $ help action
      exitSuccess

  (remainder, errors, result) <- go envs0 envs0 [] action
  mapM_ (uncurry errorLogger) errors
  case foldMap unusedLogger $ Map.keys remainder of
    Nothing -> pure ()
    Just m -> do
      m
      when pedantic exitFailure
  case result of
    Nothing -> exitFailure
    Just a -> pure a
  where
    go :: EnvMap -> EnvMap -> GroupStack -> Scan a -> IO (EnvMap, [(GroupStack, Error)], Maybe a)
    go _ envs _ (Pure a) = pure (envs, [], Just a)
    go allEnvs envs groupStack (Group name inner) = do
      let stack = name : groupStack
      go allEnvs envs stack inner
    go allEnvs envs groupStack (Scan ScanF{..} cont) = case parser (Map.lookup name allEnvs) of
      Left e -> do
        (remainder, errors, _) <- go allEnvs (Map.delete name envs) groupStack cont
        pure (remainder, (groupStack, e) : errors, Nothing)
      Right (display, v) -> do
        parseLogger groupStack name display
        (remainder, errors, func) <- go allEnvs (Map.delete name envs) groupStack cont
        pure (remainder, errors, ($ v) <$> func)

modifyError :: (Error -> Error) -> Scan a -> Scan a
modifyError func = go where
  go :: Scan a -> Scan a
  go (Pure a) = Pure a
  go (Scan v k) = Scan (apply v) (go k)
  go (Group name k) = Group name (go k)

  apply ScanF{..} = ScanF
    { name
    , parser = first func . parser
    , metavar
    }

-- | Display the list of environment variables and their default values in the dotenv format.
help :: Scan a -> String
help = unlines . go 1 where

  go :: Int -> Scan a -> [String]
  go _ (Pure _) = []
  go depth (Scan v k) = format v : go depth k
  go depth (Group name k) = (replicate depth '#' <> " " <> name) : go (depth + 1) k

  format ScanF{..} = name <> "=" <> maybe "" id metavar
