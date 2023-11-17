{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

import Data.Text (Text)
import EVP

main :: IO ()
main = do
    EVP.scanWith EVP.def
        { EVP.unusedLogger = EVP.assumePrefix "MYAPP_" <> EVP.obsolete ["OBSOLETE_VAR"] }
        parser
    putStrLn "Hello, world!"

-- ApplicativeDo is important here because Scan is not a monad.
parser :: EVP.Scan ()
parser = do
    -- @secret@ disables logging
    _token :: String <- EVP.secret $ EVP.string "API_TOKEN"
    -- parse the environment variable as a YAML value
    _port :: Int <- EVP.yaml "HTTP_PORT"
    -- obtain the environment variable as is
    _ :: Text <- EVP.string "FOO"
    -- you can also provide a default value
    _ :: Bool <- EVP.yaml $ "DEBUG_MODE" `EVP.defaultsTo` False
    _ <- EVP.group "MySQL" $ do
        _ :: String <- EVP.string $ "MYSQL_HOST" `EVP.defaultsTo` "localhost"
        _ :: Int <- EVP.yaml $ "MYSQL_PORT" `EVP.defaultsTo` 3306
        _ :: String <- EVP.secret $ EVP.string "MYSQL_PASSWORD"
        pure ()
    pure ()

{-
[EVP Info] API_TOKEN: <REDACTED>
[EVP Info] HTTP_PORT: 8080
[EVP Info] FOO: foo
[EVP Info] DEBUG_MODE: False (default)
-}
