{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}

import Data.Text (Text)
import EVP qualified

main :: IO ()
main = EVP.scanWith EVP.def
    { EVP.unusedLogger = EVP.assumePrefix "MYAPP_" <> EVP.obsolete ["OBSOLETE_VAR"] }
    parser

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
    _ :: Bool <- EVP.yamlDefault "DEBUG_MODE" False
    pure ()

{-
[EVP Info] API_TOKEN: <REDACTED>
[EVP Info] HTTP_PORT: 8080
[EVP Info] FOO: foo
[EVP Info] DEBUG_MODE: False (default)
-}
