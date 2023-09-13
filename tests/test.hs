{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}

import EVP qualified

-- | List of environment variables defined as a GADT.
-- The use of GADT ensures that the identifiers correspond to the actual variable names,
-- and the parsed values are well-typed
data Var a where
    API_TOKEN :: Var String
    HTTP_PORT :: Var Int
    FOO :: Var String
    DEBUG_MODE :: Var Bool
deriving instance Show (Var a)

main :: IO ()
main = EVP.scan parser

-- ApplicativeDo is important here because Scan is not a monad.
parser :: EVP.Scan ()
parser = do
    -- @secret@ disables logging
    _token <- EVP.secret $ EVP.string API_TOKEN
    -- parse the environment variable as a YAML value
    _port <- EVP.yaml HTTP_PORT
    -- obtain the environment variable as is
    _ <- EVP.string FOO
    -- you can also provide a default value
    _ <- EVP.yamlDefault DEBUG_MODE False
    pure ()

{-
[EVP Info] API_TOKEN: <REDACTED>
[EVP Info] HTTP_PORT: 8080
[EVP Info] FOO: foo
[EVP Info] DEBUG_MODE: False (default)
-}
