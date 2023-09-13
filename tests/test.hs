{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}

import EVP qualified

data Var a where
    API_TOKEN :: Var String
    HTTP_PORT :: Var Int
    FOO :: Var String
    DEBUG_MODE :: Var Bool
deriving instance Show (Var a)

main :: IO ()
main = EVP.scan $ do
    -- @secret@ disables logging
    _token <- EVP.secret $ EVP.string API_TOKEN
    -- parse the environment variable as a YAML value
    _port <- EVP.yaml HTTP_PORT
    -- obtain the environment variable as is
    _ <- EVP.string FOO
    _ <- EVP.yamlDefault DEBUG_MODE False
    pure ()

{-
[EVP Info] API_TOKEN: <REDACTED>
[EVP Info] HTTP_PORT: 8080
[EVP Info] FOO: foo
[EVP Info] DEBUG_MODE: False (default)
-}
