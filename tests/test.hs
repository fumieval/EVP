{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}

import EVP qualified

data Var a where
    API_TOKEN :: Var String
    HTTP_PORT :: Var Int
    FOO :: Var String
deriving instance Show (Var a)

main :: IO ()
main = EVP.scan $ do
    _token <- EVP.secret $ EVP.string API_TOKEN
    _port <- EVP.yaml HTTP_PORT
    _ <- EVP.string FOO
    pure ()
