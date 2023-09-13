EVP: Environment Variable Parser
====

EVP is a simple environment parser which focues on these three aspects:

* Ease of use: no complicated machinery is needed
* Observability: for each environment variable, EVP logs the parsed value or an error. An error does not interrupt the parsing process and it checks all the variables exhaustively.
* Composability: environment parsers can be composed via the Applicative structure.

Example
----

The following code is a complete example demonstrating how to use EVP:

```haskell
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
    -- @secret@ masks the parsed value
    _token <- EVP.secret $ EVP.string API_TOKEN
    -- parse the environment variable as a YAML value
    _port <- EVP.yaml HTTP_PORT
    -- obtain the environment variable as is
    _foo <- EVP.string FOO
    -- you can also provide a default value
    _debug <- EVP.yamlDefault DEBUG_MODE False
    pure ()
```

Running this code produces the following output.

```
[EVP Info] API_TOKEN: <REDACTED>
[EVP Info] HTTP_PORT: 8080
[EVP Info] FOO: foo
[EVP Info] DEBUG_MODE: False (default)
```

Design context
----

* If `Scan` were a monad, the parsing process would be non-deterministic. This might cause a burden when there are two or more problems in the environment variables, because it is not possible to reveal all problems in a non-deterministic context.
* It is recommended to avoid falling back to default values expect for debugging features. If the application configuration has an error such as a typo, it is much safer to exit than launching anyway with a default value.