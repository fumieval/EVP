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
{-# LANGUAGE OverloadedStrings #-}

import EVP qualified

main :: IO ()
main = EVP.scan parser

-- ApplicativeDo is important here because Scan is not a monad.
parser :: EVP.Scan ()
parser = do
    -- @secret@ masks the parsed value
    _token <- EVP.secret $ EVP.string "API_TOKEN"
    -- parse the environment variable as a YAML value
    _port <- EVP.yaml "HTTP_PORT"
    -- obtain the environment variable as is
    _foo <- EVP.string "FOO"
    -- you can also provide a default value
    _debug <- EVP.yaml $ "DEBUG_MODE" `EVP.defaultsTo` False
    pure ()
```

Running this code produces the following output.

```
[EVP Info] API_TOKEN: <REDACTED>
[EVP Info] HTTP_PORT: 8080
[EVP Info] FOO: foo
[EVP Info] DEBUG_MODE: False (default)
```

Revealing unused variables
----

EVP has a mechanism to detect unused variables.

If your application's environment variables has a common prefix `MYAPP_`, you can set `assumePrefix` as a `unusedLogger`.

```haskell
EVP.scanWith EVP.def
    { EVP.unusedLogger = EVP.assumePrefix "MYAPP_" }
    parser
```

If an environment variable prefixed by `MYAPP_` is set but not referred to, EVP prints the following warning. This is useful for detecting typos too.

```
[EVP Warn] MYAPP_OBSOLETE_FLAG is set but not used
```

Alternatively, you can also name unwanted variables individually:

```haskell
EVP.scanWith EVP.def
    { EVP.unusedLogger = EVP.obsolete ["OBSOLETE_VAR"] }
```

These can be combined using the `Semigroup` instance.

```haskell
EVP.scanWith EVP.def
    { EVP.unusedLogger = EVP.assumePrefix "MYAPP_" <> EVP.obsolete ["OBSOLETE_VAR"] }
    parser
```

Parser group
----

You can provide an additional context to a parser by applying `group "group name"`.
Group names appear in the log messages:

```
[EVP Info] DEBUG_MODE: False (default)
[EVP Info/MySQL] MYSQL_HOST: localhost
[EVP Info/MySQL] MYSQL_PASSWORD: <REDACTED>
[EVP Error/MySQL] Failed to parse MYSQL_PORT=blah: Aeson exception:
[EVP Error/MySQL] Error in $: parsing Int failed, expected Number, but encountered String
```

Design context
----

* If `Scan` were a monad, the parsing process would be non-deterministic. This might cause a burden when there are two or more problems in the environment variables, because it is not possible to reveal all problems in a non-deterministic context.
* It is recommended to avoid falling back to default values expect for debugging features. If the application configuration has an error such as a typo, it is much safer to exit than launching anyway with a default value.
