# Revision history for EVP

## 0.1

* `string`, `yaml`, and `parse` now takes a new `Var` type which has an `IsString` instance
* Added `group` function to give a name to a group of parsers. Group names are purely cosmetic, and are displayed in the log messages.
* Added `helpFlag` (defaults to `EVP_HELP`). When this is set, EVP displays a summary of the parser in dotenv format.
* Added `defaultsTo`

## 0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
