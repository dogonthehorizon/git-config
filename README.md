# git-config

A simple parser for [Git configuration] files.

## Getting Started

This project is built using [Stack], make sure you have it installed before
proceeding.

You can fire up an interactive session like so:

```
stack ghci
```

The library can be built or tested like so:

```
# Building
stack build
# Running tests
stack test
```

## Usage

A Git configuration is a colletion of sections that contain mappings of keys
to values.

For the sake of simplicity this is represented as `[Section]` where a
`Section` is a collection of section names and a mapping of keys to values.

We can use the parser like so:

```haskell
import qualified Data.Text.IO as TIO
import Text.GitConfig.Parser (parseConfig)

main :: IO ()
main = do
  file <- TIO.readFile ".git/config"
  case parseConfig file of
    Right conf ->
      print conf
    Left error ->
      print error
```

If you'd like to do your own parsing you can import the individual combinators
from the `Text.GitConfig.Parser` module.

[Stack]: https://docs.haskellstack.org/en/stable/README/
[Git configuration]: https://git-scm.com/docs/git-config/2.16.0#_syntax
