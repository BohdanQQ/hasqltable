# hasqltable
(WIP) Simple Querying over Tables in Haskell

## Usage

`cabal run hasqltable path_to_table schema delimter`

* `path_to_table` - a path to a (so far only) CSV file
* `schema` is a string made of the characters `s`, `d`, `i` or `b` indicating the type (string, double, int, bool)
* `delimiter` is a string determining the delimiter of the CSV file

The program runs in a loop of `prompt -> command -> result`. To quit, type `quit` as the command.

## Tests

Simple testing framework is implemented to test some functionality:

`cabal test`