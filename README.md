# hasqltable
Simple Querying over (CSV) Tables in Haskell

## Usage

`cabal run hasqltable path_to_table schema [-d|--delimiter delimiter]`

* `path_to_table` - a path to a (so far only) CSV file
* `schema` - a string made of the characters `s`, `d`, `i` or `b` indicating the type of columns (string, double, int, bool) in the same order
* `delimiter` - a string determining the delimiter of the CSV file, `,` by default

The program runs in a REPL. To quit, type `quit` as the command.

## Tests

Simple testing framework is implemented to test some functionality:

`cabal test`