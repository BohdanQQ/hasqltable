# hasqltable
Simple Querying over (CSV) Tables in Haskell

# User Documentation

## Usage

`hasqltable FILE SCHEMA [-d|--delimiter ARG]`

Available options:
 * SCHEMA - A string of the b/i/d/s characters representing bool/int/double/string column value in the order as they appear (e.g. "siddd" means the first colum is string, second is int, etc.)
 * -d,--delimiter ARG       The column delimiter

The program runs in a REPL. To quit, type `quit` or `exit` as the command.

## Querying

The REPL is iterative - your querries are applied to the last valid result or the initial table. If you wish to reset the iteration, i.e. start over, enter `reload` or `reset` as a command.

The following query structure is suported, clauses are case **insensitive**

`SELECT columnList [optional ordered clauses]`

* `columnList` - a list of column names separated by a `,`, the names must be either plaintext or delimited by \` (to differentiate between the colum named \`where\` and the `where` clause)
  * example: 
``` 
SELECT name, age, `limit`

SELECT name

```
### optional ordered clauses

Other clauses are optional and independent of each other apart from their order. The query may use any subset of the following clauses: `where, groupby, orderby, limit`

### Where clause

`WHERE simplExpr`

Filters out rows for which the `simplExpr` evaluates false.

* `simplExpr` is either:
  * `boolean literal`
  * `l bop l`
  * `l aop l bop l aop l` which translates into `(l aop l) bop (l aop l)`
  * `l bop l aop l` (and vice versa) which translates into `l bop (l aop l)`
    * `l` is a `numeric literal`, `string literal`, `column reference` or `boolean literal`
      * `numeric literal` is an integer or a double (regular notation with `.` as the decimal separator, e.g. `3.14159`)
      * `boolean literal` is `true` or `false` (case insensitive)
      * `string literal` is a string enclosed in `"`
      * `column reference` is a column name enclosed in \` (e.g. \`myColumn\`)
    * `aop` is an arithmetic operator `+ - / *`
    * `bop` is a boolean-result operator `> < <= >= == !=, &, |, ^` (`^` for XOR) and `n` is a numeric literal (integer/double) or a column reference
    * `column reference` is the column name enclosed in \` (e.g. \`name\`)
* type errors arising from incorrect usage of operators are handled during query execution
* example using the `where` clause: 

```
SELECT name, surname WHERE `scorePerDay` * 5 >= `scoreGoal` / 2

SELECT age WHERE `name` != "BohdanQQ"
```

### Groupby clause

`GROUPBY columnList`

Groups rows based on equality of the columns specified in columnList, filters out only the first row in each group.


* see [column list description of the SELECT clause](##Querying)

```
SELECT department, building GROUPBY department, building
```

### Orderby clause

`ORDERBY [asc|desc] columnList`

Orders based on the order specified. Columns closer to the beginning of the column list are prioritized over the following colums.

* `asc` or `desc` strings are case insensitive
* see [column list description of the SELECT clause](##Querying)

```
SELECT name, surname ORDERBY asc surname, name
```

### Limit clause

`LIMIT n`

Limits the output to `n` rows.

An example of a complex query:

```
select name, surname, department where `age` / `scorePerDay` <= 12 groupby building orderby asc name, surname limit 2
```

## Saving the table

To save the current table (the last valid table printed out), type `save`, hit enter and then input a name which will be used for **both** the table file **and** the table schema file (schema file will have `.schema` appended to the name) and hit enter once again.

# Solution documentation

* `split, optparse-applicative` - dependencies for string splitting, argument parsing
* `Either` is used to pass transformed tables or (string) errors

## Files

`Main.hs` creates the functional core - the REPL and argument parsing

`Config.hs` and `Parser.hs` define the command line arugments format and the query parser respectively.

`Types.hs` defines all used data structures, including the (core) `Cell`, `Expr` and `SubQuery` types as well as some basic functions (operator functions) and typeclasses with their implementations (runtime type compatibility check using newly defined `Compat`, `Num` instance for numeric cells, ...)

## Querying

I have used our lab/hw parser for parsing queries. Parsed query is a simple list of clauses which is then reordered for execution (SELECT for example executes nearly at the end of the entire query ). 

The WHERE clause expression does NOT support AST parsing (see user documentation for allowed expression types), but I think
that it is possible to implement this using the existing parser and `Expr` data structure.

## Table representation, operations on the cells

The table is represented using the `Cell` type which is a sort of "discriminated union". This type is used: 

a) to hold the data inside a single table cell

b) along with a column name, to specify column's type (as a part of the table schema)

This table representation provides sufficient information to perform the transormations required by the clauses. Additionally, runtime "type checks" are performed when evaluating the WHERE clause expression so that errors do not cause a crash.

## Tests & Docs

Simple testing framework is implemented to test some functionality.

To run tests: 
* `cabal test`

To generate documentation: 
* `cabal haddock --haddock-executables`


## Possible TODOs / imperfections
* enable full tree syntax for the where clause expression
* parse-time type checking (requires a parser with a state + schema available to it)
* schema column type does not need to be specified by a `Cell` (which also holds value - unused in the schema)
* unary operator support (particularly boolean not, which can be implemented using `xor True`)
* `[DONE]` implement saving of the intermediate results into a file (command `save fileName`)
* `[DONE]` implement reloading the original table (`reload` command)
* add an option to specify schema by file (consistent with the saving method)