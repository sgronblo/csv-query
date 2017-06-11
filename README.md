# csv_query - A command line tool for performing SQL queries on CSV files

Simple usage:

```
make
./csv_query.native 'select * from employees.csv'
```

## How to build

Run `make` to run the default target which builds the csv_query.native binary.

###

1. mparser
2. ounit (for testing)
3. str

I still need to figure out how to extract the dependency specificaiton to a file and what command to use to automatically install them. I installed them manually using opam, eg. `opam install mparser` on my own machine.

## How to test

Run `make test` to compile and run the test suite.

## How does it work?

The query string is parsed using a simple monadic parser constructed using [MParser](https://github.com/cakeplus/mparser). The code for the parser is located in QueryParser.ml. The parser returns a `query` data structure which can be found in Query.ml. The `query` can be executed by the `execute_query` function in QueryExecutor.ml to produce output based on the given SQL query.

## Supported SQL features

- SELECT *
- SELECT a, b
- csv files as table names, eg. FROM employees.csv

## Future work

- Add more SQL features (WHERE, JOIN, aggregate functions)
- Add method for fetching dependencies easier
- Add property based testing which would automatically generate a Query data value, print it as a string and then parse it back and check that the parsed query matches the original randomly generated one
- Improve build process (I'm terrible at writing Makefiles)

## Dev environment

I use VSCode with the OCaml plugin and Merlin. The code is built using `ocamlbuild` and `ocamlfind`