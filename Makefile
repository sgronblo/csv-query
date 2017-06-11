.DEFAULT_GOAL := csv_query.native

test : QueryParserTest.native
	./QueryParserTest.native

QueryParserTest.native : QueryParserTest.ml QueryParser.ml Query.ml
	ocamlbuild -use-ocamlfind 'QueryParserTest.native'

csv_query.native : csv_query.ml Query.ml QueryParser.ml QueryExecutor.ml
	ocamlbuild -use-ocamlfind 'csv_query.native'