open Angstrom
open OUnit2
open Query
open QueryParser
open Sexplib.Std

exception Parse_failure of string

let unwrap_result = function
    | Ok a -> a
    | Error message -> assert_failure message

let query_to_string q = Sexplib.Sexp.to_string_hum (sexp_of_query q)

let parse_test query_string expected_query test_ctxt =
    let input_string = `String query_string in
    let actual_query = unwrap_result (parse_only query_parser input_string) in
    let error_msg = "Expected query: " ^ (query_to_string expected_query) ^ " was not equal to actual query: " ^ (query_to_string actual_query) in
    assert_equal ~msg:error_msg actual_query expected_query

(*let parse_column_reference text_ctxt =
    let actual = column_reference "employees.name" (Column_reference (Some "employees", "name"))*)

let parse_simple_all_projection_query =
    parse_test "select * from helloworld.csv" (Select (All, Table "helloworld.csv", None))

let parse_simple_projection_query_with_prefixed_columns =
    parse_test "select employees.name, employees.id from employees.csv" (Select (Columns [Column_reference (Some "employees", "name"); Column_reference (Some "employees", "id")], Table "employees.csv", None))

let parse_simple_projection_query =
    parse_test "select hello, world from helloworld.csv" (Select (Columns [Column_reference (None, "hello"); Column_reference (None, "world")], Table "helloworld.csv", None))

let parse_where_query_boolean_literal =
    parse_test "select * from helloworld.csv where true" (Select (All, Table "helloworld.csv", Some (Boolean_literal true)))

let parse_where_query_boolean_column_reference =
    parse_test "select * from employees.csv where is_retired" (Select (All, Table "employees.csv", Some (Reference (None, "is_retired"))))

let parse_where_query_numeric_comparison =
    parse_test "select * from employees.csv where id = 666" (Select (All, Table "employees.csv", Some (Binary ((Eq Eq_equals), Reference (None, "id"), Numeric_literal 666.))))

let suite = "select query suite" >:::
    [
        "parsing a simple * query" >:: parse_simple_all_projection_query;
        "parsing a simple query with projection" >:: parse_simple_projection_query;
        "parsing a simple query with prefixed columns" >:: parse_simple_projection_query_with_prefixed_columns;
        "parsing a simple query with a where with a boolean literal" >:: parse_where_query_boolean_literal;
        "parsing a simple query with a where with a boolean column reference" >:: parse_where_query_boolean_column_reference;
        "parsing a simple query with a where with an equals" >:: parse_where_query_numeric_comparison
    ]

let () =
    run_test_tt_main suite