open MParser
open OUnit2
open Query
open QueryParser

exception Parse_failure of string

let unwrap_result = function
    | Success a -> a
    | Failed (message, _) -> assert_failure message

let parse_test query_string expected_query test_ctxt =
    let actual_query = unwrap_result (parse_string query_parser query_string ()) in
    let error_msg = "Expected query: " ^ (print_query expected_query) ^ " was not equal to actual query: " ^ (print_query actual_query) in
    assert_equal ~msg:error_msg actual_query expected_query

let parse_simple_all_projection_query =
    parse_test "select * from helloworld.csv" (Select (All, Table "helloworld.csv"))

let parse_simple_projection_query =
    parse_test "select hello, world from helloworld.csv" (Select (Columns [Column "hello"; Column "world"], Table "helloworld.csv"))

let suite = "select query suite" >:::
    [
        "parsing a simple * query" >:: parse_simple_all_projection_query;
        "parsing a simple query with projection" >:: parse_simple_projection_query
    ]

let () =
    run_test_tt_main suite