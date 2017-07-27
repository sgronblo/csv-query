open Angstrom
open QueryParser
open QueryExecutor

let () =
    let query_string = Sys.argv.(1) in
    match parse_query query_string with
        | Ok parsed_query ->
            print_string "Parsed query";
            execute_query parsed_query
        | Error message ->
            print_string "Failed to parse query";
            print_string message