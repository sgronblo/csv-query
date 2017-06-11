open MParser
open QueryParser
open QueryExecutor

let () =
    let query_string = Sys.argv.(1) in
    match parse_query query_string with
        | Success parsed_query ->
            execute_query parsed_query
        | Failed (message, _) ->
            print_string message