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

let some x = Some x

let unary_op_gen = QCheck.Gen.(
    oneof [
        return Unary_minus
    ]
)

let binary_op_gen = QCheck.Gen.(
    oneof [
        return (Eq Eq_equals);
        return (Eq Eq_not_equals);
        return (Comp Comp_greater_than);
        return (Comp Comp_greater_than_equals);
        return (Comp Comp_less_than);
        return (Comp Comp_less_than_equals);
        return (Term Term_minus);
        return (Term Term_plus);
        return (Factor Factor_div);
        return (Factor Factor_mult);
    ]
)

let expression_generator column_names = QCheck.Gen.(sized @@ fix
    (fun self n ->
        print_endline ("expression " ^ string_of_int n);
        match n with
        | 0 -> frequency [
            (1, return Nil);
            (2, bool >|= fun b -> Boolean_literal b);
            (2, small_string >|= fun s -> String_literal s);
            (2, float >|= fun n -> Numeric_literal n);
            (2, oneofl column_names >|= fun c -> Reference (None, c))
        ]
        | n -> frequency [
            (1, map2 (fun op e -> Unary (op, e)) unary_op_gen (self (n - 1)));
            (3, map3 (fun op e e2 -> Binary (op, e, e2)) binary_op_gen (self (n - 1)) (self (n - 1)));
        ]
    ))

let char_sequence start_char end_char =
    let start_ascii = Core.Char.to_int start_char in
    let end_ascii = Core.Char.to_int end_char in
    String.init (end_ascii - start_ascii) (fun i -> Core.Char.of_int_exn (start_ascii + i))

let identifier_chars =
    char_sequence 'a' 'z' ^
    char_sequence 'A' 'Z'

let identifier_char_gen st = String.get identifier_chars (Random.State.int st (Core.String.length identifier_chars))

let identifier_string_gen =
    QCheck.string_gen_of_size QCheck.Gen.(1--10) identifier_char_gen

let query_generator = QCheck.Gen.(
    small_list identifier_string_gen.gen >>= fun column_names ->
    identifier_string_gen.gen >>= fun table_name ->
    frequency [
        (1, return None);
        (4, map some (expression_generator column_names))
    ] >>= fun where ->
    let column_references = List.map (fun c -> Column_reference (None, c)) column_names in
    let projection = Columns column_references in
    return (Select (projection, Table table_name, where))
)

let arbitrary_query = QCheck.make
    ~print:query_to_string
    query_generator

let serializing_and_parsing =
    QCheck.Test.make
        ~count:1
        ~name:"parsing a serialized random query"
        arbitrary_query
        (fun q ->
            let query_string = to_query_string q in
            print_endline ("generated query " ^ query_string);
            match parse_query query_string with
            | Ok reconstructed_query ->
                if q <> reconstructed_query
                then
                    let failure_message =
                        "\n" ^
                        query_to_string q ^
                        "\n -> \n" ^
                        query_string ^
                        "\n -> \n" ^
                        query_to_string reconstructed_query in
                    print_endline failure_message;
                    QCheck.Test.fail_report failure_message
                else true
            | Error msg ->
                let failure_message =
                    query_to_string q ^
                    " -> " ^
                    query_string ^
                    " -> " ^
                    msg in
                print_endline failure_message;
                QCheck.Test.fail_report failure_message)

let suite = "select query suite" >:::
    [
        "parsing a simple * query" >:: parse_simple_all_projection_query;
        "parsing a simple query with projection" >:: parse_simple_projection_query;
        "parsing a simple query with prefixed columns" >:: parse_simple_projection_query_with_prefixed_columns;
        "parsing a simple query with a where with a boolean literal" >:: parse_where_query_boolean_literal;
        "parsing a simple query with a where with a boolean column reference" >:: parse_where_query_boolean_column_reference;
        "parsing a simple query with a numeric comparison" >:: parse_where_query_numeric_comparison;
        QCheck_runner.to_ounit2_test serializing_and_parsing
    ]

let () =
    run_test_tt_main suite