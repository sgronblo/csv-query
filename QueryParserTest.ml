open Angstrom
open Core
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
    let actual_query = unwrap_result (parse_query query_string) in
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

let parse_where_query_numeric_equality =
    parse_test "select * from employees.csv where id = 666" (Select (All, Table "employees.csv", Some (Binary ((Eq Eq_equals), Reference (None, "id"), Numeric_literal 666.))))

let parse_where_query_numeric_comparison_a =
    parse_test "select * from employees.csv where age > 65" (Select (All, Table "employees.csv", Some (Binary ((Comp Comp_greater_than), Reference (None, "age"), Numeric_literal 65.))))

let parse_where_query_numeric_comparison_b =
    parse_test "select * from employees.csv where 2 >= 4" (Select (All, Table "employees.csv", Some (Binary ((Comp Comp_greater_than_equals), Numeric_literal 2., Numeric_literal 65.))))

let some x = Some x

let unary_op_gen = QCheck.Gen.oneofl [
    Unary_minus
]

let equality_op_gen = QCheck.Gen.oneofl [
    Eq Eq_equals;
    Eq Eq_not_equals
]

let comparison_op_gen = QCheck.Gen.oneofl [
    Comp Comp_greater_than;
    Comp Comp_greater_than_equals;
    Comp Comp_less_than;
    Comp Comp_less_than_equals;
]

let arithmetic_op_gen = QCheck.Gen.oneofl [
    Term Term_minus;
    Term Term_plus;
    Factor Factor_div;
    Factor Factor_mult;
]

let boolean_op_gen = QCheck.Gen.oneof [
    equality_op_gen;
    comparison_op_gen
]

let binary_op_gen = QCheck.Gen.oneof [
    equality_op_gen;
    comparison_op_gen;
    arithmetic_op_gen
]

let numeric_literal_gen =
    QCheck.Gen.(float >|= (fun rand_n ->
        Numeric_literal (float_of_string (string_of_float rand_n))))

let boolean_literal_gen =
    QCheck.Gen.(bool >|= (fun b -> Boolean_literal b))

let string_literal_gen =
    QCheck.Gen.(small_string >|= (fun s -> String_literal s))

let expression_leaf column_names =
    let open QCheck.Gen in
    frequency [
      (1, return Nil);
      (2, boolean_literal_gen);
      (2, string_literal_gen);
      (2, numeric_literal_gen);
      (2, oneofl column_names >|= fun c -> Reference (None, c))
    ]

let rec numeric_expression n =
    let open QCheck.Gen in
    arithmetic_op_gen >>= fun op ->
    match n with
    | 0 -> numeric_literal_gen
    | n ->
        numeric_expression (n / 2) >>= fun operand1 ->
        numeric_expression (n / 2) >>= fun operand2 ->
        return (Binary (op, operand1, operand2))
and boolean_expression n =
    let open QCheck.Gen in
    match n with
    | 0 -> boolean_literal_gen
    | n ->
        frequency [
            (1, equality_expression (n / 2));
            (1, comparison_expression (n / 2))
        ]
and comparison_expression n =
    let open QCheck.Gen in
    match n with
    | 0 -> boolean_literal_gen
    | n ->
        comparison_op_gen >>= fun op ->
        numeric_expression (n / 2) >>= fun operand1 ->
        numeric_expression (n / 2) >>= fun operand2 ->
        return (Binary (op, operand1, operand2))
and equality_expression n =
    let open QCheck.Gen in
    equality_op_gen >>= fun eq_op ->
    oneof [
        pair (numeric_expression n) (numeric_expression n);
        pair (boolean_expression n) (boolean_expression n);
        pair (string_expression n) (string_expression n);
    ] >>= fun (e1, e2) ->
    return (Binary (eq_op, e1, e2))
and string_expression n = string_literal_gen

let expression_generator column_names = QCheck.Gen.(sized @@ fix
    (fun self n ->
        match n with
        | 0 -> expression_leaf column_names
        | n -> frequency [
            (1, map2 (fun op e -> Unary (op, e)) unary_op_gen (numeric_expression (n - 1)));
            (3, map3 (fun op e e2 -> Binary (op, e, e2)) binary_op_gen (self (n / 2)) (self (n / 2)));
        ]
    ))

let char_sequence start_char end_char =
    let start_ascii = Core.Char.to_int start_char in
    let end_ascii = Core.Char.to_int end_char in
    String.init (end_ascii - start_ascii) (fun i -> Core.Char.of_int_exn (start_ascii + i))

let identifier_chars =
    char_sequence 'a' 'z' ^
    char_sequence 'A' 'Z'

let identifier_char_gen st = String.get identifier_chars (Caml.Random.State.int st (Core.String.length identifier_chars))

let identifier_string_gen =
    QCheck.string_gen_of_size QCheck.Gen.(1--10) identifier_char_gen

let non_empty_small_list = QCheck.Gen.(list_size (1--10))

let query_generator = QCheck.Gen.(
    non_empty_small_list identifier_string_gen.gen >>= fun column_names ->
    identifier_string_gen.gen >>= fun table_name ->
    frequency [
        (1, return None);
        (4, map some (expression_generator column_names))
    ] >>= fun where ->
    let column_references = List.map ~f:(fun c -> Column_reference (None, c)) column_names in
    let projection = Columns column_references in
    return (Select (projection, Table table_name, where))
)

let boolean_literal b = Boolean_literal b
let string_literal s = String_literal s

let rec expression_iter expression =
    let open QCheck.Shrink in
    let open QCheck.Iter in
    match expression with
    | Nil -> empty
    | Boolean_literal b -> return (not b) >|= boolean_literal
    | String_literal s -> QCheck.Shrink.string s >|= string_literal
    | Numeric_literal n -> empty
    | Reference _ -> empty
    | Unary (_, expression) ->
        return expression <+> expression_iter expression
    | Binary (_, e1, e2) ->
        of_list [e1; e2] <+> expression_iter e1 <+> expression_iter e2

let shrink_query (Select (projection, table, optional_condition)) =
    let open QCheck.Iter in
    let update_condition c = Select (projection, table, Some c) in
    match optional_condition with
    | None -> empty
    | Some expression ->
        expression_iter expression >|= update_condition

let arbitrary_query = QCheck.make
    ~print:query_to_string
    ~shrink:shrink_query
    query_generator

let serializing_and_parsing =
    QCheck.Test.make
        ~count:100
        ~name:"parsing a serialized random query"
        arbitrary_query
        (fun q ->
            let query_string = to_query_string q in
            match parse_query query_string with
            | Ok reconstructed_query ->
                if q <> reconstructed_query
                then
                    let failure_message =
                        " -> \n\"" ^
                        query_string ^
                        "\"\n -> \n" ^
                        query_to_string reconstructed_query in
                    QCheck.Test.fail_report failure_message
                else true
            | Error msg ->
                let failure_message =
                    " -> \"" ^
                    query_string ^
                    "\" -> " ^
                    msg in
                QCheck.Test.fail_report failure_message)

let suite = "select query suite" >:::
    [
        "parsing a simple * query" >:: parse_simple_all_projection_query;
        "parsing a simple query with projection" >:: parse_simple_projection_query;
        "parsing a simple query with prefixed columns" >:: parse_simple_projection_query_with_prefixed_columns;
        "parsing a simple query with a where with a boolean literal" >:: parse_where_query_boolean_literal;
        "parsing a simple query with a where with a boolean column reference" >:: parse_where_query_boolean_column_reference;
        "parsing a simple query with a numeric equality" >:: parse_where_query_numeric_equality;
        "parsing a simple query with a numeric comparison a" >:: parse_where_query_numeric_comparison_a;
        "parsing a simple query with a numeric comparison b" >:: parse_where_query_numeric_comparison_b;
        QCheck_runner.to_ounit2_test serializing_and_parsing
    ]

let () =
    run_test_tt_main suite