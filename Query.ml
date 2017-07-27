open Sexplib.Std

type file_name = string
    [@@deriving sexp]

type equality_operator =
    | Eq_equals
    | Eq_not_equals
    [@@deriving sexp]

type comparison_operator =
    | Comp_greater_than
    | Comp_greater_than_equals
    | Comp_less_than_equals
    | Comp_less_than
    [@@deriving sexp]

type term_operator =
    | Term_plus
    | Term_minus
    [@@deriving sexp]

type factor_operator =
    | Factor_mult
    | Factor_div
    [@@deriving sexp]

type binary_operator =
    | Eq of equality_operator
    | Comp of comparison_operator
    | Term of term_operator
    | Factor of factor_operator
    [@@deriving sexp]

type unary_operator =
    | Unary_minus
    [@@deriving sexp]

type column_name = string
    [@@deriving sexp]

type table_name = string
    [@@deriving sexp]

(*type row = 'a 'b map*)
type value =
    | Bool of bool
    | Number of float
    | String of string
    [@@deriving sexp]

type table =
    Table of file_name
    [@@deriving sexp]

type column =
    Column_reference of (table_name option * column_name)
    [@@deriving sexp]

type projection =
    | All
    | Columns of column list
    [@@deriving sexp]

type expression =
    | Nil
    | Boolean_literal of bool
    | String_literal of string
    | Numeric_literal of float
    | Reference of (table_name option * column_name)
    | Binary of (binary_operator * expression * expression)
    | Unary of (unary_operator * expression)
    [@@deriving sexp]

type query =
    Select of (projection * table * expression option)
    [@@deriving sexp]

(*
example query: select e.name, d.name from employees.csv as e join departments.csv as d where d.name = accounting
*)

let print_table = function
    | Table table_name -> "(Table " ^ table_name ^ ")"

let print_column = function
    | Column_reference (Some table_name, column_name) -> table_name ^ "." ^ column_name
    | Column_reference (None, column_name) -> column_name

let print_projection = function
    | All -> "All"
    | Columns columns ->
        let column_names = List.map print_column columns in
        "[" ^ String.concat ", " column_names ^ "]"

let print_binary_operator = function
    | Eq Eq_equals -> "="
    | Eq Eq_not_equals -> "!="
    | Comp Comp_greater_than -> ">"
    | Comp Comp_greater_than_equals -> ">="
    | Comp Comp_less_than -> "<"
    | Comp Comp_less_than_equals -> "<="
    | Term Term_plus -> "+"
    | Term Term_minus -> "-"
    | Factor Factor_mult -> "*"
    | Factor Factor_div -> "/"

let rec print_expression = function
    | Nil -> "nil"
    | Boolean_literal b -> string_of_bool b
    | String_literal s -> s
    | Numeric_literal n -> string_of_float n
    | Reference (None, cn) -> cn
    | Reference (Some table_name, cn) -> table_name ^ "." ^ cn
    | Binary (operator, e1, e2) -> print_expression e1 ^ " " ^ (print_binary_operator operator) ^ " " ^ (print_expression e2)
    | Unary (_, e) -> "-" ^ (print_expression e)

let print_query = function
    | Select (projection, table, option_expression) ->
        let projection_string = print_projection projection in
        let table_string = print_table table in
        let where_string = match option_expression with
            | None -> ""
            | Some e -> " where " ^ print_expression e in
        "select (" ^ projection_string ^ ", " ^ table_string ^ where_string ^ ")"