open Query
open QueryParser
open Sexplib

type indexed_projection =
    | AllIndices
    | ColumnIndices of int list

let deoptionalize l =
  let rec deopt acc = function
    | [] -> List.rev acc
    | None::tl -> deopt acc tl
    | Some x::tl -> deopt (x::acc) tl
  in 
  deopt [] l

let find_index list element =
    let rec go i l e =
        match l with
            | [] -> -1
            | h :: t -> if h = e then i else go (i + 1) t e
    in go 0 list element

let optional_nth list i =
    try
        Some (List.nth list i)
    with
        | _ -> None

let get_all_indices indices list =
   let found_values = List.map (fun i -> optional_nth list i) indices in
   deoptionalize found_values

let project values = function
    | AllIndices -> values
    | ColumnIndices indices ->
        get_all_indices indices values

let get_indexed_projection header_row = function
    | All -> AllIndices
    | Columns columns ->
        ColumnIndices (List.map (fun (Column_reference (table_name, col_name)) -> find_index header_row col_name) columns)

let get_value header_row values (_, column_name) =
    let col_index = find_index header_row column_name in
    List.nth values col_index

let invert = function
    | Number x -> Number (~-. x)
    | otherwise -> invalid_arg "Cannot invert expression which is not a boolean"

let is_equal a b = match a, b with
    | Number x, Number y -> x == y
    | Bool x, Bool y -> x == y
    | String x, String y -> x == y
    | otherwise -> invalid_arg "Incomparable values given to ="

let eval_equality_op op lhs rhs = match op with
    | Eq_equals -> Bool (is_equal lhs rhs)
    | Eq_not_equals -> Bool (not (is_equal lhs rhs))

let fail_operation op_string lhs rhs =
    let lhs_string = Sexp.to_string (sexp_of_value lhs) in
    let rhs_string = Sexp.to_string (sexp_of_value rhs) in
    let m = "Invalid operation: " ^ lhs_string ^ op_string ^ rhs_string in
    invalid_arg m

let eval_comparison_op op lhs rhs = match op with
    | Comp_greater_than ->
        (match (lhs, rhs) with
            | (Number x, Number y) -> Bool (x > y)
            | otherwise -> fail_operation ">" lhs rhs)
    | Comp_greater_than_equals ->
        (match (lhs, rhs) with
            | (Number x, Number y) -> Bool (x >= y)
            | otherwise -> fail_operation ">=" lhs rhs)
    | Comp_less_than_equals ->
        (match (lhs, rhs) with
            | (Number x, Number y) -> Bool (x <= y)
            | otherwise -> fail_operation "<=" lhs rhs)
    | Comp_less_than ->
        (match (lhs, rhs) with
            | (Number x, Number y) -> Bool (x < y)
            | otherwise -> fail_operation "<" lhs rhs)

let eval_term_op op lhs rhs = match op with
    | Term_minus ->
        (match (lhs, rhs) with
            | (Number x, Number y) -> Number (x -. y)
            | otherwise -> fail_operation "-" lhs rhs)
    | Term_plus ->
        (match (lhs, rhs) with
            | (Number x, Number y) -> Number (x +. y)
            | otherwise -> fail_operation "+" lhs rhs)

let eval_factor_op op lhs rhs = match op with
    | Factor_mult ->
        (match (lhs, rhs) with
            | (Number x, Number y) -> Number (x *. y)
            | otherwise -> fail_operation "*" lhs rhs)
    | Factor_div ->
        (match (lhs, rhs) with
            | (Number x, Number y) -> Number (x /. y)
            | otherwise -> fail_operation "/" lhs rhs)

let rec eval header_row values = function
    | Nil -> Bool false
    | Boolean_literal b -> Bool b
    | String_literal s -> String s
    | Numeric_literal n -> Number n
    | Reference column_reference ->
        (match parse_value (get_value header_row values column_reference) with
            | Error message -> invalid_arg message
            | Ok v -> v)
    | Unary (Unary_minus, e) -> invert (eval header_row values e)
    | Binary (op, lhs, rhs) ->
        let lhs_value = eval header_row values lhs in
        let rhs_value = eval header_row values rhs in
        match op with
            | Eq op -> eval_equality_op op lhs_value rhs_value
            | Comp op -> eval_comparison_op op lhs_value rhs_value
            | Term op -> eval_term_op op lhs_value rhs_value
            | Factor op -> eval_factor_op op lhs_value rhs_value

let condition_matches header_row values = function
    | None -> true
    | Some condition ->
        match eval header_row values condition with
            | Bool x -> x
            | otherwise -> invalid_arg "Where expression did not evaluate to a boolean value"

let execute_query = function
    | Select (projection, Table table_file, condition) ->
        let in_channel = open_in table_file in
        let header_line = input_line in_channel in
        let header_row = Str.split (Str.regexp ",") header_line in
        let indexed_projection = get_indexed_projection header_row projection in
        print_endline (String.concat "," (project header_row indexed_projection));
        try
            while true do
                let line = input_line in_channel in
                let values = Str.split (Str.regexp ",") line in
                if
                    condition_matches header_row values condition
                then
                    let projected_values = project values indexed_projection in
                    print_endline (String.concat "," projected_values)
                else
                    ()
            done
        with End_of_file ->
          close_in in_channel