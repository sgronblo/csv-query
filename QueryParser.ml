open Angstrom
open Query

let whitespace = satisfy (function
    | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
    | _ -> false) <?> "expected whitespace"

let alphanum = satisfy (function
    | 'a'..'z' | 'A'..'Z' | '0'..'9' -> true
    | _ -> false) <?> "expected alphanumeric char"

let skip_string s =
    string s *> return () <?> "expected string " ^ s

let digit = satisfy (function
    | '0'..'9' -> true
    | _ -> false) <?> "expected a digit"

let pair = lift2 (fun x y -> (x, y))

let blank =
  satisfy (function '\t' | ' ' -> true | _ -> false) <?> "expected a tab or a space"

let optional_p p =
    let to_some x = Some x in
    option None (to_some <$> p)

let skip_char c = skip (fun parsed_char -> c = parsed_char) <?> "expected a " ^ (String.make 1 c)

let identifier_char = alphanum <|> char '_' <?> "expected an identifier char"

let join_chars cl = String.concat "" (List.map (String.make 1) cl)

let many1_chars char_parser =
    join_chars <$> many1 char_parser

let table_file_name = many1_chars (identifier_char <|> char '.') <?> "expected a table file name"

let table_name = many1_chars identifier_char <?> "expected a table name"

let column_name = many1_chars identifier_char <?> "expected a column name"

let column_reference =
    optional_p (table_name <* skip_char '.') >>= fun maybe_table_name ->
    column_name >>= fun colname ->
    return (maybe_table_name, colname)
    <?> "expected a column reference"

let csv p =
    p >>= fun parsed_head ->
    many (skip_char ',' *> whitespace *> p) >>= fun parsed_rest ->
    return (parsed_head :: parsed_rest)

let column_references_to_columns column_tuples =
    let mapped_columns = List.map (fun c -> Column_reference c) column_tuples in
    Columns mapped_columns

let projection_parser =
    (char '*' *> return All) <|>
    ((csv column_reference) >>| column_references_to_columns)

let boolean_literal =
    string "true" <|> string "false" >>| fun x -> bool_of_string x

let signed_integer_string =
    optional_p (char '-') >>= fun optional_minus_sign ->
    many1_chars digit >>= fun number_part ->
    return (match optional_minus_sign with
    | None -> number_part
    | Some minus -> Core.String.of_char minus ^ number_part)

let numeric_literal =
    signed_integer_string >>= fun integral_part ->
    optional_p (char '.' *> many1_chars digit) >>= fun optional_decimal_part ->
    optional_p ((char 'e' <|> char 'E') *> signed_integer_string) >>= fun optional_sci_part ->
    let sci_part = match optional_sci_part with
    | None -> ""
    | Some sci_digits -> "E" ^ sci_digits in
    return (match optional_decimal_part with
    | None -> float_of_string integral_part
    | Some decimal_part ->
        let final_number_string = integral_part ^ "." ^ decimal_part ^ sci_part in
        float_of_string final_number_string)

let string_literal =
    skip_char '"' *> many1_chars (blank <|> alphanum) <* skip_char '"'

let equality_operator =
    choice [
        string "=" *> return (Eq Eq_equals);
        string "!=" *> return (Eq Eq_not_equals)
    ] <?> "equality operator"

let comparison_operator =
    choice [
        string ">=" *> return (Comp Comp_greater_than_equals);
        string "<=" *> return (Comp Comp_less_than_equals);
        string ">" *> return (Comp Comp_greater_than);
        string "<" *> return (Comp Comp_less_than)
    ] <?> "comparison operator"

let term_operator =
    choice [
        string "+" *> return (Term Term_plus);
        string "-" *> return (Term Term_minus);
    ] <?> "term operator"

let factor_operator =
    choice [
        string "*" *> return (Factor Factor_mult);
        string "/" *> return (Factor Factor_div);
    ] <?> "factor operator"

let unary_operator =
    string "-" *> return Unary_minus <?> "unary operator"

let right_recursive p op_p sep_p =
    p >>= fun lhs ->
    let op_rhs = optional_p (pair (sep_p *> op_p) (sep_p *> p)) in
    op_rhs >>| function
        | Some (op, rhs) -> Binary (op, lhs, rhs)
        | None -> lhs

let escaped_char =
    choice [
        char '\\' *> any_char;
        satisfy (fun c -> c <> '\\' && c <> '"')
    ]

let any_escaped_string = many escaped_char
let expression =
    fix (fun e_parser ->
        let primary =
            choice [
                string "false" *> return (Boolean_literal false);
                string "true" *> return (Boolean_literal true);
                string "nil" *> return Nil;
                (numeric_literal >>| fun n -> Numeric_literal n);
                (column_reference >>| fun c -> Reference c);
                (skip_char '"' *> any_escaped_string <* skip_char '"' >>|
                    fun char_list -> String_literal (join_chars char_list));
                skip_char '(' *> e_parser <* skip_char ')'
            ] in
        let rec unary () =
            (numeric_literal >>| fun n -> Numeric_literal n) <|>
            (unary_operator >>= fun operator ->
            unary () >>= fun p ->
            return (Unary (Unary_minus, p))) <|>
            primary in
        let factor =
            right_recursive (unary ()) factor_operator whitespace in
        let term =
            right_recursive factor term_operator whitespace in
        let comparison =
            right_recursive term comparison_operator whitespace in
        let equality =
            right_recursive comparison equality_operator whitespace in
        equality
    )

let where_parser =
    skip_string "where" *>
    whitespace *>
    expression

let query_parser =
    skip_string "select" *>
    whitespace *>
    projection_parser >>= fun projection ->
    whitespace *>
    skip_string "from" *>
    whitespace *>
    table_file_name >>= fun tn ->
    optional_p (whitespace *> where_parser) >>= fun where_clause ->
    end_of_input *>
    return (Query.Select (projection, Query.Table tn, where_clause))

let value_parser =
    choice [
        (boolean_literal >>| fun b -> Bool b);
        (numeric_literal >>| fun f -> Number f);
        (string_literal >>| fun s -> String s)
    ]

let parse_value input_string =
    parse_only value_parser (`String input_string)

let fail_to_string marks err =
    String.concat " > " marks ^ ": " ^ err

let state_to_verbose_result = function
    | Buffered.Partial _ -> Error "incomplete input"
    | Done (_, v) -> Ok v
    | Fail (unconsumed, marks, msg) ->
        let remaining_big_string = (Core.Bigstring.sub unconsumed.buffer ~pos:unconsumed.off ~len:unconsumed.len) in
        let combined_msg = fail_to_string marks msg ^ " with unconsumed: `" ^ Core.Bigstring.to_string remaining_big_string ^ "`" in
        Error combined_msg

let parse_query s =
    let initial_parser_state = Buffered.parse ~input:(`String s) query_parser in
    let final_parser_state = Buffered.feed initial_parser_state `Eof in
    state_to_verbose_result final_parser_state