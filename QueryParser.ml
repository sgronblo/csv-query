open Angstrom
open Query

let whitespace = satisfy (function
    | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
    | _ -> false)

let alphanum = satisfy (function
    | 'a'..'z' | 'A'..'Z' | '0'..'9' -> true
    | _ -> false)

let skip_string s =
    string s *> return ()

let digit = satisfy (function
    | '0'..'9' -> true
    | _ -> false)

let pair = lift2 (fun x y -> (x, y))

let blank =
  satisfy (function '\t' | ' ' -> true | _ -> false)

let optional_p p =
    let to_some x = Some x in
    option None (to_some <$> p)

let skip_char c = skip (fun parsed_char -> c = parsed_char)

let identifier_char = alphanum <|> char '_'

let join_chars cl = String.concat "" (List.map (String.make 1) cl)

let many1_chars char_parser =
    join_chars <$> many1 char_parser

let table_file_name = many1_chars (identifier_char <|> char '.')

let table_name = many1_chars identifier_char

let column_name = many1_chars identifier_char

let column_reference =
    optional_p (table_name <* skip_char '.') >>= fun maybe_table_name ->
    column_name >>= fun colname ->
    return (maybe_table_name, colname)

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

let numeric_literal =
    many1_chars digit >>| float_of_string

let string_literal =
    skip_char '"' *> many1_chars (blank <|> alphanum) <* skip_char '"'

let equality_operator =
    choice [
        string "=" *> return (Eq Eq_equals);
        string "!=" *> return (Eq Eq_not_equals)
    ]

let comparison_operator =
    choice [
        string ">" *> return (Comp Comp_greater_than);
        string ">=" *> return (Comp Comp_greater_than_equals);
        string "<=" *> return (Comp Comp_less_than_equals);
        string "<" *> return (Comp Comp_less_than)
    ]

let term_operator =
    choice [
        string "+" *> return (Term Term_plus);
        string "-" *> return (Term Term_minus);
    ]

let factor_operator =
    choice [
        string "*" *> return (Factor Factor_mult);
        string "/" *> return (Factor Factor_div);
    ]

let unary_operator =
    string "-" *> return Unary_minus

let right_recursive p op_p sep_p =
    p >>= fun lhs ->
    let op_rhs = optional_p (pair (sep_p *> op_p) (sep_p *> p)) in
    op_rhs >>| function
        | Some (op, rhs) -> Binary (op, lhs, rhs)
        | None -> lhs

(* let rec lazy_attempt_choice = function
    | [] -> fail "Could not match any parsers"
    | next_p :: remaining_p ->
        let actual_p = Lazy.force(next_p) in
        actual_p <|> (lazy_attempt_choice remaining_p) *)

let expression =
    fix (fun e_parser ->
        let primary =
            choice [
                string "false" *> return (Boolean_literal false);
                string "true" *> return (Boolean_literal true);
                string "nil" *> return Nil;
                (many1_chars digit >>| fun n -> Numeric_literal (float_of_string n));
                (column_reference >>| fun c -> Reference c);
                skip_char '(' *> e_parser <* skip_char ')'
            ] in
        let rec unary () =
            (unary_operator >>= fun operator ->
            whitespace *>
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

let parse_query s =
    parse_only query_parser (`String s)