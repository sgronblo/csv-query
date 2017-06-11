open MParser
open Query

let whitespace = blank <|> newline

let identifier_char = alphanum

let tablename = many1_chars (identifier_char <|> char '.')

let column_spec = (many1_chars identifier_char) <|> (string "*")

let csv p =
    p >>= fun parsed_head ->
    many (skip_char ',' >> whitespace >> p) >>= fun parsed_rest ->
    return (parsed_head :: parsed_rest)

let strings_to_projection names =
    if names = ["*"]
    then All
    else Columns (List.map (fun n -> Column n) names)

let query_parser =
    skip_string "select" >>
    whitespace >>
    csv column_spec >>= fun column_names ->
    whitespace >>
    skip_string "from" >>
    whitespace >>
    tablename >>= fun tn ->
    let projection = strings_to_projection column_names in
    return (Query.Select (projection, Query.Table tn))

let parse_query string =
    parse_string query_parser string ()