open MParser
open Query

val query_parser : (query, unit) MParser.t

val parse_query : string -> query MParser.result