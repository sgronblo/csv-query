open Angstrom
open Query

val query_parser : query Angstrom.t

val parse_query : string -> (query, string) result

val parse_value : string -> (value, string) result