type file_name = string

type column_name = string

(*type row = 'a 'b map*)

type table =
    Table of file_name

type column =
    Column of column_name

type projection =
    | All
    | Columns of column list

type query =
    Select of (projection * table)

(*
example query: select e.name, d.name from employees.csv as e join departments.csv as d where d.name = accounting
*)

let print_table = function
    | Table table_name -> "(Table " ^ table_name ^ ")"

let print_column = function
    | Column column_name -> "column_name"

let print_projection = function
    | All -> "All"
    | Columns columns ->
        let column_names = List.map print_column columns in
        "[" ^ String.concat ", " column_names ^ "]"

let print_query = function
    | Select (projection, table) ->
        let projection_string = print_projection projection in
        let table_string = print_table table in
        "Select (" ^ projection_string ^ ", " ^ table_string ^ ")"