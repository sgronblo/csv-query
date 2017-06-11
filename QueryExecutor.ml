open Query

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

let optional_find list i =
    try
        Some (List.nth list i)
    with
        | _ -> None

let get_all_indices indices list =
   let found_values = List.map (fun i -> optional_find list i) indices in
   deoptionalize found_values

let project values = function
    | AllIndices -> values
    | ColumnIndices indices ->
        get_all_indices indices values

let get_indexed_projection header_row = function
    | All -> AllIndices
    | Columns columns ->
        ColumnIndices (List.map (fun (Column projection_col_name) -> find_index header_row projection_col_name) columns)

let execute_query = function
    | Select (projection, Table table_file) ->
        let in_channel = open_in table_file in
        let header_line = input_line in_channel in
        let header_row = Str.split (Str.regexp ",") header_line in
        let indexed_projection = get_indexed_projection header_row projection in
        print_endline (String.concat "," (project header_row indexed_projection));
        try
          while true do
            let line = input_line in_channel in
            let values = Str.split (Str.regexp ",") line in
            print_endline (String.concat "," (project values indexed_projection))
          done
        with End_of_file ->
          close_in in_channel