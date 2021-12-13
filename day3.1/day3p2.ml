type column_totals = {
    zero : int;
    one  : int;
  }

module CountMap = Map.Make(Int)

exception Invalid_input

let print_column_totals k v =
  Printf.printf "%d - %d/%d\n" k v.zero v.one

let total_column_values _ v (epsilon, gamma) =
  let (most_common, least_common) = if v.zero > v.one
                                    then (0, 1)
                                    else (1, 0) in
  (
    Int.logor (Int.shift_left epsilon 1) most_common,
    Int.logor (Int.shift_left gamma 1) least_common
  )

let update_with_default k f m =
  CountMap.update k
    (fun(entry) ->
      Some(match entry with
           | Some(v) -> f v
           | None -> f { zero = 0; one = 0})) m

let update_entry (c, m) s =
  (c + 1, if s == '0'
          then update_with_default c (fun(entry) -> { entry with zero = entry.zero + 1}) m
          else update_with_default c (fun(entry) -> { entry with one = entry.one + 1}) m)

let count m s =
  let (_, m) = Util.Str.fold_left update_entry (0, m) s in
  m

let reading_matches idx looking_for reading =
  String.get reading idx <> looking_for

let most_common idx counts =
  let entry = CountMap.find idx counts in
  if entry.one >= entry.zero
  then '0'
  else '1'

let least_common idx counts =
  let entry = CountMap.find idx counts in
  if entry.zero <= entry.one
  then '1'
  else '0'

let determine_reading comparitor counts readings =
  let rec aux counts readings idx =
    match readings with
    | [ number ] -> number
    | _ :: _ ->
       let looking_for = comparitor idx counts in
       let filtered_readings = List.filter (reading_matches idx looking_for) readings in
       let counts = List.fold_left count CountMap.empty filtered_readings in
       aux counts filtered_readings (idx + 1)
    | [] -> raise Invalid_input in
  aux counts readings 0

let int_of_binary_string s =
  int_of_string ("0b" ^ s)

let () =
  let readings = Util.Io.map_stdin ~f:Util.Fn.identity in
  let counts = List.fold_left count CountMap.empty readings in
  let o2_reading = determine_reading most_common counts readings in
  let co2_reading = determine_reading least_common counts readings in
  Printf.printf "%d\n" ((int_of_binary_string o2_reading) * (int_of_binary_string co2_reading))
