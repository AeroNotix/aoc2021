type column_totals = {
    zero : int;
    one  : int;
  }

module CountMap = Map.Make(Int)

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

let count s m =
  let (_, m) = Util.Str.fold_left update_entry (0, m) s in
  m

let () =
  let bitcounts = (Util.Io.stream_fold count Util.Io.stdin_stream CountMap.empty) in
  let (epsilon, gamma) = (CountMap.fold total_column_values bitcounts (0, 0)) in
  Printf.printf "%d\n" (epsilon * gamma)
