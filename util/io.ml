let apply_f_to_line ~f:f =
  try Some (f (read_line ()))
  with End_of_file -> None

let rec do_apply_f_to_lines ~f:f acc =
  match apply_f_to_line ~f:f with
  | None -> List.rev acc
  | Some(v) -> do_apply_f_to_lines ~f:f (v :: acc)

let apply_f_to_lines ~f:f = do_apply_f_to_lines ~f:f []
