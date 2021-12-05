let apply_f_to_line ~f:f =
  try Some (f (read_line ()))
  with End_of_file -> None

let rec do_map_stdin ~f:f acc =
  match apply_f_to_line ~f:f with
  | None -> List.rev acc
  | Some(v) -> do_map_stdin ~f:f (v :: acc)

let map_stdin ~f:f = do_map_stdin ~f:f []

let stdin_stream (_:int): string option =
  try Some (read_line ())
  with End_of_file -> None

let stream_fold f stream init =
  let result = ref init in
  Stream.iter
    (fun x -> result := f x !result)
    (Stream.from stream);
  !result
