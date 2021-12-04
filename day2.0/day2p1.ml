type position =
  { x : int; y : int}

type command =
  | Forward of int
  | Down of int
  | Up of int
  | Unknown

let command_of_string s =
  let tokens = String.split_on_char ' ' s in
  match tokens with
  | ["forward" ; amount ] ->
     Forward(int_of_string amount)
  | ["down" ; amount ] ->
     Down(int_of_string amount)
  | ["up" ; amount ] ->
     Up(int_of_string amount)
  | _ -> Unknown

let read_command () =
  try Some (command_of_string (read_line ()))
  with End_of_file -> None

let update_position current command =
  match command with
  | Forward(n) -> { x = current.x + n; y = current.y }
  | Down(n) -> { x = current.x; y = current.y + n}
  | Up(n) -> { x = current.x; y = current.y - n}
  | Unknown -> current

let () =
  let commands = (Util.Io.apply_f_to_lines ~f:command_of_string) in
  let final_position = List.fold_left update_position { x=0; y=0 } commands in
  Printf.printf "%d\n" (final_position.x * final_position.y)
