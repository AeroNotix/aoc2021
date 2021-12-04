let get_next_number () =
  try Some (int_of_string (read_line ()))
  with End_of_file -> None

let rec get_numbers acc =
  match get_next_number () with
  | None -> List.rev acc
  | Some(n) -> get_numbers (n :: acc)

let rec compare_numbers previous numbers increases =
  match numbers with
  | [] -> increases
  | h::tl -> if h > previous
             then compare_numbers h tl (increases + 1)
             else compare_numbers h tl increases

let () =
  let numbers = (get_numbers []) in
  Printf.printf "%d\n" (compare_numbers (List.hd numbers) (List.tl numbers) 0)
