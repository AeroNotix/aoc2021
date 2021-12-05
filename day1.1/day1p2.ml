let rec compare_windows round previous numbers increases =
  match numbers with
  | first :: second :: third :: tl ->
     let window_count = first + second + third in
     let next_numbers = (second :: third :: tl) in
     Printf.printf "Round %d is %d - previous %d - %B\n" round window_count previous (window_count > previous);
     if window_count > previous
     then compare_windows (round+1) window_count next_numbers (increases + 1)
     else compare_windows (round+1) window_count next_numbers increases
  | [] -> increases
  | _ -> increases

let () =
  Printf.printf "%d\n" (compare_windows 0 0 (Util.Io.map_stdin ~f:(fun s -> int_of_string s)) (0 -1))
