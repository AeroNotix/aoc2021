module CharMap = Map.Make(Char)

let charmap_of_list l =
  l |> List.to_seq |> CharMap.of_seq

let score_map = [
  (')', 3);
  (']', 57);
  ('}', 1197);
  ('>', 25137)
] |> charmap_of_list

let opening = [ '(' ; '{' ; '[' ; '<' ]
let closing = [ ')' ; '}' ; ']' ; '>' ]

let bracket_mappings = (List.append
                           (List.combine opening closing)
                           (List.combine closing opening)) |> charmap_of_list

let rec check_syntax unclosed remainder =
  match remainder with
  | [] -> 0
  | h::t ->
    if List.mem h closing then
      if CharMap.find h bracket_mappings = List.hd unclosed
      then check_syntax (List.tl unclosed) t
      else CharMap.find h score_map
    else
      check_syntax (h :: unclosed) t


let parse_line line score =
  let charlist = Util.Str.explode line in
  score + (check_syntax [List.hd charlist] (List.tl charlist))

let () =
  Printf.printf "%d\n" (Util.Io.stream_fold parse_line Util.Io.stdin_stream 0)
