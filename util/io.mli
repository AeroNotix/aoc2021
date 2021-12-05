val map_stdin : f:(string -> 'a) -> 'a list

val stdin_stream : int -> string option

val stream_fold : ('a -> 'b -> 'b) -> (int -> 'a option) -> 'b -> 'b
