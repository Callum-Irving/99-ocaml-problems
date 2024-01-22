let pack lst =
  let rec helper current acc lst =
    match lst with
    | x :: y :: rest when x = y ->
        helper (y :: current) acc (x :: rest)
    | x :: rest ->
        helper [] ((x :: current) :: acc) rest
    | [] -> acc
  in
  List.rev (helper [] [] lst)
