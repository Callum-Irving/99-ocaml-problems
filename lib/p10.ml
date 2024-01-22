(* run-length encoding *)

let encode lst =
  let rec helper current acc lst =
    match lst with
    | x :: rest ->
        let count, data = current in
          if x = data then helper (count + 1, data) acc rest
          else helper (1, x) (current :: acc) rest
    | [] -> current :: acc
  in
  match lst with
  | [] -> []
  | x :: rest -> List.rev (helper (1, x) [] rest)
