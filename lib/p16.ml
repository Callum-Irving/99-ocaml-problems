let drop lst n =
  let rec helper lst n_aux acc =
    match lst with
    | hd :: tl ->
        if n_aux = 0 then helper tl (n - 1) acc
        else helper tl (n_aux - 1) (hd :: acc)
    | [] -> acc
  in
  List.rev (helper lst (n - 1) [])
