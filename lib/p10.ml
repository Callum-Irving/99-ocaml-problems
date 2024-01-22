let encode lst =
  let rec helper count acc lst =
    match lst with
    | x :: (y :: _ as tl) ->
        if x = y then helper (count + 1) acc tl
        else helper 0 ((count + 1, x) :: acc) tl
    | x :: [] -> (count + 1, x) :: acc
    | [] -> acc
  in
  List.rev (helper 0 [] lst)
