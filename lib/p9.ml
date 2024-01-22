let pack lst =
  let rec helper x lst acc =
    match lst with
    | a :: rest when a = x -> helper x rest (a :: acc)
    | _ -> lst, acc
  in
  let rec aux lst acc =
    match lst with
    | hd :: tl ->
      let rest, packed = helper hd tl [hd] in
        aux rest (packed :: acc)
    | [] -> acc
  in
  List.rev (aux lst [])
