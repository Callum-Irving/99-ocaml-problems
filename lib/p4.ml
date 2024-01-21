let len lst =
  let rec helper lst n =
    match lst with [] -> n | _ :: rest -> helper rest (n + 1)
  in
  helper lst 0
