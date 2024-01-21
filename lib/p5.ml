let rev lst =
  let rec helper lst acc =
    match lst with [] -> acc | x :: rest -> helper rest (x :: acc)
  in
  helper lst []
