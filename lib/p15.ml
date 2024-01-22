let replicate lst n =
  let rec cons_n n x lst = if n = 0 then lst else cons_n (n - 1) x (x :: lst) in
  let rec helper lst acc =
    match lst with hd :: tl -> helper tl (cons_n n hd acc) | [] -> acc
  in
  List.rev (helper lst [])
