let rec nth lst n =
  match lst with
  | [] -> None
  | item :: rest -> if n = 0 then Some item else nth rest (n - 1)    