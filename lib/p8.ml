let rec compress lst =
  match lst with
  | [] -> []
  | x :: y :: rest ->
      if x = y then compress (y :: rest) else x :: compress (y :: rest)
  | x :: rest -> x :: compress rest
