let rec last_two list =
  match list with
  | [] | [ _ ] -> None
  | x :: y :: [] -> Some (x, y)
  | _ :: rest -> last_two rest