let rec last list =
  match list with
  | [] -> None
  | item :: [] -> Some item
  | _ :: rest -> last rest