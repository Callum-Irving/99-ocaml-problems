let rec last lst =
  match lst with
  | [] -> None
  | item :: [] -> Some item
  | _ :: rest -> last rest