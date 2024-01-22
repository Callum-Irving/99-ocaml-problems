type 'a node =
  | One of 'a 
  | Many of 'a node list

let flatten lon =
  let rec helper lon acc =
    match lon with
    | [] -> acc
    | One hd :: tl -> helper tl (hd :: acc)
    | Many hd :: tl -> helper tl (helper hd acc)
  in
  List.rev (helper lon [])
