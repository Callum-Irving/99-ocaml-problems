type 'a rle = One of 'a | Many of int * 'a

let decode lst =
  (* cons x to lst n times *)
  let rec cons_n n x lst = if n = 0 then lst else cons_n (n - 1) x (x :: lst) in
  let rec helper lst acc =
    match lst with
    | [] -> acc
    | hd :: tl -> (
        match hd with
        | One x -> helper tl (x :: acc)
        | Many (n, x) -> helper tl (cons_n n x acc))
  in
  List.rev (helper lst [])
