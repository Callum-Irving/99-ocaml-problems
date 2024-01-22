type 'a rle = One of 'a | Many of int * 'a

let encode lst =
  let make_rle count item =
    if count = 0 then One item else Many (count + 1, item)
  in
  let rec helper count acc lst =
    match lst with
    | x :: (y :: _ as tl) ->
        if x = y then helper (count + 1) acc tl
        else helper 0 (make_rle count x :: acc) tl
    | x :: [] -> if count = 0 then One x :: acc else Many (count + 1, x) :: acc
    | [] -> acc
  in
  List.rev (helper 0 [] lst)
