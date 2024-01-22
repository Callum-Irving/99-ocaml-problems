type 'a rle = One of 'a | Many of int * 'a

let encode lst =
  let rec helper count acc lst =
    match lst with
    | x :: (y :: _ as tl) ->
        if x = y then helper (count + 1) acc tl
        else if count = 0 then helper 0 (One x :: acc) tl
        else helper 0 (Many (count + 1, x) :: acc) tl
    | x :: [] -> if count = 0 then One x :: acc else Many (count + 1, x) :: acc
    | [] -> acc
  in
  List.rev (helper 0 [] lst)
