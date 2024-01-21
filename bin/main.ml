let () = print_endline "Hello, World!"
let () = 
  let item = Problems.P1.last ["one";"two";"three"] in
    match item with
    | Some str -> print_endline str
    | None -> print_endline "empty!"
