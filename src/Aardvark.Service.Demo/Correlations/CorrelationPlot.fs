namespace Foo



module Tmp =
                      
  let min (lst : List<'a>) =
     lst |>
      List.reduce (fun x y -> if x < y then x else y)

  let lst = [1..10]


  lst
    |> List.scan (fun x y -> x + y) lst.Head
    //|> List.length
  