namespace Foo



module Tmp =
                      
                  
  

  let min (lst : List<'a>) =
     lst |>
      List.reduce (fun x y -> if x < y then x else y)

  