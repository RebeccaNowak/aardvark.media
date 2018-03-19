module Border

//let li = [5.0;13.0;9.0;1.0;24.0;5.0;53.0;57.0]

let li = [5.0;13.0;9.0;1.0;24.0;5.0;5.0;8.0]
let sum = li |> List.fold (fun acc elem -> acc + elem) 0.0
let mean = sum / 8.0
              
let li2 = List.map (fun x -> x - mean) li
let liSquared = List.map (fun x -> x**2.0) li2
let sumSquared = 
          li2 
             |> List.fold (fun acc elem -> acc + elem**2.0) 0.0
let std = sqrt (sumSquared / 7.0)




