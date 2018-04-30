namespace CorrelationDrawing

open System
open Aardvark.Base.Incremental
open Aardvark.Base

module UtilitiesDatastructures =

  let alistFromAMap (input : amap<_,'a>) : alist<'a> = input |> AMap.toASet |> AList.ofASet |> AList.map snd 


  let plistFromHMap (input : hmap<_,'a>) : plist<'a> = input |> HMap.toSeq |> PList.ofSeq |> PList.map snd 

  let sortedPlistFromHmap (input : hmap<_,'a>) (projection : ('a -> 'b)) : plist<'a> =
      input 
          |> HMap.toSeq 
          |> Seq.map snd
          |> Seq.sortBy projection
          |> PList.ofSeq 


module List =
  let contains' (f : 'a -> bool) (lst : List<'a>)  =
    lst |> List.map (fun x -> f x)
        |> List.reduce (fun x y -> x || y)

  let contains'' (f : 'a -> 'b) (a : 'a)  (lst : List<'a>) =
    lst
       |> List.map f
       |> List.contains (f a)

  let reduce' (f1 : 'a -> 'b) (f2 : 'b -> 'b -> 'b) (lst : List<'a>) =
    lst
      |> List.map f1
      |> List.reduce f2

module PList =
  let mapiInt (lst : plist<'a>) =
    let i = ref 0
    seq {
      for item in lst do
        yield (item, i.Value)
        i := !i + 1
    }
    |> PList.ofSeq
      

module AList =
  let isEmpty (alst: alist<'a>) =
    alst.Content 
      |> Mod.map (fun x -> (x.Count < 1))
    
  let reduce (f : 'a -> 'a -> 'a) (alst: alist<'a>) =
    alst.Content
      |> Mod.map (fun (x : plist<'a>) -> 
                      let r =
                        AList.toList alst
                           |> List.reduce f//(fun x y -> if x < y then x else y)
                      r
                  )

  let minBy (f : 'a -> 'b) (alst : alist<'a>) =
    alst
      |> reduce (fun x y -> if (f x) < (f y) then x else y)
      
  let min (alst : alist<'a>) =
     alst |>
      reduce (fun x y -> if x < y then x else y)

  let maxBy (f : 'a -> 'b) (alst : alist<'a>)  =
    alst
      |> reduce (fun x y -> if (f x) > (f y) then x else y)
      
  let max (alst : alist<'a>) =
    alst
      |> reduce (fun x y -> if x > y then x else y)

  let average (alst : alist<float>) = //TODO make dynamic
    let lst = 
      alst
        |> AList.toList
    
    let sum =
      lst |> List.reduce (fun x y -> x + y)

    sum / (float lst.Length)

  let averageOf (f : 'a -> float) (alst : alist<'a>) = //TODO make dynamic
    let lst = 
      alst
        |> AList.toList

    let sum =
      lst |> List.map (fun x-> (f x))
          |> List.reduce (fun x y -> x + y)

    sum / (float lst.Length)

 