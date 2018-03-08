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


