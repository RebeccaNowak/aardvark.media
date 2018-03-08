namespace CorrelationDrawing

open System
open Aardvark.Base
open Aardvark.Base.Incremental

open Aardvark.UI

//open CorrelationDrawing.CorrelationUtilities

//
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module DropdownList =
  type Action<'a> =
    | SetSelected of option<string>
    | SetColor of C4b
    | SetList of plist<'a>

  let init<'a> : DropdownList<'a> = {
    valueList = plist.Empty
    selected = None
    color = C4b.VRVisGreen
  }

  let update (model : DropdownList<'a>) (action : Action<'a>) =
    match action with
      | SetSelected str -> {model with selected = str}
      | SetColor col -> {model with color = col}
      | SetList lst -> {model with valueList = lst}

  
  let view (mDropdown : MDropdownList<'a, _>) 
          // (changeFunction     : (string -> Action))
           (labelFunction      : ('a -> string))
           (idFunction         : (option<'a> -> option<string>))
           (getIsSelected      : ('a -> IMod<bool>))
            =

    let attributes (value : 'a) (name : string) =
        let notSelected = (attribute "value" (labelFunction value))
        let selAttr = (attribute "selected" "selected")
        let attrMap = 
            AttributeMap.ofListCond [
                always (notSelected)
                onlyWhen (getIsSelected value) (selAttr)
            ]
        attrMap
       


    let alistAttr  = 
      amap {
          yield style "color:black"
          let! lst = (mDropdown.valueList.Content)
          let aopt (i : int) = (PList.tryAt(i) lst)
          let setSel (i : int) = (idFunction (aopt i) |> SetSelected) 
          let callback (i : int) = setSel i//changeFunction (PList.tryAt(i) lst)
          yield (onEvent "onchange" 
                                ["event.target.selectedIndex"] 
                                (fun x -> 
                                    x 
                                        |> List.head 
                                        |> Int32.Parse 
                                        |> callback)) 
      }

    Incremental.select (AttributeMap.ofAMap alistAttr)                        
      (
        alist {
            let domNode = 
                mDropdown.valueList
                    |> AList.mapi(fun i x ->
                         Incremental.option 
                           (attributes x (labelFunction x)) 
                           (AList.ofList [text (labelFunction x)]))
            yield! domNode
        }
      )




