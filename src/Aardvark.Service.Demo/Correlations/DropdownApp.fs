namespace CorrelationDrawing

open System
open Aardvark.Base
open Aardvark.Base.Incremental

open Aardvark.UI


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
           (changeFunction     : (option<string> -> 'msg))
           (labelFunction      : ('a -> IMod<string>))
           (idFunction         : (option<'a> -> option<string>))
           (getIsSelected      : ('a -> IMod<bool>))
            =

    let attributes (value : 'a) (name : string) =
        let notSelected = 
          (attribute "value" (Mod.force (labelFunction value)))
          
        let selAttr = (attribute "selected" "selected")
        let attrMap = 
            AttributeMap.ofListCond [
                always (notSelected )
                onlyWhen (getIsSelected value) (selAttr)
            ]
        attrMap
       
    

    let alistAttr  = 
      amap {
          yield style "color:black"
          let! lst = (mDropdown.valueList.Content) 
          let callback (i : int) = lst
                                |> PList.tryAt(i) 
                                |> idFunction
                                |> changeFunction
           
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
                      (attributes x (Mod.force (labelFunction x))) 
                      (AList.ofList [Incremental.text (labelFunction x)]))
          yield! domNode
        }
      )




