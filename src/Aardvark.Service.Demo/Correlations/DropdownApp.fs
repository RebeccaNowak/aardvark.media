namespace CorrelationDrawing

open System
open Aardvark.Base
open Aardvark.Base.Incremental

open Aardvark.UI

//open CorrelationDrawing.CorrelationUtilities

//
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module DropdownList =
  type Action =
    | Change

  let init<'a> : DropdownList<'a> = {
    valueList = plist.Empty
    selected = None
    color = C4b.VRVisGreen
  }

  let update (model : DropdownList<'a>) (action : Action) =
    match action with
      | Change -> model

  let view (mDropdown : MDropdownList<'a, _>) 
           (changeFunction     : (Option<'a> -> Action))
           (labelFunction      : ('a -> string))
           (getIsSelected      : ('a -> IMod<bool>))
           : DomNode<Action> =

    let attributes (value : 'a) (name : string) =
        let notSelected = (attribute "value" (labelFunction value))
        let selAttr = (attribute "selected" "selected")
        let attrMap = 
            AttributeMap.ofListCond [
                always (notSelected)
                onlyWhen (getIsSelected value) (selAttr)
            ]
        //let debug = Mod.force attrMap.Content
        attrMap
       
    //let test = mDropdown.
    let rOnChange = 
        let cb (i : int) =
            let currentState = mDropdown  // |> Mod.force
            let selectedElem = PList.tryAt (i)
            changeFunction (selectedElem (Mod.force mDropdown.valueList.Content)) //////////////////
                
        onEvent "onchange" ["event.target.selectedIndex"] 
            (fun x -> 
                x 
                    |> List.head 
                    |> Int32.Parse 
                    |> cb)
    

    Incremental.select (AttributeMap.ofList [rOnChange ; style "color:black"]) 
        (
            alist {
                //let debug = mDropdown.values;
                let domNode = 
                    //mDropdown.valueList.Content
                    mDropdown.valueList
                        |> AList.mapi(fun i x ->
                             Incremental.option 
                                (attributes x (labelFunction x)) 
                                (AList.ofList [text (labelFunction x)]))
                yield! domNode
            }
        )




