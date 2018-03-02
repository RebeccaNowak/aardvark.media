namespace CorrelationDrawing

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.UI
open CorrelationUtilities


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Semantic = 
    let initial id = {
        id = id

        disabled = true
        label = "Semantic1" 
        size = 0.0
        style = {Style.color = {c = C4b.Red}; Style.thickness = {Numeric.init with value = 1.0; min = 0.5; max = 10.0; step = 0.5; format = "{0:0.0}"}}
        geometry = GeometryType.Line
        semanticType = SemanticType.Metric
    }

    type Action = 
        | Disable
        | Enable
        | ChangeLabel
        | ColorPickerMessage of ColorPicker.Action
        | ChangeThickness of Numeric.Action

    let update (sem : Semantic) (a : Action) = 
        match a with
            | ChangeLabel -> {sem with label = "OtherLabel"}
            | ColorPickerMessage m -> {sem with style = {sem.style with color = (ColorPicker.update sem.style.color m)}}
            | ChangeThickness m -> {sem with style = {sem.style with thickness = Numeric.update sem.style.thickness m}} //{sem with style = {sem.style with thickness = {Numeric.init with value =  2.0}}}
            | Disable -> {sem with disabled = true}
            | Enable -> {sem with disabled = false}



    let viewEnabled (s : MSemantic) =
      let thNode = Numeric.view' [NumericInputType.InputBox] s.style.thickness
      let createDomNodeLabel =
        adaptive {
          let! col = s.style.color.c
          return div [clazz "column"] [button [clazz "ui button"; style "margin: auto";
                                              onMouseClick (fun _ -> ChangeLabel);
                                              bgColorAttr col] [Incremental.text (s.label)]]
        }
      //require Html.semui (         
      Incremental.div
        (AttributeMap.ofList [clazz "ui row"; style tinyPadding]) (
          alist {
            let! labelNode = createDomNodeLabel
            yield labelNode
            yield div [clazz "column"] [thNode |> UI.map ChangeThickness]
            yield div [clazz "column"] [ColorPicker.view s.style.color |> UI.map ColorPickerMessage]
            
//            let! col = s.style.color.c
//            yield div [clazz "column";style noPadding] [button [clazz "ui button";
//                                              onMouseClick (fun _ -> ChangeLabel);
//                                              bgColorAttr col] [Incremental.text (s.label)]]
            
              //yield button [clazz "ui button"; onMouseClick (fun _ -> ChangeThickness)] [text "Thickness"]
          }
        )


    let viewDisabled (s : MSemantic) = 
      Incremental.div
        (AttributeMap.ofList [clazz "ui row"; style tinyPadding]) (
           alist {
             let! col = s.style.color.c
             yield div [clazz "column"] [
                     button [clazz "ui disabled button"; style "margin: auto";
                      bgColorAttr col] [Incremental.text (s.label)]]
             yield div [clazz "column"; style ""] [label [clazz "ui horizontal label"; style "margin: auto" ]
                 [Incremental.text (Mod.map(fun x -> sprintf "%.1f" x) s.style.thickness.value)]]
                  //;text "test"]
             yield div [clazz "column"] [
                     button [clazz "ui horizontal label"; style "margin: auto";
                      bgColorAttr col] [Incremental.text (Mod.map(fun (x : C4b) -> colorToHexStr x) s.style.color.c)]]

//             yield button [
//                 clazz "ui disabled button"; 
//                 onMouseClick (fun _ -> ChangeThickness)] [text "Thickness"]
           }
         )

    let view (s : MSemantic) : IMod<DomNode<Action>> =
        adaptive {
            let! disabled = s.disabled
            let v = 
                match disabled with
                    | true -> viewDisabled s
                    | false -> viewEnabled s
            return v
        }

    module Lens = 
       let style =
            { new Lens<CorrelationDrawing.Semantic, C4b>() with
                override x.Get(r) = r.style.color.c
                override x.Set(r,v) = { r with style = {r.style with color = {r.style.color with c = v}}}
                override x.Update(r,f) = { r with style = {r.style with color = {r.style.color with c = (f r.style.color.c)}}}
            }