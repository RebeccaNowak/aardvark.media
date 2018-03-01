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
        style = {Style.color = {c = C4b.Red}; Style.thickness = {Numeric.init with value = 1.0}}
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
      let thNode = Numeric.view s.style.thickness
          
      //require Html.semui (         
      Incremental.div
        (AttributeMap.ofList [clazz "ui small horizontal list"; style tinyPadding]) (
          alist {
            yield div [clazz "item";style noPadding] [ColorPicker.view s.style.color |> UI.map ColorPickerMessage]
            let! col = s.style.color.c
            yield div [clazz "item";style noPadding] [button [clazz "ui button";
                                              onMouseClick (fun _ -> ChangeLabel);
                                              bgColorAttr col] [Incremental.text (s.label)]]
            yield div [clazz "item";style noPadding] [thNode |> UI.map ChangeThickness]
              //yield button [clazz "ui button"; onMouseClick (fun _ -> ChangeThickness)] [text "Thickness"]
          }
        )


    let viewDisabled (s : MSemantic) = 
        Incremental.div
           (AttributeMap.ofList [clazz "ui small horizontal list"; style tinyPadding]) (
                alist {
                    let! col = s.style.color.c
                    //let bgAttr = style (sprintf "background: %s" (CorrelationUtilities.colorToHexStr col))
                    yield div [clazz "item";
                        style noPadding] [
                            button [
                                clazz "ui disabled button"; 
                                onMouseClick (fun _ -> ChangeLabel); 
                                bgColorAttr col] [Incremental.text (s.label)]]
                    yield div [clazz "item";
                               style noPadding] [
                                   label [clazz "ui horizontal label" ]
                        [Incremental.text (Mod.map(fun (x : bool)  -> x.ToString()) s.disabled)]]
//                    yield button [
//                        clazz "ui disabled button"; 
//                        onMouseClick (fun _ -> ChangeThickness)] [text "Thickness"]
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