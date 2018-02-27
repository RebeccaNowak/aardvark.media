namespace CorrelationDrawing

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.UI


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Semantic = 
    let initial id = {
        id = id

        label = "Semantic1" 
        size = 0.0
        style = {Style.color = {c = C4b.Red}; Style.thickness = {Numeric.init with value = 1.0}}
        geometry = GeometryType.Line
        semanticType = SemanticType.Metric
    }

    type Action = 
        | ChangeLabel
        | ColorPickerMessage of ColorPicker.Action
        | ChangeThickness

    let update (sem : Semantic) (a : Action) = 
        match a with
            | ChangeLabel -> {sem with label = "OtherLabel"}
            | ColorPickerMessage m -> {sem with style = {sem.style with color = (ColorPicker.update sem.style.color m)}}
            | ChangeThickness -> {sem with style = {sem.style with thickness = {Numeric.init with value =  2.0}}}


    let viewEnabled (s : MSemantic) =
        //require Html.semui (         
        div [clazz "ui"][
            button [clazz "ui button"; onMouseClick (fun _ -> ChangeLabel)] [Incremental.text (s.label)]
            ColorPicker.view s.style.color |> UI.map ColorPickerMessage
            button [clazz "ui button"; onMouseClick (fun _ -> ChangeThickness)] [text "Thickness"]
        ]
       // )

    let viewDisabled (s : MSemantic) = 
        div [clazz "ui"][
            button [clazz "ui disabled button"; onMouseClick (fun _ -> ChangeLabel)] [Incremental.text (s.label)]
            label [clazz "ui horizontal label" ][text "col"]
            button [clazz "ui disabled button"; onMouseClick (fun _ -> ChangeThickness)] [text "Thickness"]
        ]

    module Lens = 
       let style =
            { new Lens<CorrelationDrawing.Semantic, C4b>() with
                override x.Get(r) = r.style.color.c
                override x.Set(r,v) = { r with style = {r.style with color = {r.style.color with c = v}}}
                override x.Update(r,f) = { r with style = {r.style with color = {r.style.color with c = (f r.style.color.c)}}}
            }