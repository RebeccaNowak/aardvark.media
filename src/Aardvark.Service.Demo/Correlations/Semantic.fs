﻿namespace CorrelationDrawing

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
        label = TextInput.init
        size = 0.0
        style = {Style.color = {c = C4b.Red};
                 Style.thickness = {Numeric.init with value = 1.0;
                                                      min = 0.5; 
                                                      max = 10.0; 
                                                      step = 0.5; 
                                                      format = "{0:0.0}"}}
        geometry = GeometryType.Line
        semanticType = SemanticType.Metric
    }

    type Action = 
        | Disable
        | Enable
        | ColorPickerMessage of ColorPicker.Action
        | ChangeThickness of Numeric.Action
        | ChangeLabel of TextInput.Action

    let update (sem : Semantic) (a : Action) = 
        match a with
            | ChangeLabel m -> {sem with label = TextInput.update sem.label m}
            | ColorPickerMessage m -> 
              {sem with style = {sem.style with color = (ColorPicker.update sem.style.color m)}}
            | ChangeThickness m -> 
              {sem with style = {sem.style with thickness = Numeric.update sem.style.thickness m}} //{sem with style = {sem.style with thickness = {Numeric.init with value =  2.0}}}
            | Disable -> {sem with disabled = true}
            | Enable -> {sem with disabled = false}



    let viewEnabled (s : MSemantic) =
      let attributes = 
        amap {
          yield style "margin:auto; color:black; max-width:60px;"
        }
      let thNode = Numeric.view'' 
                    NumericInputType.InputBox 
                    s.style.thickness
                    (AttributeMap.ofAMap attributes)

      let createDomNodeLabel =
        adaptive {
           let! col = s.style.color.c
           return td 
                    [clazz "center aligned"; style lrPadding] 
                    [TextInput.view' s.label] 
                  |> UI.map Action.ChangeLabel
         }
     

      alist {
          let! labelNode = createDomNodeLabel
          yield labelNode
          yield td [clazz "center aligned"; style lrPadding] [(thNode |> UI.map ChangeThickness)]
          yield td [clazz "center aligned"; style lrPadding] [ColorPicker.view s.style.color |> UI.map ColorPickerMessage]
       }
        


    let viewDisabled (s : MSemantic) = 
           alist {
             let! col = s.style.color.c
             yield td [clazz "center aligned"; style lrPadding] 
                       [button [clazz "ui horizontal label";
                          bgColorAttr col] [Incremental.text (s.label.text)]]
             yield td [clazz "center aligned"; style lrPadding] [
                          label [clazz "ui horizontal label"]
                                [Incremental.text (Mod.map(fun x -> sprintf "%.1f" x) s.style.thickness.value)]
                       ]
             yield td [clazz "center aligned"; style lrPadding] [
                            button [clazz "ui horizontal label"; bgColorAttr col] 
                                 [Incremental.text (Mod.map(fun (x : C4b) -> colorToHexStr x) s.style.color.c)]
                          ]
           }


    let view (s : MSemantic) : IMod<alist<DomNode<Action>>> =
        adaptive {
            let! disabled = s.disabled
            let v = 
                match disabled with
                    | true -> viewDisabled s
                    | false -> viewEnabled s
            return v
        }

    module Lens = 
      let color =
         { new Lens<CorrelationDrawing.Semantic, C4b>() with
             override x.Get(r) = r.style.color.c
             override x.Set(r,v) = { r with style = {r.style with color = {r.style.color with c = v}}}
             override x.Update(r,f) = { r with style = {r.style with color = {r.style.color with c = (f r.style.color.c)}}}
         }
      
      let thickness =
         { new Lens<CorrelationDrawing.Semantic, float>() with
             override x.Get(r) = r.style.thickness.value
             override x.Set(r,v) = { r with style = {r.style with thickness = {r.style.thickness with value = v}}}
             override x.Update(r,f) = { r with style = {r.style with thickness = {r.style.thickness with value = (f r.style.thickness.value)}}}
         }