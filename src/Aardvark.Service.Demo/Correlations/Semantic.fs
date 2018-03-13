namespace CorrelationDrawing

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.UI
open UtilitiesGUI

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Semantic = 

    module Lens = 
      let _color     : Lens<CorrelationDrawing.Semantic,C4b> = Semantic.Lens.style |. Style.Lens.color |. ColorInput.Lens.c
      let _thickness : Lens<CorrelationDrawing.Semantic,float> = Semantic.Lens.style |. Style.Lens.thickness |. NumericInput.Lens.value
      let _labelText : Lens<CorrelationDrawing.Semantic,string> = Semantic.Lens.label |. TextInput.Lens.text

    [<Literal>]
    let ThicknessDefault = 1.0

    let initial id = {
        id = id

        disabled = true
        label = TextInput.init
        size = 0.0
        style = {Style.color = {c = C4b.Red};
                 Style.thickness = {Numeric.init with value = ThicknessDefault;
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
            | ChangeThickness m -> {sem with style = {sem.style with thickness = Numeric.update sem.style.thickness m}}
            | Disable -> {sem with disabled = true}
            | Enable -> {sem with disabled = false}



    let viewEnabled (s : MSemantic) =
      let thNode = Numeric.view'' 
                    NumericInputType.InputBox 
                    s.style.thickness
                    (AttributeMap.ofList [style "margin:auto; color:black; max-width:60px"])//attributes)

      let domNodeLabel =
          td [clazz "center aligned"; style lrPadding] 
             [TextInput.view' s.label] 
             |> UI.map Action.ChangeLabel
     

      alist {
          //let! labelNode = createDomNodeLabel
          yield domNodeLabel
          yield td [clazz "center aligned"; style lrPadding] [(thNode |> UI.map ChangeThickness)]
          yield td [clazz "center aligned"; style lrPadding] [ColorPicker.view s.style.color |> UI.map ColorPickerMessage]
       }
        


    let viewDisabled (s : MSemantic) = //@Thomas is this better than the version below? (performance)
      let domNodeLbl =
            td [clazz "center aligned"; style lrPadding] 
               [Incremental.label (AttributeMap.union 
                                      (AttributeMap.ofList [clazz "ui horizontal label"]) 
                                      (AttributeMap.ofAMap (incrBgColorAttr s.style.color.c)))
                                  (AList.ofList [Incremental.text (s.label.text)])
               ]

      let domNodeThickness = 
        td [clazz "center aligned"; style lrPadding] [
                          label [clazz "ui horizontal label"]
                                [Incremental.text (Mod.map(fun x -> sprintf "%.1f" x) s.style.thickness.value)]
                       ]

      let domNodeColor = td [clazz "center aligned"; style lrPadding] 
                            [Incremental.label (AttributeMap.union 
                                      (AttributeMap.ofList [clazz "ui horizontal label"])
                                      (AttributeMap.ofAMap (incrBgColorAttr s.style.color.c)))
                                 (AList.ofList [Incremental.text (Mod.map(fun (x : C4b) -> colorToHexStr x) s.style.color.c)])
                            ]
                         
      alist {
        yield domNodeLbl
        yield domNodeThickness
        yield domNodeColor
      }
//           alist {
//             let! col = s.style.color.c
//             yield td [clazz "center aligned"; style lrPadding] 
//                       [button [clazz "ui horizontal label";
//                          bgColorAttr col] [Incremental.text (s.label.text)]]
//             yield td [clazz "center aligned"; style lrPadding] [
//                          label [clazz "ui horizontal label"]
//                                [Incremental.text (Mod.map(fun x -> sprintf "%.1f" x) s.style.thickness.value)]
//                       ]
//             yield td [clazz "center aligned"; style lrPadding] [
//                            button [clazz "ui horizontal label"; bgColorAttr col] 
//                                 [Incremental.text (Mod.map(fun (x : C4b) -> colorToHexStr x) s.style.color.c)]
//                          ]
//           }



    let view (model : MSemantic) : IMod<alist<DomNode<Action>>> = //@Thomas is there a difference (Mod.map vs adaptive block)?
        Mod.map (fun d -> match d with
                            | true -> viewDisabled model
                            | false -> viewEnabled model) model.disabled

//        adaptive {
//            let! disabled = model.disabled
//            let v = 
//                match disabled with
//                    | true -> viewDisabled model
//                    | false -> viewEnabled model
//            return v
//        }