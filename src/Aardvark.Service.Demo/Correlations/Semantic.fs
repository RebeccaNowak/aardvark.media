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
      let _color     : Lens<CorrelationDrawing.Semantic,C4b>    = Semantic.Lens.style |. Style.Lens.color |. ColorInput.Lens.c
      let _thickness : Lens<CorrelationDrawing.Semantic,float>  = Semantic.Lens.style |. Style.Lens.thickness |. NumericInput.Lens.value
      let _labelText : Lens<CorrelationDrawing.Semantic,string> = Semantic.Lens.label |. TextInput.Lens.text

    [<Literal>]
    let ThicknessDefault = 1.0

    let initial id = {

        id            = id
        timestamp     = Time.getTimestamp
        disabled      = true
        label         = TextInput.init
        size          = 0.0
        style         = 
          { Style.color     = { c = C4b.Red }
            Style.thickness = 
              {
                Numeric.init with 
                  value  = ThicknessDefault
                  min    = 0.5
                  max    = 10.0
                  step   = 0.5 
                  format = "{0:0.0}"
              }
          }
        geometry      = GeometryType.Line
        semanticType  = SemanticType.Metric
        level         = 0
    }

    let initialHorizon0 id = {
      initial id with 
        label         = {TextInput.init with text = "Horizon0"}
        style         = {Style.color      = {c = C4b.Red};
                         Style.thickness  = {Numeric.init with value = 5.0}}
        geometry      = GeometryType.Line
        semanticType  = SemanticType.Metric
        level         = 0
      }

    let initialHorizon1 id = {
      initial id with 
        label         = {TextInput.init with text = "Horizon1"}
        style         = {Style.color      = {c = C4b.DarkRed};
                         Style.thickness  = {Numeric.init with value = 5.0}}
        geometry      = GeometryType.Polygon
        semanticType  = SemanticType.Metric
        level         = 0
      }

    let initialHorizon2 id = {
      initial id with 
        label         = {TextInput.init with text = "Horizon2"}
        style         = {Style.color      = {c = C4b.DarkMagenta};
                         Style.thickness  = {Numeric.init with value = 5.0}}
        geometry      = GeometryType.Line
        semanticType  = SemanticType.Hierarchical
        level         = 0
      }

    let initialHorizon3 id = {
      initial id with 
        label         = {TextInput.init with text = "Horizon3"}
        style         = {Style.color      = {c = C4b.Magenta};
                         Style.thickness  = {Numeric.init with value = 5.0}}
        geometry      = GeometryType.Line
        semanticType  = SemanticType.Metric
      }

    let initialHorizon4 id = {
      initial id with 
        label         = {TextInput.init with text = "Horizon4"}
        style         = {Style.color      = {c = C4b.DarkBlue};
                         Style.thickness  = {Numeric.init with value = 5.0}}
        geometry      = GeometryType.Line
        semanticType  = SemanticType.Metric
      }


    let initialGrainSize id = {
      initial id with 
        label         = {TextInput.init with text = "Grainsize"}
        style         = {Style.color      = {c = C4b.Gray};
                         Style.thickness  = {Numeric.init with value = 1.0}}
        geometry      = GeometryType.Line
        semanticType  = SemanticType.Metric
        level         = 2
      }

    let initialCrossbed id = {
      initial id with 
        label         = {TextInput.init with text = "Crossbed"}
        style         = {Style.color      = {c = C4b.Blue};
                         Style.thickness  = {Numeric.init with value = 1.0}}
        geometry      = GeometryType.Line
        semanticType  = SemanticType.Metric
        level         = 3
      }

    let impactBreccia id = {
      initial id with 
        label         = {TextInput.init with text = "Impact Breccia"}
        style         = {Style.color      = {c = C4b.Black};
                         Style.thickness  = {Numeric.init with value = 1.0}}
        geometry      = GeometryType.Point
        semanticType  = SemanticType.Angular
        level         = 3
      }
    

    type Action = 
        | Disable
        | Enable
        | ColorPickerMessage  of ColorPicker.Action
        | ChangeThickness     of Numeric.Action
        | ChangeLabel         of TextInput.Action

    let update (sem : Semantic) (a : Action) = 
        match a with
            | ChangeLabel m -> 
                {sem with label = TextInput.update sem.label m}
            | ColorPickerMessage m -> 
                {sem with style = {sem.style with color = (ColorPicker.update sem.style.color m)}}
            | ChangeThickness m -> 
                {sem with style = {sem.style with thickness = Numeric.update sem.style.thickness m}}
            | Disable -> 
                {sem with disabled = true}
            | Enable -> 
                {sem with disabled = false}



    let viewEnabled (s : MSemantic) =
      let thNode = Numeric.view'' 
                    NumericInputType.InputBox 
                    s.style.thickness
                    (AttributeMap.ofList [style "margin:auto; color:black; max-width:60px"])

      let domNodeLabel =
        td [clazz "center aligned"; style lrPadding] 
            [TextInput.view' s.label] 
            |> UI.map Action.ChangeLabel
     

      [
        domNodeLabel;
        td [clazz "center aligned"; style lrPadding] 
           [(thNode |> UI.map ChangeThickness)];
        td [clazz "center aligned"; style lrPadding] 
           [ColorPicker.view s.style.color |> UI.map ColorPickerMessage]
      ]
       
      //          [div [clazz "item"] 
//              [div [clazz "ui right labeled input"] [
//                      label [clazz "ui label"] [text "Geometry"]  // style "color:white"
//                      Html.SemUi.dropDown model.geometry SetGeometry]];  


    let viewDisabled (s : MSemantic) = 
      let domNodeLbl =
        td [clazz "center aligned"; style lrPadding] [
          Incremental.label 
            (AttributeMap.union 
               (AttributeMap.ofList [clazz "ui horizontal label"]) 
               (AttributeMap.ofAMap (incrBgColorAttr s.style.color.c)))
            (AList.ofList [Incremental.text (s.label.text)])
        ]

      let domNodeThickness = 
        td [clazz "center aligned"; style lrPadding] [
          label [clazz "ui horizontal label"] [
            Incremental.text (Mod.map(fun x -> sprintf "%.1f" x) s.style.thickness.value)
          ]
        ]

      let domNodeColor = 
        td  [clazz "center aligned"; style lrPadding] 
            [Incremental.label (AttributeMap.union 
                      (AttributeMap.ofList [clazz "ui horizontal label"])
                      (AttributeMap.ofAMap (incrBgColorAttr s.style.color.c)))
                  (AList.ofList [Incremental.text (Mod.map(fun (x : C4b) -> colorToHexStr x) s.style.color.c)])
            ]

      let domNodeLevel =  
        td  [clazz "center aligned"; style lrPadding] 
            [label [clazz "ui horizontal label"]
                   [Incremental.text (Mod.map(fun x -> sprintf "%i" x) s.level)]
            ]

      let domNodeGeometryType =  
        td [clazz "center aligned"; style lrPadding] 
           [label [clazz "ui horizontal label"]
                  [Incremental.text (Mod.map(fun x -> x.ToString()) s.geometry)]
           ]

      let domNodeSemanticType =  
        td [clazz "center aligned"; style lrPadding] 
           [label [clazz "ui horizontal label"]
                  [Incremental.text (Mod.map(fun x -> x.ToString()) s.semanticType)]
           ]
                         
      [domNodeLbl;
       domNodeThickness;
       domNodeColor;
       domNodeLevel;
       domNodeGeometryType;
       domNodeSemanticType]
      


    let view (model : MSemantic) : IMod<list<DomNode<Action>>> =
        Mod.map (fun d -> 
          match d with
            | true  -> viewDisabled model
            | false -> viewEnabled model) model.disabled
