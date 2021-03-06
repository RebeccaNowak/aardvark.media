﻿namespace CorrelationDrawing

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

    let levels = [0;1;2;3;4;5;6;7;8]

    let initial id = {

        id            = id
        timestamp     = Time.getTimestamp
        state         = SemanticState.New
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
        semanticType  = SemanticType.Metric
        level         = 0
    }

    /////// DEFAULT SEMANTICS
    let initialHorizon0 id = {
      initial id with 
        label         = {TextInput.init with text = "Horizon0"}
        style         = {Style.color      = {c = new C4b(37,52,148)}
                         Style.thickness  = {Numeric.init with value = 6.0}}
        semanticType  = SemanticType.Hierarchical
        level         = 0
      }

    let initialHorizon1 id = {
      initial id with 
        label         = {TextInput.init with text = "Horizon1"}
        style         = {Style.color      = {c = new C4b(44,127,184)}
                         Style.thickness  = {Numeric.init with value = 5.0}}
        semanticType  = SemanticType.Hierarchical
        level         = 1
      }

    let initialHorizon2 id = {
      initial id with 
        label         = {TextInput.init with text = "Horizon2"}
        style         = {Style.color      = {c = new C4b(65,182,196)}
                         Style.thickness  = {Numeric.init with value = 4.0}}
        semanticType  = SemanticType.Hierarchical
        level         = 2
      }

    let initialHorizon3 id = {
      initial id with 
        label         = {TextInput.init with text = "Horizon3"}
        style         = {Style.color      = {c = new C4b(127,205,187)}
                         Style.thickness  = {Numeric.init with value = 3.0}}
        semanticType  = SemanticType.Hierarchical
        level         = 3
      }

    let initialHorizon4 id = {
      initial id with 
        label         = {TextInput.init with text = "Horizon4"}
        style         = {Style.color      = {c = new C4b(199,233,180)}
                         Style.thickness  = {Numeric.init with value = 2.0}}
        semanticType  = SemanticType.Hierarchical
        level         = 4
      }


    let initialGrainSize id = {
      initial id with 
        label         = {TextInput.init with text = "Grainsize"}
        style         = {Style.color      = {c = new C4b(252,141,98)}
                         Style.thickness  = {Numeric.init with value = 1.0}}
        semanticType  = SemanticType.Metric
        level         = -1
      }

    let initialCrossbed id = {
      initial id with 
        label         = {TextInput.init with text = "Crossbed"}
        style         = {Style.color      = {c = new C4b(231,138,195)}
                         Style.thickness  = {Numeric.init with value = 1.0}}
        semanticType  = SemanticType.Angular
        level         = -1
      }

    let impactBreccia id = {
      initial id with 
        label         = {TextInput.init with text = "Impact Breccia"}
        style         = {Style.color      = {c = new C4b(166,216,84)}
                         Style.thickness  = {Numeric.init with value = 1.0}}
        semanticType  = SemanticType.Angular
        level         = -1
      }
    
    ////// ACTIONS
    type Action = 
        | SetState            of SemanticState
        | ColorPickerMessage  of ColorPicker.Action
        | ChangeThickness     of Numeric.Action
        | TextInputMessage    of TextInput.Action
        | SetLevel            of int
//        | SetGeometry         of GeometryType
        | SetSemanticType     of SemanticType
        | Save
        | Cancel

    ////// UPDATE
    let update (model : Semantic) (a : Action) = 
        match a with
            | TextInputMessage m -> 
                {model with label = TextInput.update model.label m}
            | ColorPickerMessage m -> 
                {model with 
                  style = {model.style with 
                            color = (ColorPicker.update model.style.color m)
                          }
                }
            | ChangeThickness m -> 
                {model with 
                  style = {model.style with 
                            thickness = Numeric.update model.style.thickness m
                          }
                }
            | SetState state -> 
                {model with state = state}
            | SetLevel i ->
                {model with level = i}
            | SetSemanticType st ->
                {model with semanticType = st}
            | _ -> model

    ////// HELPER FUNCTIONS
    let intoTd (x) = 
      td [clazz "center aligned"; style lrPadding] [x]
      

    ////// VIEW
    let viewNew (model : MSemantic) =
      let thNode = Numeric.view'' 
                     NumericInputType.InputBox 
                     model.style.thickness
                     (AttributeMap.ofList 
                        [style "margin:auto; color:black; max-width:60px"])

      let labelNode = 
        (TextInput.view'' 
          "box-shadow: 0px 0px 0px 1px rgba(0, 0, 0, 0.1) inset"
          model.label)
          
        
      [
        labelNode
          |> intoTd
          |> UI.map Action.TextInputMessage
        thNode 
          |> UI.map ChangeThickness
          |> intoTd
        ColorPicker.view model.style.color 
          |> UI.map ColorPickerMessage
          |> intoTd
        Html.SemUi.dropDown' (AList.ofList levels) model.level SetLevel (fun x -> sprintf "%i" x)
          |> intoTd
//        Html.SemUi.dropDown model.geometry SetGeometry
//          |> intoTd
        Html.SemUi.dropDown model.semanticType SetSemanticType
          |> intoTd

      ]


    let viewEdit (model : MSemantic) =
      let thNode = Numeric.view'' 
                     NumericInputType.InputBox 
                     model.style.thickness
                     (AttributeMap.ofList 
                        [style "margin:auto; color:black; max-width:60px"])

      let labelNode = 
        (TextInput.view'' 
          "box-shadow: 0px 0px 0px 1px rgba(0, 0, 0, 0.1) inset"
          model.label)
          

      let domNodeSemanticType =  
        intoTd <|
          label [clazz "ui horizontal label"]
                [Incremental.text (Mod.map(fun x -> x.ToString()) model.semanticType)]
      [
        labelNode
          |> intoTd
          |> UI.map Action.TextInputMessage
        thNode 
          |> UI.map ChangeThickness
          |> intoTd
        ColorPicker.view model.style.color 
          |> UI.map ColorPickerMessage
          |> intoTd
        Html.SemUi.dropDown' (AList.ofList levels) model.level SetLevel (fun x -> sprintf "%i" x)
          |> intoTd
//        domNodeGeometryType
        domNodeSemanticType
      ]
       
    let viewDisplay (s : MSemantic) = 
      let domNodeLbl =
        intoTd <| Incremental.text (s.label.text)
          //Incremental.label 
            //(AttributeMap.union 
              // (AttributeMap.ofList [clazz "ui horizontal label"]) 
               //(AttributeMap.ofAMap (incrBgColorAMap s.style.color.c)))
           // (AList.ofList [Incremental.text (s.label.text)])
        

      let domNodeThickness = 
        intoTd <|
          //label [clazz "ui horizontal label"] [
            Incremental.text (Mod.map(fun x -> sprintf "%.1f" x) s.style.thickness.value)
         // ]
        

      let domNodeColor = 
        let iconAttr =
          amap {
            yield clazz "circle icon"
            let! c = s.style.color.c
            yield style (sprintf "color:%s" (colorToHexStr c))
          }  
        intoTd <|
          div[] [
            Incremental.i (AttributeMap.ofAMap iconAttr) (AList.ofList [])
            //Incremental.text (Mod.map(fun (x : C4b) -> colorToHexStr x) s.style.color.c)
          ]
           

      let domNodeLevel = 
        intoTd <| 
                Incremental.text (Mod.map(fun x -> sprintf "%i" x) s.level)
            


      let domNodeSemanticType =  
        intoTd <|
                Incremental.text (Mod.map(fun x -> x.ToString()) s.semanticType)
           
                 
      [domNodeLbl;
        domNodeThickness;
        domNodeColor;
        domNodeLevel;
        domNodeSemanticType]
      


    let view (model : MSemantic) : IMod<list<DomNode<Action>>> =
        Mod.map (fun d -> 
          match d with
            | SemanticState.Display  -> viewDisplay model
            | SemanticState.Edit -> viewEdit model
            | SemanticState.New -> viewNew model) model.state
