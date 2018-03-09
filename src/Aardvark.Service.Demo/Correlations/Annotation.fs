namespace CorrelationDrawing

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Incremental.Operators
open Aardvark.Base.Rendering
open Aardvark.UI
open CorrelationUtilities


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Annotation =
  
  let initial id = 
      {     
          id = id

          geometry = GeometryType.Line
          semanticId = ""
          points = plist.Empty
          segments = plist.Empty //[]
          projection = Projection.Viewpoint
          visible = true
          text = "text"
      }

  type Action =
      | SetSemantic //of string

  let update (anno : Annotation) (a : Action) =
      match a with
          //| SetSemantic s -> anno
          | SetSemantic -> anno

  // TODO make generic function for fs below

  let getSemanticLblMod (anno : MAnnotation) (model : MCorrelationDrawingModel) = 
      adaptive {
          let! id = anno.semanticId
          let! semantic = AMap.tryFind id model.semantics
          match semantic with
              | Some s -> 
                  let! lbl = s.label.text
                  return lbl
              | None -> return "-None-"
      }

  let getSemanticLblMod' (anno : IMod<Option<MAnnotation>>) (model : MCorrelationDrawingModel) = 
      adaptive {
          let! a = anno
          match a with
              | Some b -> 
                  let! lbl = getSemanticLblMod b model
                  return lbl
              | None -> return "-None-"
  }

  let getColorMod (anno : MAnnotation) (model : MCorrelationDrawingModel) = 
      adaptive {
          let! id = anno.semanticId
          let! semantic = AMap.tryFind id model.semantics
          match semantic with
              | Some s -> 
                  let! col = s.style.color.c
                  return col
              | None -> return C4b.Blue
      }

  let getColorMod' (anno : IMod<Option<MAnnotation>>) (model : MCorrelationDrawingModel) = 
      adaptive {
          let! a = anno
          match a with
              | Some b -> 
                  let! col = getColorMod b model
                  return col
              | None -> return C4b.Cyan
  }

  let getThicknessMod (anno : MAnnotation) (model : MCorrelationDrawingModel) = 
      adaptive {
          let! id = anno.semanticId
          let! semantic = AMap.tryFind id model.semantics
          match semantic with
              | Some s -> 
                  let! th = s.style.thickness.value
                  return th
              | None -> return 1.0
      }

  let getThicknessMod' (anno : IMod<Option<MAnnotation>>) (model : MCorrelationDrawingModel) = 
      adaptive {
          let! a = anno
          match a with
              | Some b ->
                  let! th = getThicknessMod b model
                  return th
              | None -> return 1.0
      }




//  let color (anno : Annotation) =
//      (Annotation.Lens.semantic |. Semantic.Lens.style |. Style.Lens.color |. ColorInput.Lens.c).Get anno


  let view (model : MCorrelationDrawingModel) (mAnno : MAnnotation) = 
    let getHtmlColor (ma : MAnnotation) =
        adaptive {
            let! colorMod = (getColorMod mAnno model)
            return  ColorPicker.colorToHex colorMod
        }
    

    let semanticButtonTextNode =
        adaptive {
            let! lbl = getSemanticLblMod mAnno model
            let! col = (getHtmlColor mAnno)
            return div [clazz "item"] [label [clazz "ui label"; style (sprintf "background: #%s" col); onMouseClick (fun _ -> SetSemantic)] [text lbl]]
            // return div [clazz "item"] [button [clazz "ui button"; onMouseClick (fun _ -> ChangeSemantic)] [text lbl]]
        }

    let geometryTypeNode =
      div [clazz "item"] [label  [clazz "ui label"] [text (mAnno.geometry.ToString())]]
    //  adaptive {
    //    let! geo = mAnno.geometry
    //    return div [clazz "item"] [label  [clazz "ui label"] [text (geo.ToString())]]
    //  }

    let projectionNode =
      div [clazz "item"] [label  [clazz "ui label"] [text (mAnno.projection.ToString())]]

    let annotationTextNode = 
      adaptive {
        let! txt = mAnno.text
        return div [clazz "item"] [label  [clazz "ui label"] [text txt]]
      }

    Incremental.div AttributeMap.empty (
        alist {
            let! col = getHtmlColor mAnno
            //let! icon = iconNode
            let! button = semanticButtonTextNode
            let! annoText = annotationTextNode
            //let! geoNode = geometryTypeNode
            yield div [clazz "ui horizontal list"] [button; 
                                                    geometryTypeNode; 
                                                    projectionNode
                                                    annoText]
        }
    )

  //let viewEnabled (model : MCorrelationDrawingModel) (mAnno : MAnnotation) =
    //alist {
      //let! labelNode = createDomNodeLabel
      //yield labelNode
      //yield td [clazz "center aligned"; style lrPadding] [(thNode |> UI.map ChangeThickness)]
      //yield td [clazz "center aligned"; style lrPadding] [ColorPicker.view s.style.color |> UI.map ColorPickerMessage]
    //}




       
        
        
                                               
        

