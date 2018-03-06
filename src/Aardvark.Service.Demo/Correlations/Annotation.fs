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
    let thickness = [1.0; 2.0; 3.0; 4.0; 5.0;]
//    //let color = [new C4b(241,238,246); new C4b(189,201,225); new C4b(116,169,207); new C4b(43,140,190); new C4b(4,90,141); new C4b(241,163,64); new C4b(153,142,195) ]
//
//    let thickn = {
//        value   = 3.0
//        min     = 1.0
//        max     = 8.0
//        step    = 1.0
//        format  = "{0:0}"
//    }
    
    let initial  = 
        {     
            geometry = GeometryType.Line
            semanticId = ""
            points = plist.Empty
            segments = plist.Empty //[]
            projection = Projection.Viewpoint
            visible = true
            text = "text"
        }

    type Action =
        | ChangeSemantic

    let update (anno : Annotation) (a : Action) =
        match a with
            | ChangeSemantic -> anno

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




//    let color (anno : Annotation) =
//        (Annotation.Lens.semantic |. Semantic.Lens.style |. Style.Lens.color |. ColorInput.Lens.c).Get anno


    let view (model : MCorrelationDrawingModel) (mAnno : MAnnotation) = 
        let getHtmlColor (ma : MAnnotation) =
            adaptive {
                let! colorMod = (getColorMod mAnno model)
                return  ColorPicker.colorToHex colorMod
            }
            //adaptive {
            //    let! bgc = mAnno.semanticId.style.color.c //mAnno.semantic.style.color.c
            //    return ColorPicker.colorToHex bgc // |> UI.map Semantic.Action
           // }

        //let iconNode = 
            //adaptive {
            // works: attribute "style" "background: blue"
                // (style (sprintf "background: %s" (col)))
                // attribute "style" (sprintf "background: %s" (getHtmlColor mAnno))
                //div [clazz "item"; style "color: blue"] [label [clazz "ui label"][clazz ]]
         //       div [clazz "item"; style "color: blue"] [i [clazz "medium File Outline middle aligned icon"][]]
            //}
        
        let buttonTextNode =
            adaptive {
                let! lbl = getSemanticLblMod mAnno model
                let! col = (getHtmlColor mAnno)
                return div [clazz "item"] [label [clazz "ui label"; style (sprintf "background: #%s" col); onMouseClick (fun _ -> ChangeSemantic)] [text lbl]]
                // return div [clazz "item"] [button [clazz "ui button"; onMouseClick (fun _ -> ChangeSemantic)] [text lbl]]
            }

        Incremental.div AttributeMap.empty (
            alist {
                let! col = getHtmlColor mAnno
                //let! icon = iconNode
                let! button = buttonTextNode
                yield div [clazz "ui horizontal list"] [button]
            }
        )
       
        
        
                                               
        

