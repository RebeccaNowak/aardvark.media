namespace CorrelationDrawing

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Incremental.Operators
open Aardvark.Base.Rendering
open Aardvark.UI
open UtilitiesGUI


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Annotation =

  let initial (id : string) = 
      {     
          id = id

          geometry = GeometryType.Line
          semanticId = ""
          points = plist.Empty
          segments = plist.Empty
          projection = Projection.Viewpoint
          visible = true
          text = "text"
          overrideStyle = None
          overrideLevel = None
          overrideGeometryType = None
      }

  type Action =
      | SetSemantic of option<string>

  let update (anno : Annotation) (a : Action) =
      match a with
          | SetSemantic str -> match str with
                                | Some s -> {anno with semanticId = s}
                                | None -> anno

  let view (model : MAnnotation)  (semanticApp : MSemanticApp) = 
    let semanticsNode = 
      let iconAttr =
        amap {
          yield clazz "circle icon"
          let! c = SemanticApp.getColor semanticApp model.semanticId
          yield style (sprintf "color:%s" (colorToHexStr c))
//          yield attribute "color" "blue"
        }      
      td [clazz "center aligned"; style lrPadding] 
         [
          Incremental.i (AttributeMap.ofAMap iconAttr) (AList.ofList []);
          //label  [clazz "ui label"] 
                 Incremental.text (SemanticApp.getLabel semanticApp model.semanticId)]

        
    let geometryTypeNode =
      td [clazz "center aligned"; style lrPadding] 
         //[label  [clazz "ui label"] 
                 [text (model.geometry.ToString())]

    let projectionNode =
      td [clazz "center aligned"; style lrPadding] 
         //[label  [clazz "ui label"] 
                 [text (model.projection.ToString())]

    let annotationTextNode = 
        td [clazz "center aligned"; style lrPadding] 
           //[label  [clazz "ui label"] 
                   [Incremental.text model.text]
    
    [semanticsNode;geometryTypeNode;projectionNode;annotationTextNode]
    
        
  ///////// HELPER FUNCTIONS
  let getColor (anno : IMod<Option<MAnnotation>>) (semanticApp : MSemanticApp) = 
    anno |> 
      Mod.bind (fun (a : Option<MAnnotation>) -> 
        match a with
          | Some a -> SemanticApp.getColor semanticApp a.semanticId
          | None -> Mod.constant C4b.Red)


//  let getColor' (anno : MAnnotation) (semanticApp : MSemanticApp) = 
//    let sem = Mod.bind (fun id -> AMap.tryFind id cdModel.semantics) anno.semanticId
//    Mod.bind (fun (se : option<MSemantic>) ->
//      match se with
//                  | Some s -> s.style.color.c
//                  | None -> Mod.constant C4b.Red) sem

  
    

  let getThickness (anno : IMod<Option<MAnnotation>>) (semanticApp : MSemanticApp) = 
    Mod.bind (fun (a : Option<MAnnotation>)
                  -> match a with
                      | Some a -> SemanticApp.getThickness semanticApp a.semanticId
                      | None -> Mod.constant Semantic.ThicknessDefault) anno   


//  let getThickness' (anno : MAnnotation) (cdModel : MCorrelationDrawingModel) = 
//    let sem = Mod.bind (fun id -> AMap.tryFind id cdModel.semantics) anno.semanticId
//    Mod.bind (fun (se : option<MSemantic>) ->
//      match se with
//                  | Some s -> s.style.thickness.value
//                  | None -> Mod.constant Semantic.ThicknessDefault) sem      
                                               
        

